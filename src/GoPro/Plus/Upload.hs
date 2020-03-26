module GoPro.Plus.Upload where

import           Control.Applicative    (liftA3)
import           Control.Lens
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson             as J
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BL
import           Data.Char              (toUpper)
import qualified Data.Text              as T
import           Data.Time.Clock.POSIX  (getCurrentTime)
import qualified Data.Vector            as V
import           Network.Wreq           (header, params, putWith)
import           System.FilePath.Posix  (takeExtension, takeFileName)
import           System.IO              (Handle, IOMode (..), SeekMode (..),
                                         hSeek, hTell, withFile)

import           GoPro.HTTP

uploadFile :: MonadIO m => Token -> String -> FilePath -> m ()
uploadFile tok uid fp = liftIO $ withFile fp ReadMode $ \fh -> do
  hSeek fh SeekFromEnd 0
  fileSize <- hTell fh
  hSeek fh AbsoluteSeek 0

  let fn = takeFileName fp
      ext = T.pack . fmap toUpper . drop 1 . takeExtension $ fn
      m1 = J.Object (mempty & at "file_extension" ?~ J.String ext
                     & at "filename" ?~ J.String (T.pack fn)
                     & at "type" ?~ J.String (fileType ext)
                     & at "on_public_profile" ?~ J.Bool False
                     & at "content_title" ?~ J.String (T.pack fn)
                     & at "content_source" ?~ J.String "web_media_library"
                     & at "access_token" ?~ J.String (T.pack tok)
                     & at "gopro_user_id" ?~ J.String (T.pack uid))
  cr <- jpost "https://api.gopro.com/media" m1

  let Just mid = cr ^? key "id" . _String
      d1 = J.Object (mempty & at "medium_id" ?~ J.String mid
                     & at "file_extension" ?~ J.String ext
                     & at "type" ?~ J.String "Source"
                     & at "label" ?~ J.String "Source"
                     & at "available" ?~ J.Bool False
                     & at "item_count" ?~ J.Number 1
                     & at "camera_positions" ?~ J.String "default"
                     & at "on_public_profile" ?~ J.Bool False
                     & at "access_token" ?~ J.String (T.pack tok)
                     & at "gopro_user_id" ?~ J.String (T.pack uid))
  dr <- jpost "https://api.gopro.com/derivatives" d1

  let Just did = dr ^? key "id" . _String
      u1 = J.Object (mempty & at "derivative_id" ?~ J.String did
                     & at "camera_position" ?~ J.String "default"
                     & at "item_number" ?~ J.Number 1
                     & at "access_token" ?~ J.String (T.pack tok)
                     & at "gopro_user_id" ?~ J.String (T.pack uid))
  ur <- jpost "https://api.gopro.com/user-uploads" u1

  let Just upid = ur ^? key "id" . _String
      upopts = authOpts tok & params .~ [("id", upid),
                                         ("page", "1"),
                                         ("per_page", "100"),
                                         ("item_number", "1"),
                                         ("camera_position", "default"),
                                         ("file_size", (T.pack . show) fileSize),
                                         ("part_size", (T.pack . show) chunkSize)]
                            & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]
  upaths <- (jgetWith upopts (T.unpack ("https://api.gopro.com/user-uploads/" <> did))) :: IO J.Value
  let Just ups = upaths ^? key "_embedded" . key "authorizations" . _Array . to V.toList

  mapM_ (uploadOne fh) ups

  let u2 = J.Object (mempty & at "id" ?~ J.String upid
                     & at "item_number" ?~ J.Number 1
                     & at "camera_position" ?~ J.String "default"
                     & at "complete" ?~ J.Bool True
                     & at "derivative_id" ?~ J.String did
                     & at "file_size" ?~ J.String ((T.pack . show) fileSize)
                     & at "part_size" ?~ J.String ((T.pack . show) chunkSize))
  _ <- putWith popts (T.unpack ("https://api.gopro.com/user-uploads/" <> did)) u2

  let d2 = J.Object (mempty & at "available" ?~ J.Bool True
                     & at "access_token" ?~ J.String (T.pack tok)
                     & at "gopro_user_id" ?~ J.String (T.pack uid))

  _ <- putWith popts (T.unpack ("https://api.gopro.com/derivatives/" <> did)) d2

  now <- getCurrentTime
  let done = J.Object (mempty & at "upload_completed_at" ?~ J.toJSON now
                       & at "client_updated_at" ?~ J.toJSON now
                       & at "revision_number" ?~ J.Number 0
                       & at "access_token" ?~ J.String (T.pack tok)
                       & at "gopro_user_id" ?~ J.String (T.pack uid))

  void $ putWith popts (T.unpack ("https://api.gopro.com/media/" <> mid)) done

  where
    chunkSize :: Integer
    chunkSize = 6291456

    fileType "JPG" = "Photo"
    fileType _     = "Video"

    popts = authOpts tok & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]

    jpost :: String -> J.Value -> IO J.Value
    jpost = jpostWith popts

    tInt :: T.Text -> Integer
    tInt = read . T.unpack

    uploadOne :: Handle -> J.Value -> IO ()
    uploadOne fh v = do
      let Just (l, p, u) = liftA3 (,,) (v ^? key "Content-Length" . _String . to tInt)
                                       (v ^? key "part" . _Integer . to toInteger)
                                       (v ^? key "url" . _String . to T.unpack)

          offs = (p - 1) * chunkSize
      hSeek fh AbsoluteSeek offs
      void $ putWith defOpts u =<< BL.hGet fh (fromIntegral l)
