{-# LANGUAGE TemplateHaskell #-}

module GoPro.Plus.Upload (MediumID, UID, UploadID, DerivativeID,
                          UploadPart(..), uploadLength, uploadPart, uploadURL,
                          Upload(..), uploadID, uploadParts,
                           uploadFile,
                           createMedium, createDerivative, createUpload,
                           completeUpload, uploadChunk
                         ) where

import           Control.Applicative    (liftA3)
import           Control.Lens
import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson             as J
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BL
import           Data.Char              (toUpper)
import           Data.Maybe             (fromJust)
import qualified Data.Text              as T
import           Data.Time.Clock.POSIX  (getCurrentTime)
import qualified Data.Vector            as V
import           Network.Wreq           (Options, header, params, putWith)
import           System.FilePath.Posix  (takeExtension, takeFileName)
import           System.IO              (IOMode (..), SeekMode (..), hSeek,
                                         withFile)
import           System.Posix.Files     (fileSize, getFileStatus)

import           GoPro.HTTP

type MediumID = T.Text
type UID = String
type UploadID = T.Text
type DerivativeID = T.Text

-- filename, file extension, file type
fileInfo :: FilePath -> (String, String, String)
fileInfo fp = (fn, ext, fileType ext)
  where ext = fmap toUpper . drop 1 . takeExtension $ fn
        fn = takeFileName fp

        fileType "JPG" = "Photo"
        fileType _     = "Video"


jpostVal :: MonadIO m => Options -> String -> J.Value -> m J.Value
jpostVal opts u v = liftIO $ jpostWith opts u v

createMedium :: MonadIO m => Token -> UID -> FilePath -> m MediumID
createMedium tok uid fp = do
  let (fn, ext, fileType) = fileInfo fp
      m1 = J.Object (mempty & at "file_extension" ?~ J.String (T.pack ext)
                     & at "filename" ?~ J.String (T.pack fn)
                     & at "type" ?~ J.String (T.pack fileType)
                     & at "on_public_profile" ?~ J.Bool False
                     & at "content_title" ?~ J.String (T.pack fn)
                     & at "content_source" ?~ J.String "web_media_library"
                     & at "access_token" ?~ J.String (T.pack tok)
                     & at "gopro_user_id" ?~ J.String (T.pack uid))
  fromJust . preview (key "id" . _String) <$> jpostVal (authOpts tok) "https://api.gopro.com/media" m1

createDerivative :: MonadIO m => Token -> UID -> MediumID -> FilePath -> m DerivativeID
createDerivative tok uid mid fp = do
  let (_, ext, _) = fileInfo fp
      d1 = J.Object (mempty & at "medium_id" ?~ J.String mid
                     & at "file_extension" ?~ J.String (T.pack ext)
                     & at "type" ?~ J.String "Source"
                     & at "label" ?~ J.String "Source"
                     & at "available" ?~ J.Bool False
                     & at "item_count" ?~ J.Number 1
                     & at "camera_positions" ?~ J.String "default"
                     & at "on_public_profile" ?~ J.Bool False
                     & at "access_token" ?~ J.String (T.pack tok)
                     & at "gopro_user_id" ?~ J.String (T.pack uid))
  fromJust . preview (key "id" . _String) <$> jpostVal (authOpts tok) "https://api.gopro.com/derivatives" d1

data UploadPart = UploadPart {
  _uploadLength :: Integer,
  _uploadPart   :: Integer,
  _uploadURL    :: String
  } deriving Show

makeLenses ''UploadPart

data Upload = Upload {
  _uploadID    :: UploadID,
  _uploadParts :: [UploadPart]
  } deriving Show

makeLenses ''Upload

chunkSize :: Integer
chunkSize = 6291456

createUpload :: MonadIO m => Token -> UID -> DerivativeID -> Int -> m Upload
createUpload tok uid did fsize = do
  let u1 = J.Object (mempty & at "derivative_id" ?~ J.String did
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
                                         ("file_size", (T.pack . show) fsize),
                                         ("part_size", (T.pack . show) chunkSize)]
                            & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]
  upaths <- (jgetWith upopts (T.unpack ("https://api.gopro.com/user-uploads/" <> did)))
  let Just ups = (upaths :: J.Value) ^? key "_embedded" . key "authorizations" . _Array . to V.toList
  pure $ Upload upid (fromJust $ traverse aChunk ups)

  where
    popts = authOpts tok & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]
    jpost :: MonadIO m => String -> J.Value -> m J.Value
    jpost = jpostVal popts

    tInt :: T.Text -> Integer
    tInt = read . T.unpack

    aChunk v = liftA3 UploadPart (v ^? key "Content-Length" . _String . to tInt)
                                 (v ^? key "part" . _Integer . to toInteger)
                                 (v ^? key "url" . _String . to T.unpack)

uploadChunk :: MonadIO m => FilePath -> UploadPart -> m ()
uploadChunk fp UploadPart{..} = liftIO $ withFile fp ReadMode $ \fh -> do
  hSeek fh AbsoluteSeek ((_uploadPart - 1) * chunkSize)
  void $ putWith defOpts _uploadURL =<< BL.hGet fh (fromIntegral _uploadLength)

completeUpload :: MonadIO m => Token -> UID -> UploadID -> DerivativeID -> Integer -> MediumID -> m ()
completeUpload tok uid upid did fsize mid = do

  let u2 = J.Object (mempty & at "id" ?~ J.String upid
                     & at "item_number" ?~ J.Number 1
                     & at "camera_position" ?~ J.String "default"
                     & at "complete" ?~ J.Bool True
                     & at "derivative_id" ?~ J.String did
                     & at "file_size" ?~ J.String ((T.pack . show) fsize)
                     & at "part_size" ?~ J.String ((T.pack . show) chunkSize))
  _ <- liftIO $ putWith popts (T.unpack ("https://api.gopro.com/user-uploads/" <> did)) u2

  let d2 = J.Object (mempty & at "available" ?~ J.Bool True
                     & at "access_token" ?~ J.String (T.pack tok)
                     & at "gopro_user_id" ?~ J.String (T.pack uid))

  _ <- liftIO $ putWith popts (T.unpack ("https://api.gopro.com/derivatives/" <> did)) d2

  now <- liftIO $ getCurrentTime
  let done = J.Object (mempty & at "upload_completed_at" ?~ J.toJSON now
                       & at "client_updated_at" ?~ J.toJSON now
                       & at "revision_number" ?~ J.Number 0
                       & at "access_token" ?~ J.String (T.pack tok)
                       & at "gopro_user_id" ?~ J.String (T.pack uid))

  void . liftIO $ putWith popts (T.unpack ("https://api.gopro.com/media/" <> mid)) done

  where
    popts = authOpts tok & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]

uploadFile :: MonadIO m => Token -> UID -> FilePath -> m MediumID
uploadFile tok uid fp = do
  fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp

  mid <- createMedium tok uid fp
  did <- createDerivative tok uid mid fp
  Upload{..} <- createUpload tok uid did (fromInteger fsize)
  mapM_ (uploadChunk fp) _uploadParts
  completeUpload tok uid _uploadID did fsize mid

  pure mid
