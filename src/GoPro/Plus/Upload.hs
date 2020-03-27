{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module GoPro.Plus.Upload (MediumID, UID, UploadID, DerivativeID,
                          UploadPart(..), uploadLength, uploadPart, uploadURL,
                          Upload(..), uploadID, uploadParts,
                           uploadMedium,
                           createMedium, createDerivative, createUpload,
                           completeUpload, uploadChunk, markAvailable,
                           Uploader, runUpload, resumeUpload,
                           setMediumType,
                           listUploading
                         ) where

import           Control.Applicative    (liftA3)
import           Control.Lens
import           Control.Monad          (void, when)
import           Control.Monad.Catch    (MonadMask (..))
import           Control.Monad.Fail     (MonadFail (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.State    (StateT (..), evalStateT, get, modify)
import           Control.Retry          (RetryStatus (..), exponentialBackoff,
                                         limitRetries, recoverAll)
import qualified Data.Aeson             as J
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy   as BL
import           Data.Char              (toUpper)
import           Data.Maybe             (fromJust)
import qualified Data.Text              as T
import           Data.Time.Clock.POSIX  (getCurrentTime)
import qualified Data.Vector            as V
import           Network.Wreq           (Options, header, params, putWith)
import           Prelude                hiding (fail)
import           System.FilePath.Posix  (takeExtension, takeFileName)
import           System.IO              (IOMode (..), SeekMode (..), hSeek,
                                         withFile)
import           System.Posix.Files     (fileSize, getFileStatus)
import           UnliftIO               (MonadUnliftIO (..))

import           GoPro.HTTP
import           GoPro.Plus.Media       (Media (..), list)

type MediumID = T.Text
type UID = String
type UploadID = T.Text
type DerivativeID = T.Text

type Uploader = StateT Env

-- This is typically a bad idea, but we assume we only mutate state
-- before we'd ever need an unlift.
instance MonadUnliftIO m => MonadUnliftIO (StateT Env m) where
  withRunInIO inner =
    get >>= \st -> StateT $ \_ ->
                              withRunInIO $ \run -> (,st) <$> inner (run . flip evalStateT st)

data Env = Env {
  token      :: Token,
  userID     :: UID,
  fileList   :: [FilePath],
  mediumType :: T.Text,
  extension  :: T.Text,
  filename   :: String,
  mediumID   :: MediumID
  }

-- | List all media in uploading state.
listUploading :: MonadIO m => Token -> m [Media]
listUploading tok = do
  filter (\Media{..} -> _media_ready_to_view == "uploading") . fst <$> list tok 30 1

-- | Run an Uploader monad.
runUpload :: (MonadFail m, MonadIO m) => Token -> UID -> [FilePath] -> Uploader m a -> m a
runUpload token userID fileList a = resumeUpload token userID fileList "" a

-- | Run an Uploader monad for which we already know the MediumID
-- (i.e., we're resuming an upload we previously began).
resumeUpload :: (MonadFail m, MonadIO m) => Token -> UID -> [FilePath] -> MediumID -> Uploader m a -> m a
resumeUpload _ _ [] _ _ = fail "empty file list"
resumeUpload token userID fileList@(fp:_) mediumID a = evalStateT a Env{..}
  where
    extension = T.pack . fmap toUpper . drop 1 . takeExtension $ filename
    filename = takeFileName fp
    mediumType = fileType extension

    fileType "JPG" = "Photo"
    fileType _     = "Video"

-- | Override the detected medium type.
setMediumType :: Monad m => T.Text -> Uploader m ()
setMediumType t = modify (\m -> m{mediumType=t})

jpostVal :: MonadIO m => Options -> String -> J.Value -> m J.Value
jpostVal opts u v = liftIO $ jpostWith opts u v

-- | Create a new medium (e.g., video, photo, etc...).
createMedium :: MonadIO m =>  Uploader m MediumID
createMedium = do
  Env{..} <- get
  let m1 = J.Object (mempty & at "file_extension" ?~ J.String extension
                     & at "filename" ?~ J.String (T.pack filename)
                     & at "type" ?~ J.String mediumType
                     & at "on_public_profile" ?~ J.Bool False
                     & at "content_title" ?~ J.String (T.pack filename)
                     & at "content_source" ?~ J.String "web_media_library"
                     & at "access_token" ?~ J.String (T.pack token)
                     & at "gopro_user_id" ?~ J.String (T.pack userID))
  m <- fromJust . preview (key "id" . _String) <$> jpostVal (authOpts token) "https://api.gopro.com/media" m1
  modify (\s -> s{mediumID=m})
  pure m

-- | Create a new derivative of the current medium containing the given number of parts.
createDerivative :: MonadIO m => Int -> Uploader m DerivativeID
createDerivative nparts = do
  Env{..} <- get
  let d1 = J.Object (mempty & at "medium_id" ?~ J.String mediumID
                     & at "file_extension" ?~ J.String extension
                     & at "type" ?~ J.String "Source"
                     & at "label" ?~ J.String "Source"
                     & at "available" ?~ J.Bool False
                     & at "item_count" ?~ J.Number (fromIntegral nparts)
                     & at "camera_positions" ?~ J.String "default"
                     & at "on_public_profile" ?~ J.Bool False
                     & at "access_token" ?~ J.String (T.pack token)
                     & at "gopro_user_id" ?~ J.String (T.pack userID))
  fromJust . preview (key "id" . _String) <$> jpostVal (authOpts token) "https://api.gopro.com/derivatives" d1

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

-- | Create a new upload for a derivative.
createUpload :: MonadIO m => DerivativeID -> Int -> Int -> Uploader m Upload
createUpload did part fsize = do
  Env{..} <- get
  let u1 = J.Object (mempty & at "derivative_id" ?~ J.String did
                     & at "camera_position" ?~ J.String "default"
                     & at "item_number" ?~ J.Number (fromIntegral part)
                     & at "access_token" ?~ J.String (T.pack token)
                     & at "gopro_user_id" ?~ J.String (T.pack userID))
  ur <- jpost token "https://api.gopro.com/user-uploads" u1

  let Just upid = ur ^? key "id" . _String
      upopts = authOpts token & params .~ [("id", upid),
                                           ("page", "1"),
                                           ("per_page", "100"),
                                           ("item_number", (T.pack . show) part),
                                           ("camera_position", "default"),
                                           ("file_size", (T.pack . show) fsize),
                                           ("part_size", (T.pack . show) chunkSize)]
                              & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]
  upaths <- (jgetWith upopts (T.unpack ("https://api.gopro.com/user-uploads/" <> did)))
  let Just ups = (upaths :: J.Value) ^? key "_embedded" . key "authorizations" . _Array . to V.toList
  pure $ Upload upid (fromJust $ traverse aChunk ups)

  where
    popts tok = authOpts tok & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]
    jpost :: MonadIO m => String ->  String -> J.Value -> m J.Value
    jpost tok = jpostVal (popts tok)

    tInt :: T.Text -> Integer
    tInt = read . T.unpack

    aChunk v = liftA3 UploadPart (v ^? key "Content-Length" . _String . to tInt)
                                 (v ^? key "part" . _Integer . to toInteger)
                                 (v ^? key "url" . _String . to T.unpack)

-- | Upload a chunk of of the given file as specified by this UploadPart.
uploadChunk :: (MonadMask m, MonadIO m) => FilePath -> (Int -> m ()) -> UploadPart -> m ()
uploadChunk fp retrycb UploadPart{..} = recoverAll policy $ \r -> do
  when (rsIterNumber r > 0) $ retrycb (rsIterNumber r)
  liftIO $ withFile fp ReadMode $ \fh -> do
    hSeek fh AbsoluteSeek ((_uploadPart - 1) * chunkSize)
    void $ putWith defOpts _uploadURL =<< BL.hGet fh (fromIntegral _uploadLength)

    where policy = exponentialBackoff 2000000 <> limitRetries 5

-- | Mark the given upload for the given derivative as complete.
completeUpload :: MonadIO m => UploadID -> DerivativeID -> Int -> Integer -> Uploader m ()
completeUpload upid did part fsize = do
  Env{..} <- get
  let u2 = J.Object (mempty & at "id" ?~ J.String upid
                     & at "item_number" ?~ J.Number (fromIntegral part)
                     & at "camera_position" ?~ J.String "default"
                     & at "complete" ?~ J.Bool True
                     & at "derivative_id" ?~ J.String did
                     & at "file_size" ?~ J.String ((T.pack . show) fsize)
                     & at "part_size" ?~ J.String ((T.pack . show) chunkSize))
  void . liftIO $ putWith (popts token) (T.unpack ("https://api.gopro.com/user-uploads/" <> did)) u2

  where
    popts tok = authOpts tok & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]

-- | Mark the given derivative as availble to use.  This also updates
-- the medium record marking it as having completed its upload.
markAvailable :: MonadIO m => DerivativeID -> Uploader m ()
markAvailable did = do
  Env{..} <- get
  let d2 = J.Object (mempty & at "available" ?~ J.Bool True
                     & at "access_token" ?~ J.String (T.pack token)
                     & at "gopro_user_id" ?~ J.String (T.pack userID))

  _ <- liftIO $ putWith (popts token) (T.unpack ("https://api.gopro.com/derivatives/" <> did)) d2

  now <- liftIO $ getCurrentTime
  let done = J.Object (mempty & at "upload_completed_at" ?~ J.toJSON now
                       & at "client_updated_at" ?~ J.toJSON now
                       & at "revision_number" ?~ J.Number 0
                       & at "access_token" ?~ J.String (T.pack token)
                       & at "gopro_user_id" ?~ J.String (T.pack userID))

  void . liftIO $ putWith (popts token) (T.unpack ("https://api.gopro.com/media/" <> mediumID)) done

  where
    popts tok = authOpts tok & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]

-- | Convenience function to upload a single medium.
uploadMedium :: (MonadMask m, MonadFail m, MonadIO m) => Token -> UID -> [FilePath] -> m MediumID
uploadMedium _ _ [] = fail "no files provided"
uploadMedium tok uid fps = runUpload tok uid fps $ do
  mid <- createMedium
  did <- createDerivative (length fps)
  mapM_ (\(fp,n) -> do
            fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
            Upload{..} <- createUpload did n (fromInteger fsize)
            mapM_ (uploadChunk fp (const . pure $ ())) _uploadParts
            completeUpload _uploadID did n fsize
        ) $ zip fps [1..]
  markAvailable did

  pure mid
