{-|
Module      : GoPro.Plus.Upload
Description : Functionality for uploading media to GoPro Plus.
Copyright   : (c) Dustin Sallings, 2020
License     : BSD3
Maintainer  : dustin@spy.net
Stability   : experimental

GoPro Plus media upload client.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module GoPro.Plus.Upload (
  -- * High level upload all-in-one convenience.
  uploadMedium,
  -- * Low-level upload parts.
  runUpload, resumeUpload,
  createMedium, createSource, createDerivative, createUpload,
  completeUpload, getUpload, uploadChunk, markAvailable,
  -- * Data Types
  UploadID, DerivativeID,
  UploadPart(..), uploadLength, uploadPart, uploadURL,
  Upload(..), uploadID, uploadParts,
  -- * Uploader monad.
  Uploader,
  setMediumType, setLogAction,
  -- * For your convenience.
  listUploading
  ) where

import           Control.Applicative          (liftA3)
import           Control.Lens
import           Control.Monad                (void, when)
import           Control.Monad.Catch          (MonadMask (..))
import           Control.Monad.Fail           (MonadFail (..))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.State          (StateT (..), evalStateT, get,
                                               gets, lift, modify)
import           Control.Retry                (RetryStatus (..),
                                               exponentialBackoff, limitRetries,
                                               recoverAll)
import qualified Data.Aeson                   as J
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy         as BL
import           Data.Char                    (toUpper)
import           Data.Maybe                   (fromJust)
import qualified Data.Text                    as T
import           Data.Time.Clock.POSIX        (getCurrentTime)
import qualified Data.Vector                  as V
import           Network.Wreq                 (Options, header, params, putWith)
import           Prelude                      hiding (fail)
import           System.FilePath.Posix        (takeExtension, takeFileName)
import           System.IO                    (IOMode (..), SeekMode (..),
                                               hSeek, withFile)
import           System.Posix.Files           (fileSize, getFileStatus)
import           UnliftIO                     (MonadUnliftIO (..))

import           GoPro.Plus.Auth              (AuthInfo (..), HasGoProAuth (..))
import           GoPro.Plus.Internal.AuthHTTP
import           GoPro.Plus.Internal.HTTP
import           GoPro.Plus.Media             (Medium (..), MediumID,
                                               MediumType (..),
                                               ReadyToViewType (..), list,
                                               putMedium)

type UploadID = T.Text
type DerivativeID = T.Text

-- | GoPro Plus uploader monad.
type Uploader m = StateT (Env m) m

-- This is typically a bad idea, but we assume we only mutate state
-- before we'd ever need an unlift.
instance MonadUnliftIO m => MonadUnliftIO (StateT (Env m) m) where
  withRunInIO inner =
    get >>= \st -> StateT $ \_ ->
                              withRunInIO $ \run -> (,st) <$> inner (run . flip evalStateT st)

instance HasGoProAuth m => HasGoProAuth (Uploader m) where
  goproAuth = lift goproAuth

data Env m = Env {
  fileList   :: [FilePath],
  mediumType :: MediumType,
  extension  :: T.Text,
  filename   :: String,
  mediumID   :: MediumID,
  logAction  :: (MonadMask m, Monad m) => String -> m ()
  }

-- | List all media in uploading state.
listUploading :: (HasGoProAuth m, MonadIO m) => m [Medium]
listUploading = filter (\Medium{..} -> _medium_ready_to_view == ViewUploading) . fst <$> list 30 1

-- | Run an Uploader monad to create a single medium and upload the content for it.
runUpload :: (HasGoProAuth m, MonadFail m, MonadIO m)
          => [FilePath]   -- ^ The list of files to include in the medium.
          -> Uploader m a -- ^ The action to perform.
          -> m a          -- ^ The result of the inner action.
runUpload fileList = resumeUpload fileList ""

-- | Run an Uploader monad for which we already know the MediumID
-- (i.e., we're resuming an upload we previously began).
resumeUpload :: (HasGoProAuth m, MonadFail m, MonadIO m) => [FilePath] -> MediumID -> Uploader m a -> m a
resumeUpload [] _ _ = fail "empty file list"
resumeUpload fileList@(fp:_) mediumID a =
  goproAuth >>= \AuthInfo{..} -> evalStateT a Env{..}
  where
    extension = T.pack . fmap toUpper . drop 1 . takeExtension $ filename
    filename = takeFileName fp
    mediumType = fileType extension
    logAction _ = pure ()

    fileType "JPG" = Photo
    fileType _     = Video

-- | Override the detected medium type.
setMediumType :: Monad m => MediumType -> Uploader m ()
setMediumType t = modify (\m -> m{mediumType=t})

-- | Set the logging action to report retries (or whatever other
-- interesting things might happen).
setLogAction :: (Monad m, MonadMask m) => (String -> m ()) -> Uploader m ()
setLogAction t = modify (\m -> m{logAction=t})

jpostVal :: (HasGoProAuth m, MonadIO m) => Options -> String -> J.Value -> m J.Value
jpostVal opts u v = liftIO $ jpostWith opts u v

jpostAuthVal :: (HasGoProAuth m, MonadIO m) => String -> J.Value -> m J.Value
jpostAuthVal = jpostAuth

-- | Create a new medium (e.g., video, photo, etc...) and return its ID.
createMedium :: (HasGoProAuth m, MonadIO m) => Uploader m MediumID
createMedium = do
  Env{..} <- get
  AuthInfo{..} <- goproAuth
  let m1 = J.Object (mempty & at "file_extension" ?~ J.String extension
                     & at "filename" ?~ J.String (T.pack filename)
                     & at "type" ?~ J.toJSON mediumType
                     & at "on_public_profile" ?~ J.Bool False
                     & at "content_title" ?~ J.String (T.pack filename)
                     & at "content_source" ?~ J.String "web_media_library"
                     & at "access_token" ?~ J.String _access_token
                     & at "gopro_user_id" ?~ J.String _resource_owner_id)
  m <- fromJust . preview (key "id" . _String) <$> jpostAuthVal "https://api.gopro.com/media" m1
  modify (\s -> s{mediumID=m})
  pure m

-- | Convenient action for creating a Source derivative.
createSource :: (HasGoProAuth m, MonadIO m) => Int -> Uploader m DerivativeID
createSource nparts = createDerivative nparts "Source" "Source"

-- | Create a new derivative of the current medium containing the given number of parts.
createDerivative :: (HasGoProAuth m, MonadIO m)
                 => Int     -- ^ The number of parts this derivative contains.
                 -> T.Text  -- ^ The "type" of this derivative.
                 -> T.Text  -- ^ The label of this derivative.
                 -> Uploader m DerivativeID
createDerivative nparts typ lbl = do
  Env{..} <- get
  AuthInfo{..} <- goproAuth
  let d1 = J.Object (mempty & at "medium_id" ?~ J.String mediumID
                     & at "file_extension" ?~ J.String extension
                     & at "type" ?~ J.String  typ
                     & at "label" ?~ J.String lbl
                     & at "available" ?~ J.Bool False
                     & at "item_count" ?~ J.Number (fromIntegral nparts)
                     & at "camera_positions" ?~ J.String "default"
                     & at "on_public_profile" ?~ J.Bool False
                     & at "access_token" ?~ J.String _access_token
                     & at "gopro_user_id" ?~ J.String _resource_owner_id)
  fromJust . preview (key "id" . _String) <$> jpostAuthVal "https://api.gopro.com/derivatives" d1

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
createUpload :: (HasGoProAuth m, MonadIO m)
             => DerivativeID -- ^ The derivative into which we're uploading.
             -> Int          -- ^ The part number (1-based) being uploaded.
             -> Int          -- ^ The size of the file being uploaded in this part.
             -> Uploader m Upload
createUpload did part fsize = do
  Env{..} <- get
  AuthInfo{..} <- goproAuth
  let u1 = J.Object (mempty & at "derivative_id" ?~ J.String did
                     & at "camera_position" ?~ J.String "default"
                     & at "item_number" ?~ J.Number (fromIntegral part)
                     & at "access_token" ?~ J.String _access_token
                     & at "gopro_user_id" ?~ J.String _resource_owner_id)
  ur <- jpost "https://api.gopro.com/user-uploads" u1
  let Just upid = ur ^? key "id" . _String
  getUpload upid did part fsize

  where
    popts tok = authOpts tok & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]
    jpost :: (HasGoProAuth m, MonadIO m) => String -> J.Value -> m J.Value
    jpost u p = (_access_token <$> goproAuth) >>= \tok -> jpostVal (popts tok) u p

getUpload :: (HasGoProAuth m, MonadIO m)
          => UploadID
          -> DerivativeID
          -> Int
          -> Int
          -> Uploader m Upload
getUpload upid did part fsize = do
  Env{..} <- get
  AuthInfo{..} <- goproAuth

  let pages = (ceiling ((fromIntegral fsize :: Double) / fromIntegral chunkSize)) :: Int
      upopts = authOpts _access_token & params .~ [("id", upid),
                                                   ("page", "1"),
                                                   ("per_page", (T.pack . show) pages),
                                                   ("item_number", (T.pack . show) part),
                                                   ("camera_position", "default"),
                                                   ("file_size", (T.pack . show) fsize),
                                                   ("part_size", (T.pack . show) chunkSize)]
               & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]
  upaths <- jgetWith upopts (T.unpack ("https://api.gopro.com/user-uploads/" <> did))
  let Just ups = (upaths :: J.Value) ^? key "_embedded" . key "authorizations" . _Array . to V.toList
  pure $ Upload upid (fromJust $ traverse aChunk ups)

  where
    tInt :: T.Text -> Integer
    tInt = read . T.unpack
    aChunk v = liftA3 UploadPart (v ^? key "Content-Length" . _String . to tInt)
                                 (v ^? key "part" . _Integer . to toInteger)
                                 (v ^? key "url" . _String . to T.unpack)

-- | Upload a chunk of of the given file as specified by this UploadPart.
uploadChunk :: (MonadMask m, MonadIO m)
            => FilePath    -- ^ The path being uploaded.
            -> UploadPart  -- ^ The UploadPart describing the chunk of upload being transferred
            -> Uploader m ()
uploadChunk fp UploadPart{..} = recoverAll policy $ \r -> do
  when (rsIterNumber r > 0) $ gets logAction >>= \f -> lift (f (retryMsg (rsIterNumber r)))
  liftIO $ withFile fp ReadMode $ \fh -> do
    hSeek fh AbsoluteSeek ((_uploadPart - 1) * chunkSize)
    void $ putWith defOpts _uploadURL =<< BL.hGet fh (fromIntegral _uploadLength)

    where policy = exponentialBackoff 2000000 <> limitRetries 9
          retryMsg a = mconcat ["Retrying upload of ", show fp,
                                " part ", show _uploadPart, " attempt ", show a]

-- | Mark the given upload for the given derivative as complete.
completeUpload :: (HasGoProAuth m, MonadIO m)
               => UploadID     -- ^ The upload ID.
               -> DerivativeID -- ^ The derivative ID.
               -> Int          -- ^ The part number within the derivative.
               -> Integer      -- ^ The size of the file that was uploaded.
               -> Uploader m ()
completeUpload upid did part fsize = do
  Env{..} <- get
  AuthInfo{..} <- goproAuth
  let u2 = J.Object (mempty & at "id" ?~ J.String upid
                     & at "item_number" ?~ J.Number (fromIntegral part)
                     & at "camera_position" ?~ J.String "default"
                     & at "complete" ?~ J.Bool True
                     & at "derivative_id" ?~ J.String did
                     & at "file_size" ?~ J.String ((T.pack . show) fsize)
                     & at "part_size" ?~ J.String ((T.pack . show) chunkSize))
  void . liftIO $ putWith (popts _access_token) (T.unpack ("https://api.gopro.com/user-uploads/" <> did)) u2

  where
    popts tok = authOpts tok & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]

-- | Mark the given derivative as availble to use.  This also updates
-- the medium record marking it as having completed its upload.
markAvailable :: (HasGoProAuth m, MonadIO m) => DerivativeID -> Uploader m ()
markAvailable did = do
  Env{..} <- get
  AuthInfo{..} <- goproAuth
  let d2 = J.Object (mempty & at "available" ?~ J.Bool True
                     & at "access_token" ?~ J.String _access_token
                     & at "gopro_user_id" ?~ J.String _resource_owner_id)

  _ <- liftIO $ putWith (popts _access_token) (T.unpack ("https://api.gopro.com/derivatives/" <> did)) d2

  now <- liftIO getCurrentTime
  let done = J.Object (mempty & at "upload_completed_at" ?~ J.toJSON now
                       & at "client_updated_at" ?~ J.toJSON now
                       & at "revision_number" ?~ J.Number 0
                       & at "access_token" ?~ J.String _access_token
                       & at "gopro_user_id" ?~ J.String _resource_owner_id)

  putMedium mediumID done

  where
    popts tok = authOpts tok & header "Accept" .~  ["application/vnd.gopro.jk.user-uploads+json; version=2.0.0"]

-- | Convenience action to upload a single medium.
uploadMedium :: (HasGoProAuth m, MonadMask m, MonadFail m, MonadIO m)
             => [FilePath] -- ^ Parts of a single medium to upload (e.g., a video file).
             -> m MediumID
uploadMedium [] = fail "no files provided"
uploadMedium fps = runUpload fps $ do
  mid <- createMedium
  did <- createSource (length fps)
  mapM_ (\(fp,n) -> do
            fsize <- toInteger . fileSize <$> (liftIO . getFileStatus) fp
            Upload{..} <- createUpload did n (fromInteger fsize)
            mapM_ (uploadChunk fp) _uploadParts
            completeUpload _uploadID did n fsize
        ) $ zip fps [1..]
  markAvailable did

  pure mid
