{-|
Module      : GoPro.Plus.Media
Description : Functionality for managing media within GoPro Plus.
Copyright   : (c) Dustin Sallings, 2020
License     : BSD3
Maintainer  : dustin@spy.net
Stability   : experimental

GoPro Plus media client.
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module GoPro.Plus.Media (
  -- * Accessing Data
  list, listAll, listWhile, medium,
  notReady,
  retrieve, delete,
  fetchThumbnail,
  -- * Data Types
  PageInfo(..), current_page, per_page, total_items, total_pages,
  MediumID, MediumType(..), ReadyToViewType(..),
  Medium(..), medium_id, medium_camera_model, medium_captured_at,
  medium_created_at, medium_file_size, medium_moments_count,
  medium_ready_to_view, medium_source_duration, medium_type,
  medium_token, medium_width, medium_height, medium_filename,
  Listing(..), media, pages,
  HasMediaURL(..), HasMediaHead(..), HasMediaLabel(..), HasMediaType(..), HasMediaItemNumber(..),
  File(..), file_camera_position, file_height, file_width,
  file_item_number, file_orientation, file_head, file_url, file_transforms,
  Variation(..), var_height, var_width, var_label, var_quality,
  var_type, var_transforms, var_head, var_url, var_item_number,
  SpriteFrame(..), frame_count, frame_height, frame_width,
  Sprite(..), sprite_fps, sprite_frame, sprite_height, sprite_width,
  sprite_type, sprite_heads, sprite_urls,
  SidecarFile(..), sidecar_fps, sidecar_label, sidecar_type, sidecar_head, sidecar_url, sidecar_item_number,
  FileStuff(..), files, variations, sprites, sidecar_files,
  FileInfo(..), fileStuff, filename,
  Error(..), error_reason, error_code, error_description, error_id,
  Moment(..), moment_id, moment_time, moments,
  -- * Low-level Junk
  updateMedium, putMedium
  ) where

import           Control.Lens                 hiding ((.=))
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Aeson                   (FromJSON (..), Options (..), ToJSON (..), Value (..), defaultOptions,
                                               fieldLabelModifier, genericParseJSON, genericToEncoding, genericToJSON,
                                               object, (.:), (.=))
import qualified Data.Aeson                   as J
import           Data.Aeson.Types             (typeMismatch)
import qualified Data.ByteString.Lazy         as BL
import           Data.Char                    (toLower)
import qualified Data.Map.Strict              as Map
import qualified Data.Text                    as T
import           Data.Time.Clock              (UTCTime)
import qualified Data.Vector                  as V
import           Generics.Deriving.Base       (Generic)
import           Network.Wreq                 (asJSON, deleteWith, responseBody)
import           Network.Wreq.Types           (Putable)
import           System.Random                (getStdRandom, randomR)
import           Text.Read                    (readMaybe)

import           GoPro.Plus.Auth
import           GoPro.Plus.Internal.AuthHTTP
import           GoPro.Plus.Internal.HTTP

-- | Pagination info returned from lists.
data PageInfo = PageInfo
    { _current_page :: Int
    , _per_page     :: Int
    , _total_items  :: Int
    , _total_pages  :: Int
    }
    deriving (Generic, Show, Eq)

makeLenses ''PageInfo

dropPrefix :: String -> (String -> String)
dropPrefix s = drop (length s)

instance FromJSON PageInfo where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON PageInfo where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = dropPrefix "_" }
  toJSON = genericToJSON jsonOpts{ fieldLabelModifier = dropPrefix "_" }

-- | GoPro-assigned identifier for an uploaded item.
type MediumID = T.Text

-- | Type of Media for a given item.
data MediumType = Photo -- ^ a still photo
    | Video -- ^ normal video
    | TimeLapse -- ^ a timelapse series of photos
    | TimeLapseVideo -- ^ a timelapse video
    | Burst -- ^ a set of photos taken in a burst
    | Chaptered
    | Livestream
    | Looped
    | LoopedVideo
    | BurstVideo
    deriving (Bounded, Enum, Show, Read, Eq)

instance ToJSON MediumType where
  toJSON = J.String . T.pack . show

instance FromJSON MediumType where
  parseJSON (J.String x) = maybe (fail ("Unexpected MediumType: " <> show x)) pure . readMaybe . T.unpack $ x
  parseJSON invalid      = typeMismatch "Response" invalid

data ReadyToViewType = ViewReady
    | ViewFailure
    | ViewLoading
    | ViewRegistered
    | ViewTranscoding
    | ViewProcessing
    | ViewUploading
    | ViewPreTranscoding
    | ViewUpdating
    deriving (Bounded, Enum, Show, Read, Generic, Eq)

instance ToJSON ReadyToViewType where
  toEncoding = genericToEncoding jsonOpts{ constructorTagModifier = fmap toLower . dropPrefix "View"}
  toJSON = genericToJSON jsonOpts{ constructorTagModifier = fmap toLower . dropPrefix "View"}


instance FromJSON ReadyToViewType where
  parseJSON = genericParseJSON jsonOpts{ constructorTagModifier = fmap toLower . dropPrefix "View"}

data Medium = Medium
    { _medium_id              :: MediumID
    , _medium_camera_model    :: Maybe String
    , _medium_captured_at     :: UTCTime
    , _medium_created_at      :: UTCTime
    , _medium_file_size       :: Maybe Int
    , _medium_moments_count   :: Int
    , _medium_ready_to_view   :: ReadyToViewType
    , _medium_source_duration :: Maybe String
    , _medium_type            :: MediumType
    , _medium_token           :: String
    , _medium_width           :: Maybe Int
    , _medium_height          :: Maybe Int
    , _medium_filename        :: Maybe String
    }
    deriving (Generic, Eq, Show)

makeLenses ''Medium

mediumMod :: String -> String
mediumMod = dropPrefix "_medium_"

instance ToJSON Medium where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = mediumMod}
  toJSON = genericToJSON jsonOpts{ fieldLabelModifier = mediumMod}

instance FromJSON Medium where
  parseJSON = genericParseJSON jsonOpts{ fieldLabelModifier = mediumMod}

-- | Get the thumbnail token for a given medium result.
thumbnailURL :: Int    -- ^ Server ID [1..4]
             -> Medium -- ^ The Medium whose thumbnail is requested
             -> String -- ^ A URL to a ~450 pixel wide thumbnail
thumbnailURL n Medium{_medium_token} = "https://images-0" <> show n <> ".gopro.com/resize/450wwp/" <> _medium_token

-- | Fetch a 450px wide thumbnail data for the given medium.
fetchThumbnail :: (HasGoProAuth m, MonadIO m) => Medium -> m BL.ByteString
fetchThumbnail m = do
  n <- liftIO $ getStdRandom (randomR (1,4))
  proxyAuth (thumbnailURL n m)

data Listing = Listing
    { _media :: [Medium]
    , _pages :: PageInfo
    }
    deriving (Generic, Eq, Show)

makeLenses ''Listing

instance FromJSON Listing where
  parseJSON (Object v) = do
    o <- v .: "_embedded"
    m <- o .: "media"
    ms <- traverse parseJSON (V.toList m)
    Listing ms <$> v .: "_pages"
  parseJSON invalid    = typeMismatch "Response" invalid

instance ToJSON Listing where
  toJSON Listing{..} = object ["_embedded" .= object [ "media" .= _media], "_pages" .= _pages]

-- | List a page worth of media.
list :: (HasGoProAuth m, MonadIO m)
  => Int -- ^ Number of items per page.
  -> Int -- ^ Page number (one-based).
  -> m ([Medium], PageInfo)
list psize page = do
  r <- jgetAuth ("https://api.gopro.com/media/search?fields=captured_at,created_at,file_size,id,moments_count,ready_to_view,source_duration,type,token,width,height,camera_model&order_by=created_at&per_page=" <> show psize <> "&page=" <> show page)
  pure (r ^.. media . folded,
        r ^. pages)

-- | List all media.
listAll :: (HasGoProAuth m, MonadIO m) => m [Medium]
listAll = listWhile (const True)

-- | List all media while returned batches pass the given predicate.
listWhile :: (HasGoProAuth m, MonadIO m) => ([Medium] -> Bool) -> m [Medium]
listWhile f = Map.elems <$> dig 0 mempty
    where
      dig n m = do
        (ms, _) <- list 100 n
        let m' = Map.union m . Map.fromList . map (\md@Medium{..} -> (_medium_id, md)) $ ms
        if (not . null) ms && f ms
          then dig (n + 1) m'
          else pure m'

-- | Get a list of items whose processing is failed or incomplete.
--
-- This includes items that are currently uploading, or items that will not upload.
notReady :: (HasGoProAuth m, MonadIO m) => m [Medium]
notReady = toListOf (media . folded) <$> jgetAuth "https://api.gopro.com/media/filters/not-ready?page=1&per_page=200"

class HasMediaURL c where
  media_url :: Lens' c String

class HasMediaHead c where
  media_head :: Lens' c String

class HasMediaLabel c where
  media_label :: Lens' c String

class HasMediaType c where
  media_type :: Lens' c String

class HasMediaItemNumber c where
  media_item_number :: Lens' c (Maybe Int)

data File = File
    { _file_camera_position :: String
    , _file_height          :: Int
    , _file_width           :: Int
    , _file_item_number     :: Int
    , _file_orientation     :: Int
    , _file_transforms      :: Maybe [String]
    , _file_head            :: String
    , _file_url             :: String
    }
    deriving (Generic, Eq, Show)

makeLenses  ''File

instance HasMediaURL File where media_url = file_url
instance HasMediaHead File where media_head = file_head

instance HasMediaItemNumber File where
  media_item_number = lens
                        (Just . _file_item_number)
                        (\f -> maybe f (\x -> f{_file_item_number=x}))

instance FromJSON File where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_file_"
    }

instance ToJSON File where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = dropPrefix "_file_" }
  toJSON = genericToJSON jsonOpts{ fieldLabelModifier = dropPrefix "_file_" }

data Variation = Variation
    { _var_height      :: Int
    , _var_width       :: Int
    , _var_label       :: String
    , _var_quality     :: String
    , _var_transforms  :: Maybe [String]
    , _var_item_number :: Maybe Int
    , _var_type        :: String
    , _var_head        :: String
    , _var_url         :: String
    }
    deriving (Generic, Eq, Show)

makeLenses ''Variation

instance HasMediaURL Variation where media_url = var_url
instance HasMediaHead Variation where media_head = var_head
instance HasMediaLabel Variation where media_label = var_label
instance HasMediaType Variation where media_type = var_type
instance HasMediaItemNumber Variation where media_item_number = var_item_number

instance FromJSON Variation where
  parseJSON = genericParseJSON defaultOptions {
  fieldLabelModifier = dropPrefix "_var_"
  }

instance ToJSON Variation where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = dropPrefix "_var_" }
  toJSON = genericToJSON jsonOpts{ fieldLabelModifier = dropPrefix "_var_" }

data SpriteFrame = SpriteFrame
    { _frame_count  :: Int
    , _frame_height :: Int
    , _frame_width  :: Int
    }
    deriving (Generic, Eq, Show)

makeLenses ''SpriteFrame

instance FromJSON SpriteFrame where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_frame_"
  }

instance ToJSON SpriteFrame where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = dropPrefix "_frame_" }
  toJSON = genericToJSON jsonOpts{ fieldLabelModifier = dropPrefix "_frame_" }

data Sprite = Sprite
    { _sprite_fps    :: Double
    , _sprite_frame  :: SpriteFrame
    , _sprite_height :: Int
    , _sprite_width  :: Int
    , _sprite_type   :: String
    , _sprite_heads  :: [String]
    , _sprite_urls   :: [String]
    }
    deriving (Generic, Eq, Show)

makeLenses ''Sprite

instance FromJSON Sprite where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_sprite_"
  }

instance ToJSON Sprite where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = dropPrefix "_sprite_" }
  toJSON = genericToJSON jsonOpts{ fieldLabelModifier = dropPrefix "_sprite_" }

data SidecarFile = SidecarFile
  { _sidecar_fps         :: Double
  , _sidecar_label       :: String
  , _sidecar_type        :: String
  , _sidecar_head        :: String
  , _sidecar_url         :: String
  , _sidecar_item_number :: Maybe Int
  } deriving (Generic, Eq, Show)

makeLenses ''SidecarFile

instance HasMediaURL SidecarFile where media_url = sidecar_url
instance HasMediaHead SidecarFile where media_head = sidecar_head
instance HasMediaLabel SidecarFile where media_label = sidecar_label
instance HasMediaType SidecarFile where media_type = sidecar_type
instance HasMediaItemNumber SidecarFile where media_item_number = sidecar_item_number

instance FromJSON SidecarFile where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_sidecar_"
  }

instance ToJSON SidecarFile where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = dropPrefix "_sidecar_" }
  toJSON = genericToJSON jsonOpts{ fieldLabelModifier = dropPrefix "_sidecar_" }

data FileStuff = FileStuff
    { _files         :: [File]
    , _variations    :: [Variation]
    , _sprites       :: [Sprite]
    , _sidecar_files :: [SidecarFile]
    }
    deriving (Generic, Eq, Show)

makeLenses ''FileStuff

instance FromJSON FileStuff where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON FileStuff where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = dropPrefix "_" }
  toJSON = genericToJSON jsonOpts{ fieldLabelModifier = dropPrefix "_" }

data FileInfo = FileInfo
    { _fileStuff :: FileStuff
    , _filename  :: String
    }
    deriving (Generic, Eq, Show)

makeLenses ''FileInfo

instance FromJSON FileInfo where
  parseJSON (Object v) = do
    o <- v .: "_embedded"
    fs <- parseJSON o
    FileInfo fs <$> v .: "filename"
  parseJSON invalid    = typeMismatch "Response" invalid

instance ToJSON FileInfo where
  toJSON FileInfo{..} = object ["_embedded" .= _fileStuff, "filename" .= _filename]

dlURL :: MediumID -> String
dlURL k = "https://api.gopro.com/media/" <> T.unpack k <> "/download"

-- | Get download descriptors for a given medium.  The format is
-- typically 'FileInfo', but it can be useful to map it into something
-- else.
retrieve :: (HasGoProAuth m, FromJSON j, MonadIO m) => MediumID -> m j
retrieve k = jgetAuth (dlURL k)

data Error = Error
    { _error_reason      :: String
    , _error_code        :: Int
    , _error_description :: String
    , _error_id          :: String
    }
    deriving (Generic, Show)

makeLenses ''Error

instance FromJSON Error where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_error_"
  }

newtype Errors = Errors [Error] deriving (Show)

instance FromJSON Errors where
  parseJSON (Object v) = do
    o <- v .: "_embedded"
    e <- o .: "errors"
    Errors <$> parseJSON e
  parseJSON invalid    = typeMismatch "Response" invalid

-- | Delete an item.
delete :: (HasGoProAuth m, MonadIO m) => MediumID -> m [Error]
delete k = do
  tok <- _access_token <$> goproAuth
  let u = "https://api.gopro.com/media?ids=" <> k
  Errors r <- view responseBody <$> liftIO (deleteWith (authOpts tok) (T.unpack u) >>= asJSON)
  pure r

mediumURL :: MediumID -> String
mediumURL = ("https://api.gopro.com/media/" <>) . T.unpack

-- | Get the current 'Medium' record for the given Medium ID.
medium :: (HasGoProAuth m, FromJSON j, MonadIO m) => MediumID -> m j
medium = jgetAuth . mediumURL

-- | Put a Medium.  It's probably best to get a raw JSON Value and update it in place.
putMedium :: (HasGoProAuth m, MonadIO m, Putable a) => MediumID -> a -> m ()
putMedium mid = fmap v . jputAuth (mediumURL mid)
  where
    v :: Value -> ()
    v = const ()

-- | Fetch, modify, and store a medium value.
updateMedium :: (HasGoProAuth m, MonadIO m, FromJSON j, Putable a)
             => (j -> a) -- ^ Transformation function.
             -> MediumID -- ^ Medium to update.
             -> m ()
updateMedium f m = (f <$> medium m) >>= putMedium m

-- | A moment of interestingness in a Medium.
data Moment = Moment
    { _moment_id   :: T.Text
    , _moment_time :: Maybe Int
    }
    deriving (Show, Eq, Generic)

makeLenses ''Moment

instance FromJSON Moment where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_moment_"
  }

instance ToJSON Moment where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = dropPrefix "_moment_" }
  toJSON = genericToJSON jsonOpts{ fieldLabelModifier = dropPrefix "_moment_" }

newtype Moments = Moments { unMoments :: [Moment] }

instance FromJSON Moments where
  parseJSON (Object v) = do
    o <- v .: "_embedded"
    m <- o .: "moments"
    Moments <$> traverse parseJSON (V.toList m)

  parseJSON invalid    = typeMismatch "Response" invalid

-- | Get the moments for the given medium.
moments :: (HasGoProAuth m, MonadIO m) => MediumID -> m [Moment]
moments mid = unMoments <$> jgetAuth ("https://api.gopro.com/media/" <> T.unpack mid <> "/moments?fields=time")
