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
  retrieve, delete,
  fetchThumbnail,
  -- * Data Types
  PageInfo(..), current_page, per_page, total_items, total_pages,
  MediumID,
  Medium(..), medium_id, medium_camera_model, medium_captured_at,
  medium_created_at, medium_file_size, medium_moments_count,
  medium_ready_to_view, medium_source_duration, medium_type,
  medium_token, medium_width, medium_height,
  Listing(..), media, pages,
  File(..), file_camera_position, file_height, file_width,
  file_item_number, file_orientation, file_url,
  Variation(..), var_height, var_width, var_label, var_quality,
  var_type, var_url,
  SpriteFrame(..), frame_count, frame_height, frame_width,
  Sprite(..), sprite_fps, sprite_frame, sprite_height, sprite_width,
  sprite_type, sprite_urls,
  FileStuff(..), files, variations, sprites, sidecar_files,
  FileInfo(..), fileStuff, filename,
  Error(..), error_reason, error_code, error_description, error_id,
  -- * Low-level Junk
  putRawMedium
  ) where

import           Control.Lens
import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.Aeson               (FromJSON (..), Options (..),
                                           ToJSON (..), Value (..),
                                           defaultOptions, fieldLabelModifier,
                                           genericParseJSON, genericToEncoding,
                                           genericToJSON, (.:))
import           Data.Aeson.Types         (typeMismatch)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import           Data.Time.Clock          (UTCTime)
import qualified Data.Vector              as V
import           Generics.Deriving.Base   (Generic)
import           Network.Wreq             (asJSON, deleteWith, putWith,
                                           responseBody)
import           System.Random            (getStdRandom, randomR)


import           GoPro.Plus.Internal.HTTP

data PageInfo = PageInfo {
  _current_page :: Int,
  _per_page     :: Int,
  _total_items  :: Int,
  _total_pages  :: Int
  } deriving (Generic, Show)

makeLenses ''PageInfo

instance FromJSON PageInfo where
  parseJSON = genericParseJSON jsonOpts

type MediumID = T.Text

data Medium = Medium {
  _medium_id              :: MediumID,
  _medium_camera_model    :: Maybe String,
  _medium_captured_at     :: UTCTime,
  _medium_created_at      :: UTCTime,
  _medium_file_size       :: Maybe Int,
  _medium_moments_count   :: Int,
  _medium_ready_to_view   :: String,
  _medium_source_duration :: Maybe String,
  _medium_type            :: String,
  _medium_token           :: String,
  _medium_width           :: Maybe Int,
  _medium_height          :: Maybe Int
  } deriving (Generic, Show)

makeLenses ''Medium

dropPrefix :: String -> (String -> String)
dropPrefix s = drop (length s)

mediumMod :: String -> String
mediumMod = dropPrefix "_medium_"

instance ToJSON Medium where
  toEncoding = genericToEncoding jsonOpts{ fieldLabelModifier = mediumMod}
  toJSON = genericToJSON jsonOpts{ fieldLabelModifier = mediumMod}

instance FromJSON Medium where
  parseJSON = genericParseJSON jsonOpts{ fieldLabelModifier = mediumMod}

-- | Get the thumbnail token for a given medium result.
thumbnailURL :: Int -> Medium -> String
thumbnailURL n Medium{_medium_token} = "https://images-0" <> show n <> ".gopro.com/resize/450wwp/" <> _medium_token

-- | Fetch thumbnail data for the given medium.
fetchThumbnail :: MonadIO m => Token -> Medium -> m BL.ByteString
fetchThumbnail tok m = do
  n <- liftIO $ getStdRandom (randomR (1,4))
  proxy tok (thumbnailURL n m)

data Listing = Listing {
  _media :: [Medium],
  _pages :: PageInfo
  } deriving (Generic, Show)

makeLenses ''Listing

instance FromJSON Listing where
  parseJSON (Object v) = do
    o <- v .: "_embedded"
    m <- o .: "media"
    ms <- traverse parseJSON (V.toList m)
    Listing ms <$> v .: "_pages"
  parseJSON invalid    = typeMismatch "Response" invalid

-- | List a page worth of media.
list :: MonadIO m => Token -> Int -> Int -> m ([Medium], PageInfo)
list tok psize page = do
  r <- jget tok ("https://api.gopro.com/media/search?fields=captured_at,created_at,file_size,id,moments_count,ready_to_view,source_duration,type,token,width,height,camera_model&order_by=created_at&per_page=" <> show psize <> "&page=" <> show page)
  pure $ (r ^.. media . folded,
          r ^. pages)

-- | List all media.
listAll :: MonadIO m => Token -> m [Medium]
listAll tok = listWhile tok (const True)

-- | List all media while returned batches pass the given predicate.
listWhile :: MonadIO m => Token -> ([Medium] -> Bool) -> m [Medium]
listWhile tok f = do
  Map.elems <$> dig 0 mempty
    where
      dig n m = do
        (ms, _) <- list tok 100 n
        let m' = Map.union m . Map.fromList . map (\md@Medium{..} -> (_medium_id, md)) $ ms
        if (not . null) ms && f ms
          then dig (n + 1) m'
          else pure m'


data File = File {
  _file_camera_position :: String,
  _file_height          :: Int,
  _file_width           :: Int,
  _file_item_number     :: Int,
  _file_orientation     :: Int,
  _file_url             :: String
  } deriving (Generic, Show)

makeLenses  ''File

instance FromJSON File where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_file_"
    }

data Variation = Variation {
  _var_height  :: Int,
  _var_width   :: Int,
  _var_label   :: String,
  _var_quality :: String,
  _var_type    :: String,
  _var_url     :: String
  } deriving(Generic, Show)

makeLenses ''Variation

instance FromJSON Variation where
  parseJSON = genericParseJSON defaultOptions {
  fieldLabelModifier = dropPrefix "_var_"
  }

data SpriteFrame = SpriteFrame {
  _frame_count  :: Int,
  _frame_height :: Int,
  _frame_width  :: Int
  } deriving(Generic, Show)

makeLenses ''SpriteFrame

instance FromJSON SpriteFrame where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_frame_"
  }

data Sprite = Sprite {
  _sprite_fps    :: Double,
  _sprite_frame  :: SpriteFrame,
  _sprite_height :: Int,
  _sprite_width  :: Int,
  _sprite_type   :: String,
  _sprite_urls   :: [String]
  } deriving (Generic, Show)

makeLenses ''Sprite

instance FromJSON Sprite where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = dropPrefix "_sprite_"
  }

data FileStuff = FileStuff {
  _files         :: [File],
  _variations    :: [Variation],
  _sprites       :: [Sprite],
  _sidecar_files :: [Value]
  } deriving (Generic, Show)

makeLenses ''FileStuff

instance FromJSON FileStuff where
  parseJSON = genericParseJSON jsonOpts

data FileInfo = FileInfo {
  _fileStuff :: FileStuff,
  _filename  :: String
  } deriving (Generic, Show)

makeLenses ''FileInfo

instance FromJSON FileInfo where
  parseJSON (Object v) = do
    o <- v .: "_embedded"
    fs <- parseJSON o
    FileInfo fs <$> v .: "filename"
  parseJSON invalid    = typeMismatch "Response" invalid

dlURL :: MediumID -> String
dlURL k = "https://api.gopro.com/media/" <> T.unpack k <> "/download"

-- | Get download descriptors for a given medium.  The format is
-- typically 'FileInfo', but it can be useful to map it into something
-- else.
retrieve :: (FromJSON j, MonadIO m) => Token -> MediumID -> m j
retrieve tok k = jget tok (dlURL k)

data Error = Error {
  _error_reason      :: String,
  _error_code        :: Int,
  _error_description :: String,
  _error_id          :: String
  } deriving (Generic, Show)

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
delete :: MonadIO m => Token -> MediumID -> m [Error]
delete tok k = do
  let u = "https://api.gopro.com/media?ids=" <> k
  Errors r <- view responseBody <$> liftIO (deleteWith (authOpts tok) (T.unpack u) >>= asJSON)
  pure r

mediumURL :: MediumID -> String
mediumURL = ("https://api.gopro.com/media/" <>) . T.unpack

-- | Get the current 'Medium' record for the given Medium ID.
medium :: (FromJSON j, MonadIO m) => Token -> MediumID -> m j
medium tok = jget tok . mediumURL

putRawMedium :: MonadIO m => Token -> MediumID -> Value -> m Value
putRawMedium tok mid v = view responseBody <$> liftIO (putWith (authOpts tok) (mediumURL mid) v >>= asJSON)

