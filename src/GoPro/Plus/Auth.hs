{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module GoPro.Plus.Auth where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (FromJSON (..), genericParseJSON)
import           Generics.Deriving.Base (Generic)
import           Network.Wreq           (FormParam (..))


import           GoPro.HTTP

apiClientID, apiClientSecret :: String
apiClientID = "71611e67ea968cfacf45e2b6936c81156fcf5dbe553a2bf2d342da1562d05f46"
apiClientSecret = "3863c9b438c07b82f39ab3eeeef9c24fefa50c6856253e3f1d37e0e3b1ead68d"

authURL :: String
authURL = "https://api.gopro.com/v1/oauth2/token"

-- | An Authentication response.
data AuthResponse = AuthResponse {
  _access_token        :: String
  , _expires_in        :: Int
  , _refresh_token     :: String
  , _resource_owner_id :: String
  } deriving(Generic, Show)

instance FromJSON AuthResponse where
  parseJSON = genericParseJSON jsonOpts

makeLenses ''AuthResponse

authenticate :: MonadIO m => String -> String -> m AuthResponse
authenticate username password =
  jpostWith defOpts authURL ["grant_type" := ("password" :: String),
                             "client_id" := apiClientID,
                             "client_secret" := apiClientSecret,
                             "scope" := ("root root:channels public me upload media_library_beta live" :: String),
                             "username" := username,
                             "password" := password]

-- | Refresh authentication credentials using a refresh token.
refreshAuth :: MonadIO m => AuthResponse -> m AuthResponse
refreshAuth AuthResponse{..} =
  jpostWith defOpts authURL ["grant_type" := ("refresh_token" :: String),
                             "client_id" := apiClientID,
                             "client_secret" := apiClientSecret,
                             "refresh_token" := _refresh_token]
