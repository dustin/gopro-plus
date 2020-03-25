module GoPro.HTTP where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Aeson             (FromJSON (..), Options (..),
                                         defaultOptions, fieldLabelModifier)
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as BL
import           Network.Wreq           (Options, asJSON, defaults, getWith,
                                         header, postWith, responseBody)
import           Network.Wreq.Types     (Postable)

userAgent :: BC.ByteString
userAgent = "github.com/dustin/gopro 0.1"

defOpts :: Network.Wreq.Options
defOpts = defaults & header "User-Agent" .~ [userAgent]


authOpts :: String -> Network.Wreq.Options
authOpts tok = defOpts & header "Authorization" .~ ["Bearer " <> BC.pack tok]
               & header "Accept" .~ ["application/vnd.gopro.jk.media+json; version=2.0.0"]
               & header "Content-Type" .~ ["application/json"]

type Token = String

jsonOpts :: Data.Aeson.Options
jsonOpts = defaultOptions {
  fieldLabelModifier = dropWhile (== '_')
  }

-- | Proxy a request to GoPro with authentication.
proxy :: MonadIO m => Token -> String -> m BL.ByteString
proxy tok u = do
  r <- liftIO $ getWith (authOpts tok) u
  pure $ r ^. responseBody

jget :: (MonadIO m, FromJSON a) => Token -> String -> m a
jget tok = jgetWith (authOpts tok)

jgetWith :: (MonadIO m, FromJSON a) => Network.Wreq.Options -> String -> m a
jgetWith opts u = view responseBody <$> liftIO (getWith opts u >>= asJSON)

jpostWith :: (MonadIO m, Postable a, FromJSON r) => Network.Wreq.Options -> String -> a -> m r
jpostWith opts u v = view responseBody <$> liftIO (postWith opts u v >>= asJSON)
