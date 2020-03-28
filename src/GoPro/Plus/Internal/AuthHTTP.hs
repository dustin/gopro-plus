module GoPro.Plus.Internal.AuthHTTP where

import           Control.Lens
import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.Aeson               (FromJSON (..))
import qualified Data.ByteString.Lazy     as BL
import           Network.Wreq             (Options, asJSON, getWith, postWith,
                                           putWith, responseBody)
import           Network.Wreq.Types       (Postable, Putable)

import           GoPro.Plus.Auth
import           GoPro.Plus.Internal.HTTP


jgetAuth :: (HasGoProAuth m, MonadIO m, FromJSON a) => String -> m a
jgetAuth u = goproAuth >>= \AuthResponse{..} -> jgetWith (authOpts _access_token) u


authOptsM :: HasGoProAuth m => m Network.Wreq.Options
authOptsM = authOpts . _access_token <$> goproAuth

jgetWithAuth :: (HasGoProAuth m, MonadIO m, FromJSON a) => Network.Wreq.Options -> String -> m a
jgetWithAuth opts u = view responseBody <$> liftIO (getWith opts u >>= asJSON)

jputAuth :: (HasGoProAuth m, MonadIO m, FromJSON j, Putable a) => String -> a -> m j
jputAuth u a = authOptsM >>= \o -> view responseBody <$> liftIO (putWith o u a >>= asJSON)


-- | Proxy a request to GoPro with authentication.
proxyAuth :: (HasGoProAuth m, MonadIO m) => String -> m BL.ByteString
proxyAuth u = (_access_token <$> goproAuth) >>= \tok -> proxy tok u

jpostAuth :: (HasGoProAuth m, MonadIO m, Postable a, FromJSON r) => String -> a -> m r
jpostAuth u v = authOptsM >>= \opts -> view responseBody <$> liftIO (postWith opts u v >>= asJSON)
