{-# OPTIONS_GHC -fno-warn-orphans #-}

module Volare.Foundation where

import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Builder as T
import Database.Persist.Class
    ( PersistConfigPool
    , runPool
    )
import Database.Persist.Sql (SqlBackend)
import qualified Network.HTTP.Client as Http
import Text.Blaze.Html (Html)
import Text.Julius
    ( Javascript(..)
    , ToJavascript(..)
    )
import Text.Shakespeare.I18N
    ( RenderMessage
    , renderMessage
    )
import Web.ClientSession (getKey)
import Yesod.Core
    ( HandlerSite
    , MonadHandler
    , Yesod(..)
    , clientSessionBackend
    , clientSessionDateCacher
    , renderRoute
    )
import Yesod.Core.Dispatch
    ( mkYesodData
    , parseRoutesFile
    )
import Yesod.Core.Handler (getYesod)
import Yesod.Default.Config
    ( AppConfig
    , DefaultEnv
    , appExtra
    )
import Yesod.Form
    ( FormMessage
    , FormResult
    , MForm
    , defaultFormMessage
    )
import Yesod.Persist (YesodPersist(..))
import Yesod.Static (Static)

import Volare.Config (Config)
import qualified Volare.Model as M
import Volare.Settings (PersistConfig)


data Volare = Volare
    { volareConfig         :: AppConfig DefaultEnv Config
    , volarePersistConfig  :: PersistConfig
    , volareConnectionPool :: PersistConfigPool PersistConfig
    , volareHttpManager    :: Http.Manager
    , volareStatic         :: Static
    }


mkYesodData "Volare" $(parseRoutesFile "config/routes")


instance Yesod Volare where
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        (getCachedDate, _) <- clientSessionDateCacher $ 120 * 60
        return $ Just $ clientSessionBackend key getCachedDate


instance YesodPersist Volare where
    type YesodPersistBackend Volare = SqlBackend

    runDB action = do
        persistConfig <- volarePersistConfig <$> getYesod
        pool <- volareConnectionPool <$> getYesod
        runPool persistConfig action pool


instance RenderMessage Volare FormMessage where
    renderMessage _ _ = defaultFormMessage


type Form a = Html ->
              MForm IO (FormResult a, Widget)


instance JSON.ToJSON a => ToJavascript a where
    toJavascript = Javascript . T.fromText . T.decodeUtf8 . B.concat . BL.toChunks . JSON.encode


getConfig :: (MonadHandler m, HandlerSite m ~ Volare) =>
             m Config
getConfig = (appExtra . volareConfig) <$> getYesod


getHttpManager :: (MonadHandler m, HandlerSite m ~ Volare) =>
                  m Http.Manager
getHttpManager = volareHttpManager <$> getYesod
