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
import Yesod.Core
    ( HandlerSite
    , MonadHandler
    , Yesod(..)
    , defaultClientSessionBackend
    , renderRoute
    )
import Yesod.Core.Dispatch
    ( mkYesodData
    , parseRoutesFile
    )
import Yesod.Core.Handler (getYesod)
import Yesod.Form
    ( FormMessage
    , FormResult
    , MForm
    , defaultFormMessage
    )
import Yesod.Persist (YesodPersist(..))
import Yesod.Static (Static)

import qualified Volare.Model as M
import Volare.Settings
    ( PersistConfig
    , Settings
    )
import qualified Volare.Settings as Settings


data Volare = Volare
    { volareSettings       :: Settings
    , volareConnectionPool :: PersistConfigPool PersistConfig
    , volareHttpManager    :: Http.Manager
    , volareStatic         :: Static
    }


mkYesodData "Volare" $(parseRoutesFile "config/routes")


instance Yesod Volare where
    makeSessionBackend _ = Just <$> defaultClientSessionBackend 120 "config/client_session_key.aes"


instance YesodPersist Volare where
    type YesodPersistBackend Volare = SqlBackend

    runDB action = do
        persistConfig <- (Settings.persistConfig . volareSettings) <$> getYesod
        pool <- volareConnectionPool <$> getYesod
        runPool persistConfig action pool


instance RenderMessage Volare FormMessage where
    renderMessage _ _ = defaultFormMessage


type Form a = Html ->
              MForm IO (FormResult a, Widget)


instance JSON.ToJSON a => ToJavascript a where
    toJavascript = Javascript . T.fromText . T.decodeUtf8 . B.concat . BL.toChunks . JSON.encode


getSettings :: (MonadHandler m, HandlerSite m ~ Volare) =>
               m Settings
getSettings = volareSettings <$> getYesod


getHttpManager :: (MonadHandler m, HandlerSite m ~ Volare) =>
                  m Http.Manager
getHttpManager = volareHttpManager <$> getYesod
