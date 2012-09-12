module Volare.Foundation where

import Control.Applicative ((<$>))
import Control.Monad.Logger (LogLevel(LevelDebug))
import Database.Persist.GenericSql (SqlPersist)
import Database.Persist.Store (PersistConfigPool,
                               runPool)
import Text.Blaze.Html (Html)
import Text.Shakespeare.I18N (RenderMessage,
                              renderMessage)
import Web.ClientSession (getKey)
import Yesod.Core (Yesod(..),
                   clientSessionBackend,
                   renderRoute)
import Yesod.Default.Config (AppConfig,
                             DefaultEnv,
                             appExtra)
import Yesod.Dispatch (mkYesodData,
                       parseRoutesFile)
import Yesod.Form (FormMessage,
                   FormResult,
                   MForm,
                   defaultFormMessage)
import Yesod.Handler (getYesod)
import Yesod.Persist (YesodPersist(..))
import Yesod.Static (Static)

import Volare.Config (Config)
import qualified Volare.Model as M
import Volare.Settings (PersistConfig)


data Volare = Volare {
    volareConfig         :: AppConfig DefaultEnv Config,
    volarePersistConfig  :: PersistConfig,
    volareConnectionPool :: PersistConfigPool PersistConfig,
    volareStatic         :: Static
}


mkYesodData "Volare" $(parseRoutesFile "config/routes")


instance Yesod Volare where
    logLevel _ = LevelDebug

    makeSessionBackend _ = do
      key <- getKey "config/client_session_key.aes"
      return $ Just $ clientSessionBackend key 120


instance YesodPersist Volare where
    type YesodPersistBackend Volare = SqlPersist

    runDB action = do
      persistConfig <- volarePersistConfig <$> getYesod
      pool <- volareConnectionPool <$> getYesod
      runPool persistConfig action pool


instance RenderMessage Volare FormMessage where
    renderMessage _ _ = defaultFormMessage


type Form a = Html ->
              MForm Volare Volare (FormResult a, Widget)


getConfig :: Handler Config
getConfig = (appExtra . volareConfig) <$> getYesod
