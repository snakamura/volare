module Volare.Settings
    ( PersistConfig
    , Settings(..)
    , development
    , staticDir
    , widgetFile
    ) where

import Data.Aeson
    ( FromJSON(..)
    , (.:)
    , withObject
    )
import Data.Default (def)
import Data.String (fromString)
import qualified Data.Text as T
import Database.Persist.Postgresql (PostgresConf)
import Language.Haskell.TH.Syntax
    ( Exp
    , Q
    )
import Network.Wai.Handler.Warp (HostPreference)
import Text.Hamlet
    ( NewlineStyle(NoNewlines)
    , defaultHamletSettings
    , hamletNewlines
    )
import Yesod.Default.Util
    ( WidgetFileSettings
    , wfsHamletSettings
    , widgetFileNoReload
    , widgetFileReload
    )


type PersistConfig = PostgresConf


data Settings = Settings
    { host :: HostPreference
    , port :: Int
    , persistConfig :: PersistConfig
    , googleApiKey :: T.Text
    } deriving Show

instance FromJSON Settings where
    parseJSON = withObject "Settings" $ \o ->
        Settings <$> (fromString <$> o .: "host")
                 <*> o .: "port"
                 <*> o .: "database"
                 <*> o .: "google-api-key"


development :: Bool
#if DEVELOPMENT
development = True
#else
development = False
#endif


staticDir :: FilePath
staticDir = if development then
                "static"
            else
                "static_build"


widgetFile :: String ->
              Q Exp
widgetFile = widgetFile' widgetFileSettings
  where
    widgetFile' = if development then
                      widgetFileReload
                  else
                      widgetFileNoReload


widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def {
    wfsHamletSettings = defaultHamletSettings {
        hamletNewlines = NoNewlines
    }
}
