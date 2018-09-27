module Main (main) where

import Network.Wai (Application)
import Network.Wai.Handler.Warp as Warp
import Yesod.Default.Config2
    ( configSettingsYml
    , develMainHelper
    , getDevSettings
    , loadYamlSettings
    , useEnv
    )

import Volare.Application (getApplication)
import qualified Volare.Settings as Settings


main :: IO ()
main = develMainHelper getApplicationDev


getApplicationDev :: IO (Warp.Settings, Application)
getApplicationDev = do
    settings <- loadYamlSettings [configSettingsYml] [] useEnv
    application <- getApplication settings
    warpSettings <- getDevSettings $ setHost (Settings.host settings) $ setPort (Settings.port settings) Warp.defaultSettings
    return  (warpSettings, application)
