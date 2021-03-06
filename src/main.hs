module Main (main) where

import Network.Wai.Handler.Warp as Warp
import Yesod.Default.Config2
    ( configSettingsYml
    , loadYamlSettings
    , useEnv
    )

import Volare.Application (getApplication)
import qualified Volare.Settings as Settings


main :: IO ()
main = do
    settings <- loadYamlSettings [configSettingsYml] [] useEnv
    application <- getApplication settings
    let warpSettings = setHost (Settings.host settings) $ setPort (Settings.port settings) Warp.defaultSettings
    Warp.runSettings warpSettings application
