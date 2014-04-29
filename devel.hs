{-# LANGUAGE PackageImports #-}

module Main (main) where

import Control.Concurrent
    ( forkIO
    , threadDelay)
import Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettings
    , setPort)
import System.Directory
    ( doesFileExist
    , removeFile)
import System.Exit (exitSuccess)
import Yesod.Default.Config
    ( DefaultEnv(Development)
    , configSettings
    , csParseExtra
    , loadConfig)
import Yesod.Default.Main (defaultDevelApp)

import "volare" Volare.Application (makeVolare)
import "volare" Volare.Config (parseConfig)


main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- defaultDevelApp loader makeVolare
    forkIO $ runSettings (setPort port defaultSettings) app
    loop
  where
    loader = loadConfig (configSettings Development) {
                 csParseExtra = const parseConfig
             }


loop :: IO ()
loop = do
    threadDelay 100000
    e <- doesFileExist "yesod-devel/devel-terminate"
    if e then
        terminateDevel
    else
        loop


terminateDevel :: IO ()
terminateDevel = exitSuccess
