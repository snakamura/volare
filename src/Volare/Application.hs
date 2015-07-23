{-# OPTIONS_GHC -fno-warn-orphans #-}

module Volare.Application
    ( makeVolare
    , withVolare
    ) where

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Class
    ( applyEnv
    , createPoolConfig
    , loadConfig
    , runPool
    )
import Database.Persist.Sql (runMigration)
import qualified Network.HTTP.Client as Http
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Yesod.Core.Dispatch
    ( mkYesodDispatch
    , toWaiApp
    )
import Yesod.Default.Config
    ( AppConfig
    , DefaultEnv
    , appEnv
    , fromArgs
    , withYamlEnvironment
    )

import Volare.Config
    ( Config
    , parseConfig
    )
import Volare.Foundation
import Volare.Handler.AMEDAS
import Volare.Handler.Flight
import Volare.Handler.Forcast
import Volare.Handler.MSM
import Volare.Handler.Root
import Volare.Handler.Route
import Volare.Handler.UAS
import Volare.Handler.Waypoint
import Volare.Handler.WINDAS
import Volare.Handler.Workspace
import Volare.Model (migrateAll)
import Volare.Static (staticSite)


mkYesodDispatch "Volare" resourcesVolare


makeVolare :: AppConfig DefaultEnv Config ->
              IO Application
makeVolare config = do
    persistConfig <- withYamlEnvironment "config/persist.yml" (appEnv config) loadConfig >>= applyEnv
    pool <- createPoolConfig persistConfig
    runStderrLoggingT $ runPool persistConfig (runMigration migrateAll) pool
    manager <- Http.newManager Http.defaultManagerSettings
    static <- staticSite
    toWaiApp $ Volare config persistConfig pool manager static


withVolare :: (Application -> IO ()) -> IO ()
withVolare f = do
    config <- fromArgs $ const parseConfig
    app <- makeVolare config
    f $ logStdout app
