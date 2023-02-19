{-# OPTIONS_GHC -fno-warn-orphans #-}

module Volare.Application
    ( getApplication
    ) where

import Control.Monad ((>=>))
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Class
    ( createPoolConfig
    , runPool
    )
import Database.Persist.Sql (runMigration)
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Network.Wai (Application)
import Yesod.Core.Dispatch
    ( mkYesodDispatch
    , toWaiApp
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
import Volare.Settings (Settings)
import qualified Volare.Settings as Settings
import Volare.Static (staticSite)


mkYesodDispatch "Volare" resourcesVolare


makeVolare :: Settings ->
              IO Volare
makeVolare settings = do
    let persistConfig = Settings.persistConfig settings
    pool <- createPoolConfig persistConfig
    runStderrLoggingT $ runPool persistConfig (runMigration migrateAll) pool
    manager <- Http.newManager Http.tlsManagerSettings
    static <- staticSite
    return $ Volare settings pool manager static


getApplication :: Settings ->
                  IO Application
getApplication = makeVolare >=> toWaiApp
