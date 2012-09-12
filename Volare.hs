{-# OPTIONS_GHC -fno-warn-orphans #-}

module Volare (
    makeVolare,
    withVolare
) where

import Database.Persist.GenericSql (runMigration)
import Database.Persist.Store (applyEnv,
                               createPoolConfig,
                               loadConfig,
                               runPool)
import Network.Wai (Application)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Yesod.Core (yesodDispatch)
import Yesod.Default.Config (AppConfig,
                             DefaultEnv,
                             appEnv,
                             fromArgs,
                             withYamlEnvironment)
import Yesod.Dispatch (mkYesodDispatch,
                       toWaiApp)

import Volare.Config (Config,
                      parseConfig)
import Volare.Foundation
import Volare.Handler.Flight
import Volare.Handler.Root
import Volare.Model (migrateAll)
import Volare.Static (staticSite)


mkYesodDispatch "Volare" resourcesVolare


makeVolare :: AppConfig DefaultEnv Config ->
              IO Application
makeVolare config = do
  persistConfig <- withYamlEnvironment "config/persist.yml" (appEnv config) loadConfig >>= applyEnv
  pool <- createPoolConfig persistConfig
  runPool persistConfig (runMigration migrateAll) pool
  s <- staticSite
  toWaiApp $ Volare config persistConfig pool s


withVolare :: (Application -> IO ()) -> IO ()
withVolare f = do
  config <- fromArgs $ const parseConfig
  app <- makeVolare config
  f $ logStdout $ app
