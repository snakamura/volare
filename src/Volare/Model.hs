{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Volare.Model where

import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (mkDeleteCascade,
                            mkMigrate,
                            mkPersist,
                            persistFileWith,
                            share)

import Volare.Model.Settings (settings)

share [mkPersist settings, mkMigrate "migrateAll", mkDeleteCascade settings] $(persistFileWith lowerCaseSettings "config/models")
