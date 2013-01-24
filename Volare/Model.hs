module Volare.Model where

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist (Entity(Entity),
                         PersistEntity(..),
                         PersistEntityBackend,
                         PersistField(..))
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (mkMigrate,
                            mkPersist,
                            persistFileWith,
                            share,
                            sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "config/models")

instance JSON.ToJSON (Entity Flight) where
    toJSON (Entity id flight) = JSON.object [
                                    "id" .= id,
                                    "name" .= flightName flight,
                                    "time" .= flightTime flight,
                                    "duration" .= flightDuration flight
                                  ]

instance JSON.ToJSON (Entity Record) where
    toJSON (Entity id record) = JSON.object [
                                    "id" .= id,
                                    "time" .= recordTime record,
                                    "latitude" .= recordLatitude record,
                                    "longitude" .= recordLongitude record,
                                    "altitude" .= recordAltitude record
                                  ]

instance JSON.ToJSON (Entity Workspace) where
    toJSON (Entity id workspace) = JSON.object [
                                       "id" .= id,
                                       "name" .= workspaceName workspace
                                     ]
