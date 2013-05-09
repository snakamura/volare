module Volare.Model where

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.Persist (Entity(Entity))
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (mkMigrate,
                            mkPersist,
                            persistFileWith,
                            share,
                            sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "config/models")

instance JSON.ToJSON (Entity Flight) where
    toJSON (Entity flightId flight) = JSON.object [
                                          "id" .= flightId,
                                          "name" .= flightName flight,
                                          "time" .= flightTime flight,
                                          "duration" .= flightDuration flight
                                        ]

instance JSON.ToJSON (Entity Record) where
    toJSON (Entity recordId record) = JSON.object [
                                          "id" .= recordId,
                                          "time" .= recordTime record,
                                          "latitude" .= recordLatitude record,
                                          "longitude" .= recordLongitude record,
                                          "altitude" .= recordAltitude record
                                        ]

instance JSON.ToJSON (Entity Workspace) where
    toJSON (Entity workspaceId workspace) = JSON.object [
                                                "id" .= workspaceId,
                                                "name" .= workspaceName workspace
                                              ]
