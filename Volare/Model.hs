module Volare.Model where

import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.Time (Day,
                  UTCTime)
import Database.Persist (PersistEntity(..),
                         PersistEntityBackend,
                         PersistField(..))
import Database.Persist.TH (mkMigrate,
                            mkPersist,
                            persist,
                            share,
                            sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Flight
  name T.Text
  date Day
  minLatitude Double
  maxLatitude Double
  minLongitude Double
  maxLongitude Double
  minAltitude Double
  maxAltitude Double
  deriving Show
Record
  flightId FlightId
  index Int
  time UTCTime
  latitude Double
  longitude Double
  altitude Double
  deriving Show
|]

instance JSON.ToJSON Record where
    toJSON record = JSON.object [
                             "time" .= recordTime record,
                             "latitude" .= recordLatitude record,
                             "longitude" .= recordLongitude record,
                             "altitude" .= recordAltitude record
                            ]
