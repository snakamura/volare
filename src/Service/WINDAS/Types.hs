module Service.WINDAS.Types
    ( Location(..)
    , Observation(..)
    , Item(..)
    , Station(..)
    ) where

import qualified Data.Text as T


data Location = Location
    { locationId        :: Int
    , locationLatitude  :: Float
    , locationLongitude :: Float
    , locationAltitude  :: Int
    } deriving (Show, Eq)

data Observation = Observation
    { year   :: Int
    , month  :: Int
    , day    :: Int
    , hour   :: Int
    , minute :: Int
    , items  :: [Item]
    } deriving (Show, Eq)

data Item = Item
    { altitude      :: Int
    , eastwardWind  :: Float
    , northwardWind :: Float
    , verticalWind  :: Float
    } deriving (Show, Eq)


data Station = Station
    { stationId        :: Int
    , stationLatitude  :: Float
    , stationLongitude :: Float
    , stationMessage   :: Int
    , stationName      :: T.Text
    } deriving (Show, Eq)
