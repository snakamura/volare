module Service.UAS.Types
    ( Station(..)
    , Observation(..)
    , Item(..)
    , Plane(..)
    , Pressure(..)
    , Entry(..)
    ) where

import qualified Data.Text as T

data Station = Station
    { id        :: Int
    , latitude  :: Float
    , longitude :: Float
    , name      :: T.Text
    } deriving (Show, Eq)

data Observation = Observation
    { day       :: Int
    , hour      :: Int
    , stationId :: Int
    , items     :: [Item]
    } deriving (Show, Eq)

data Item = Item
    { plane    :: Plane
    , pressure :: Pressure
    , entry    :: Entry
    } deriving (Show, Eq)

data Plane = Surface
           | Barometric (Maybe Int)
    deriving (Show, Eq)

newtype Pressure = Pressure Int deriving (Show, Eq, Ord)

data Entry = Entry
    { temperature   :: Maybe Float
    , dewPoint      :: Maybe Float
    , windDirection :: Maybe Int
    , windSpeed     :: Maybe Int
    } deriving (Show, Eq)
