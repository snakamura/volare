module Service.UAS.Types
    ( Station(..)
    , Observation(..)
    , Item(..)
    , Entry(..)
    ) where

import qualified Data.Text as T

data Station = Station
    { id   :: Int
    , name :: T.Text
    } deriving (Show, Eq)

data Observation = Observation
    { day   :: Int
    , hour  :: Int
    , items :: [Item]
    } deriving (Show, Eq)

data Item = SurfaceItem Int Entry
          | BarometricItem Int Entry
    deriving (Show, Eq)

data Entry = Entry
    { temperature   :: Maybe Float
    , dewPoint      :: Maybe Float
    , windDirection :: Maybe Int
    , windSpeed     :: Maybe Int
    } deriving (Show, Eq)
