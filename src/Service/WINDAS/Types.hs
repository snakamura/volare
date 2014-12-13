module Service.WINDAS.Types
    ( Station(..)
    , Observation(..)
    , Item(..)
    ) where

import qualified Data.Text as T


data Station = Station
    { id        :: Int
    , latitude  :: Float
    , longitude :: Float
    , elevation :: Int
    , message   :: Int
    , name      :: T.Text
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
