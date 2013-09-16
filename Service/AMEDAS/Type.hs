module Service.AMEDAS.Type (
    Item(..),
    WindDirection(..),
    Station(..)
) where

import qualified Data.Aeson as JSON
import Data.Aeson.TH (defaultOptions,
                      deriveToJSON)
import qualified Data.Text as T


data Item = Item {
    time          :: Int,
    precipitation :: Maybe Float,
    temperature   :: Maybe Float,
    windSpeed     :: Maybe Float,
    windDirection :: Maybe WindDirection,
    sunshine      :: Maybe Float
} deriving (Show, Eq)


data WindDirection = N
                   | NNE
                   | NE
                   | ENE
                   | E
                   | ESE
                   | SE
                   | SSE
                   | S
                   | SSW
                   | SW
                   | WSW
                   | W
                   | WNW
                   | NW
                   | NNW
                   | CALM
  deriving (Show, Read, Eq)


data Station = Station {
    prec      :: Int,
    block     :: Int,
    latitude  :: Float,
    longitude :: Float,
    name      :: String
} deriving Show


deriveToJSON defaultOptions ''Item

instance JSON.ToJSON WindDirection where
    toJSON = JSON.String . T.pack . show

deriveToJSON defaultOptions ''Station
