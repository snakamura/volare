module Codec.GeoWpt.Types (
   Wpt(..),
   Item(..)
) where

import qualified Data.Text as T


data Wpt = Wpt {
      items :: [Item]
    } deriving Show


data Item = Item {
      name        :: T.Text,
      latitude    :: Float,
      longitude   :: Float,
      altitude    :: Int,
      description :: T.Text
    } deriving Show
