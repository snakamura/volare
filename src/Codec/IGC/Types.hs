module Codec.IGC.Types (
    IGC(..),
    Record(..),
    Position(..)
) where

import Data.Time (Day,
                  DiffTime)


data IGC = IGC {
      date    :: Day,
      records :: [Record]
    } deriving Show


data Record = Record {
      time     :: DiffTime,
      position :: Position
    } deriving Show


data Position = Position {
      latitude  :: Float,
      longitude :: Float,
      altitude  :: Float
    } deriving Show
