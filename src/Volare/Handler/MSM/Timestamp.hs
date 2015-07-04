module Volare.Handler.MSM.Timestamp
    ( Timestamp(..)
    ) where

import Data.Aeson.TH
    ( defaultOptions
    , deriveJSON
    )

data Timestamp = Timestamp {
    year  :: Int,
    month :: Int,
    day   :: Int,
    hour  :: Int
} deriving Show

deriveJSON defaultOptions ''Timestamp
