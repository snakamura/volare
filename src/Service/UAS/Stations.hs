module Service.UAS.Stations
    ( station
    , stations
    ) where

import Data.List (find)

import Service.UAS.Types (Station(Station))
import qualified Service.UAS.Types as Types


station :: Int ->
           Maybe Station
station identifier = find ((== identifier) . Types.id) stations


stations :: [Station]
stations =
    [ Station 47646 "Tateno"
    ]
