module Volare.AMEDAS.Stations (
    stations
) where

import Volare.AMEDAS.Type (Station(..))


stations :: (Float, Float) ->
            (Float, Float) ->
            [Station]
stations (nwLat, nwLng) (seLat, seLng) = filter f allStations
    where
      f (Station _ _ lat lng _) = seLat <= lat && lat <= nwLat &&
                                  nwLng <= lng && lng <= seLng


allStations :: [Station]
allStations = [
    Station 40 47646 (c 36 03.4) (c 140 07.5) "Tsukuba",
    Station 40  0315 (c 36 50.0) (c 140 46.3) "Kitaibaraki",
    Station 40  0316 (c 36 46.7) (c 140 20.7) "Daigo",
    Station 40  1011 (c 36 34.8) (c 140 38.7) "Hitachi",
    Station 40  1331 (c 36 36.4) (c 140 19.5) "Hitachioomiya",
    Station 40 47629 (c 36 22.8) (c 140 28.0) "Mito",
    Station 40  0318 (c 36 23.0) (c 140 14.2) "Kasama",
    Station 40  1530 (c 36 16.9) (c 139 59.3) "Shimodate",
    Station 40  1082 (c 36 13.5) (c 140 05.9) "Tsukubasan",
    Station 40  0322 (c 36 10.1) (c 139 56.7) "Shimotsuma",
    Station 40  0320 (c 36 12.1) (c 139 43.0) "Koga",
    Station 40  0324 (c 36 06.2) (c 140 13.2) "Tuchiura",
    Station 40  1245 (c 36 10.1) (c 140 31.6) "Hokota",
    Station 40  1014 (c 35 53.4) (c 140 12.7) "Ryugasaki",
    Station 40  0325 (c 35 57.8) (c 140 37.3) "Kashima",
    Station 40  1635 (c 36 42.2) (c 140 43.0) "Takahagi"
  ]


c :: Int ->
     Float ->
     Float
c d m = fromIntegral d + m / 60
