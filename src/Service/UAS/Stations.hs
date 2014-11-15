module Service.UAS.Stations
    ( station
    , stations
    , allStations
    ) where

import Data.List (find)

import Service.UAS.Types (Station(Station))
import qualified Service.UAS.Types as Types


station :: Int ->
           Maybe Station
station identifier = find ((== identifier) . Types.id) allStations


stations :: (Float, Float) ->
            (Float, Float) ->
            [Station]
stations (nwLat, nwLng) (seLat, seLng) = filter f allStations
  where
    f (Station _ lat lng _) = seLat <= lat && lat <= nwLat &&
                              nwLng <= lng && lng <= seLng


allStations :: [Station]
allStations =
    [ Station 47401 45.42 141.68 "Wakkanai"
    , Station 47412 43.07 141.33 "Sapporo"
    , Station 47418 42.95 144.43 "Kushiro"
    , Station 47580 40.68 141.37 "Misawa Ab"
    , Station 47582 39.72 140.10 "Akita"
    , Station 47600 37.40 136.90 "Wajima"
    , Station 47646 36.05 140.13 "Tateno"
    , Station 47678 33.12 139.78 "Hachijyojima"
    , Station 47681 34.73 137.67 "Hamamatsu Ab"
    , Station 47741 35.45 133.07 "Matsue"
    , Station 47778 33.45 135.75 "Shionomisaki"
    , Station 47807 33.58 130.38 "Fukuoka"
    , Station 47827 31.55 130.55 "Kagoshima"
    , Station 47909 28.40 129.55 "Naze"
    , Station 47918 24.33 124.17 "Ishigakijima"
    , Station 47945 25.83 131.23 "Minamidaitojima"
    , Station 47971 27.10 142.18 "Chichijima"
    , Station 47991 24.28 153.98 "Minamitorishima"
    ]
