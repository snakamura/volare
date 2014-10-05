module Service.WINDAS.Stations
    ( station
    , stations
    , allStations
    ) where

import Data.List (find)

import Service.WINDAS.Types (Station(Station))
import qualified Service.WINDAS.Types as Types


station :: Int ->
           Maybe Station
station identifier = find ((== identifier) . Types.id) allStations


stations :: (Float, Float) ->
            (Float, Float) ->
            [Station]
stations (nwLat, nwLng) (seLat, seLng) = filter f allStations
  where
    f (Station _ lat lng _ _ _) = seLat <= lat && lat <= nwLat &&
                                  nwLng <= lng && lng <= seLng


allStations :: [Station]
allStations =
    [ Station 47406 43.95 141.63  23 41 "Rumoi"
    , Station 47417 42.92 143.21  38 41 "Obihiro"
    , Station 47423 42.32 140.97   3 41 "Muroran"
    , Station 47570 37.49 139.91 212 42 "Wakamatsu"
    , Station 47585 39.65 141.96  43 42 "Miyako"
    , Station 47587 38.91 139.84   3 42 "Sakata"
    , Station 47590 38.26 140.90  39 42 "Sendai"
    , Station 47612 37.11 138.25  13 44 "Takada"
    , Station 47616 36.06 136.22   9 45 "Fukui"
    , Station 47626 36.15 139.38  30 43 "Kumagaya"
    , Station 47629 36.38 140.47  29 43 "Mito"
    , Station 47636 35.17 136.96  51 45 "Nagoya"
    , Station 47640 35.50 138.76 860 44 "Kawaguchiko"
    , Station 47656 34.98 138.40  14 44 "Shizuoka"
    , Station 47663 34.07 136.19  15 45 "Owase"
    , Station 47674 35.15 140.31  12 43 "Katsuura"
    , Station 47678 33.12 139.78 152 49 "Hachijojima"
    , Station 47746 35.53 134.20   6 49 "Tottori"
    , Station 47755 34.90 132.07  20 46 "Hamada"
    , Station 47795 33.89 135.13   9 49 "Mihama"
    , Station 47800 34.15 129.22 130 48 "Izuhara"
    , Station 47805 33.36 129.55  58 48 "Hirado"
    , Station 47815 33.24 131.62   5 47 "Oita"
    , Station 47819 32.81 130.71  38 47 "Kumamoto"
    , Station 47822 32.58 131.66  19 47 "Nobeoka"
    , Station 47836 30.38 130.66  36 48 "Yakushima"
    , Station 47848 31.71 130.32  25 50 "Ichiki"
    , Station 47891 34.32 134.05   9 46 "Takamatsu"
    , Station 47893 33.57 133.55   3 46 "Kouchi"
    , Station 47898 32.72 133.01  31 46 "Shimizu"
    , Station 47909 28.38 129.50   3 50 "Naze"
    , Station 47912 24.47 123.01  30 48 "Yonagunijima"
    , Station 47945 25.83 131.23  16 50 "Minamidaitojima"
    ]
