module Service.WINDAS.Stations (stations) where

import Service.WINDAS.Types (Station(Station))


stations :: (Float, Float) ->
            (Float, Float) ->
            [Station]
stations (nwLat, nwLng) (seLat, seLng) = filter f allStations
  where
    f (Station _ lat lng _ _) = seLat <= lat && lat <= nwLat &&
                                nwLng <= lng && lng <= seLng


allStations :: [Station]
allStations =
    [ Station 47406 43.95 141.63 41 "Rumoi"
    , Station 47417 42.92 143.21 41 "Obihiro"
    , Station 47423 42.32 140.97 41 "Muroran"
    , Station 47570 37.49 139.91 42 "Wakamatsu"
    , Station 47585 39.65 141.96 42 "Miyako"
    , Station 47587 38.91 139.84 42 "Sakata"
    , Station 47590 38.26 140.90 42 "Sendai"
    , Station 47612 37.11 138.25 44 "Takada"
    , Station 47616 36.06 136.22 45 "Fukui"
    , Station 47626 36.15 139.38 43 "Kumagaya"
    , Station 47629 36.38 140.47 43 "Mito"
    , Station 47636 35.17 136.96 45 "Nagoya"
    , Station 47640 35.50 138.76 44 "Kawaguchiko"
    , Station 47656 34.98 138.40 44 "Shizuoka"
    , Station 47663 34.07 136.19 45 "Owase"
    , Station 47674 35.15 140.31 43 "Katsuura"
    , Station 47678 33.12 139.78 49 "Hachijojima"
    , Station 47746 35.53 134.20 49 "Tottori"
    , Station 47755 34.90 132.07 46 "Hamada"
    , Station 47795 33.89 135.13 49 "Mihama"
    , Station 47800 34.15 129.22 48 "Izuhara"
    , Station 47805 33.36 129.55 48 "Hirado"
    , Station 47815 33.24 131.62 47 "Oita"
    , Station 47819 32.81 130.71 47 "Kumamoto"
    , Station 47822 32.58 131.66 47 "Nobeoka"
    , Station 47836 30.38 130.66 48 "Yakushima"
    , Station 47848 31.71 130.32 50 "Ichiki"
    , Station 47891 34.32 134.05 46 "Takamatsu"
    , Station 47893 33.57 133.55 46 "Kouchi"
    , Station 47898 32.72 133.01 46 "Shimizu"
    , Station 47909 28.38 129.50 50 "Naze"
    , Station 47912 24.47 123.01 48 "Yonagunijima"
    , Station 47945 25.83 131.23 50 "Minamidaitojima"
    ]
