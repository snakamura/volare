module Service.AMEDAS.Stations (
    stations
) where

import Service.AMEDAS.Type (Station(..))


stations :: (Float, Float) ->
            (Float, Float) ->
            [Station]
stations (nwLat, nwLng) (seLat, seLng) = filter f allStations
    where
      f (Station _ _ lat lng _) = seLat <= lat && lat <= nwLat &&
                                  nwLng <= lng && lng <= seLng


allStations :: [Station]
allStations = [
    Station 36  0281 (c 37 51.1) (c 140 35.3) "Yanagawa",
    Station 36  0285 (c 37 47.0) (c 140 55.5) "Souma",
    Station 36  0286 (c 37 39.5) (c 139 51.8) "Kitakata",
    Station 36  0290 (c 37 33.3) (c 140 07.3) "Inawashiro",
    Station 36  0291 (c 37 35.0) (c 140 25.8) "Nihonmatsu",
    Station 36  0294 (c 37 26.1) (c 140 34.6) "Funehiki",
    Station 36  0295 (c 37 29.5) (c 140 57.9) "Namie",
    Station 36  0297 (c 37 20.6) (c 139 18.8) "Tadami",
    Station 36  0299 (c 37 22.1) (c 140 19.8) "Koriyama",
    Station 36  0301 (c 37 15.9) (c 139 32.2) "Nangou",
    Station 36  0303 (c 37 12.4) (c 139 47.7) "Tajima",
    Station 36  0304 (c 37 17.2) (c 140 37.5) "Ononiimachi",
    Station 36  0307 (c 37 08.8) (c 140 27.6) "Ishikawa",
    Station 36  0312 (c 36 56.3) (c 140 24.5) "Higashishirakawa",
    Station 36  1031 (c 37 35.3) (c 139 39.4) "Nishiaizu",
    Station 36  1034 (c 37 14.0) (c 141 00.0) "Hirono",
    Station 36  1044 (c 37 28.4) (c 139 31.7) "Kaneyama",
    Station 36  1116 (c 37 40.1) (c 140 15.6) "Washikura",
    Station 36  1129 (c 37 20.2) (c 140 48.5) "Kawauchi",
    Station 36  1130 (c 37 41.7) (c 140 44.8) "Iitate",
    Station 36  1282 (c 37 43.3) (c 140 03.5) "Hibara",
    Station 36  1293 (c 37 53.5) (c 140 26.2) "Moniwa",
    Station 36  1294 (c 37 16.6) (c 140 03.8) "Yumoto",
    Station 36  1295 (c 37 01.4) (c 139 23.2) "Hinoemata",
    Station 36  1466 (c 37 13.6) (c 140 25.6) "Tamakawa",
    Station 36  1607 (c 36 56.0) (c 140 44.0) "Yamada",
    Station 36  1633 (c 37 52.5) (c 140 55.1) "Shinchi",
    Station 36  1634 (c 37 05.4) (c 140 33.6) "Furudono",
    Station 36 47570 (c 37 29.3) (c 139 54.6) "Wakamatsu",
    Station 36 47595 (c 37 45.5) (c 140 28.2) "Fukushima",
    Station 36 47597 (c 37 07.9) (c 140 12.9) "Shirakawa",
    Station 36 47598 (c 36 56.8) (c 140 54.2) "Onahama",

    Station 40  0315 (c 36 50.0) (c 140 46.3) "Kitaibaraki",
    Station 40  0316 (c 36 46.7) (c 140 20.7) "Daigo",
    Station 40  0318 (c 36 23.0) (c 140 14.2) "Kasama",
    Station 40  0320 (c 36 12.1) (c 139 43.0) "Koga",
    Station 40  0322 (c 36 10.1) (c 139 56.7) "Shimotsuma",
    Station 40  0324 (c 36 06.2) (c 140 13.2) "Tsuchiura",
    Station 40  0325 (c 35 57.8) (c 140 37.3) "Kashima",
    Station 40  1011 (c 36 34.8) (c 140 38.7) "Hitachi",
    Station 40  1014 (c 35 53.4) (c 140 12.7) "Ryugasaki",
    Station 40  1245 (c 36 10.1) (c 140 31.6) "Hokota",
    Station 40  1331 (c 36 36.4) (c 140 19.5) "Hitachioomiya",
    Station 40  1530 (c 36 16.9) (c 139 59.3) "Shimodate",
    Station 40  1635 (c 36 42.2) (c 140 43.0) "Takahagi",
    Station 40 47629 (c 36 22.8) (c 140 28.0) "Mito",
    Station 40 47646 (c 36 03.4) (c 140 07.5) "Tsukuba",

    Station 41  0326 (c 37 07.4) (c 140 02.1) "Nasu",
    Station 41  0329 (c 36 58.9) (c 140 01.1) "Kuroiso",
    Station 41  0331 (c 36 50.4) (c 140 02.1) "Ootawara",
    Station 41  0335 (c 36 35.5) (c 139 44.1) "Kanuma",
    Station 41  0338 (c 36 28.6) (c 139 59.2) "Mooka",
    Station 41  0341 (c 36 20.3) (c 139 49.8) "Oyama",
    Station 41  1015 (c 36 55.3) (c 139 41.7) "Ikari",
    Station 41  1018 (c 36 21.8) (c 139 34.2) "Sano",
    Station 41  1221 (c 36 53.5) (c 139 34.1) "Dorobu",
    Station 41  1333 (c 36 43.6) (c 139 40.6) "Imaichi",
    Station 41  1334 (c 36 45.4) (c 139 54.0) "Shioya",
    Station 41  1605 (c 36 38.5) (c 140 07.0) "Nasukarasuyama",
    Station 41 47615 (c 36 32.9) (c 139 52.1) "Utsunomiya",
    Station 41 47690 (c 36 44.3) (c 139 30.0) "Okunikko",

    Station 62  0600 (c 34 56.9) (c 135 27.3) "Nose",
    Station 62  0602 (c 34 47.0) (c 135 26.2) "Toyonaka",
    Station 62  0604 (c 34 40.5) (c 135 40.6) "Ikomayama",
    Station 62  0606 (c 34 23.1) (c 135 21.0) "Kumatori",
    Station 62  1062 (c 34 33.3) (c 135 29.1) "Sakai",
    Station 62  1065 (c 34 48.5) (c 135 40.3) "Hirakata",
    Station 62  1470 (c 34 35.8) (c 135 36.0) "Yao",
    Station 62  1471 (c 34 26.0) (c 135 13.9) "Kankujima",
    Station 62 47772 (c 34 40.9) (c 135 31.1) "Osaka",

    Station 64  0630 (c 34 36.3) (c 135 57.2) "Hari",
    Station 64  0633 (c 34 29.3) (c 135 55.9) "Oouda",
    Station 64  0635 (c 34 22.8) (c 135 43.8) "Gojo",
    Station 64  0957 (c 34 08.2) (c 136 00.3) "Kamikitayama",
    Station 64  1228 (c 34 02.7) (c 135 47.2) "Kazeya",
    Station 64 47780 (c 34 41.6) (c 135 49.6) "Nara",

    Station 65  0645 (c 34 13.3) (c 135 35.4) "Kouyasan",
    Station 65  0649 (c 33 41.2) (c 135 58.2) "Shingu",
    Station 65  0971 (c 33 47.5) (c 135 30.8) "Kurisugawa",
    Station 65  1063 (c 34 05.2) (c 135 25.5) "Shimizu",
    Station 65  1089 (c 33 56.7) (c 135 33.4) "Ryuujin",
    Station 65  1342 (c 34 18.6) (c 135 31.7) "Katsuragi",
    Station 65  1347 (c 33 38.3) (c 135 42.6) "Nishikawa",
    Station 65  1457 (c 34 16.8) (c 135 00.0) "Tomogashima",
    Station 65  1485 (c 33 53.6) (c 135 13.0) "Kawabe",
    Station 65  1589 (c 33 39.7) (c 135 21.8) "Nankishirahama",
    Station 65 47777 (c 34 13.7) (c 135 09.8) "Wakayama",
    Station 65 47778 (c 33 27.0) (c 135 45.4) "Shionomisaki"
  ]


c :: Int ->
     Float ->
     Float
c d m = fromIntegral d + m / 60
