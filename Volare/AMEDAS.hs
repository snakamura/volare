module Volare.AMEDAS (
    Type.Item(..),
    Type.WindDirection(..),
    Type.Station(..),
    download,
    save,
    load,
    parseHtml,
    stations
) where

import Control.Applicative ((<$>),
                            (<*>))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List.Split (splitOn,
                        splitWhen)
import Data.Maybe (catMaybes)
import qualified Network.HTTP.Conduit as Http
import Text.HTML.TagSoup (Tag(TagOpen, TagClose),
                          (~==),
                          (~/=),
                          fromTagText,
                          isTagText,
                          parseTags)
import Text.Printf (printf)
import Text.Read (readMaybe)

import qualified Volare.AMEDAS.Type as Type
import Volare.AMEDAS.Stations (stations)


download :: Type.Station ->
            Int ->
            Int ->
            Int ->
            IO [Type.Item]
download station year month day = do
  let c = if Type.block station >= 10000 then 's' else 'a'
      url = printf "http://www.data.jma.go.jp/obd/stats/etrn/view/10min_%c1.php?prec_no=%d&block_no=%04d&year=%d&month=%d&day=%d&view=p1" c (Type.prec station) (Type.block station) year month day
  parseHtml <$> Http.simpleHttp url


save :: FilePath ->
        [Type.Item] ->
        IO ()
save path = writeFile path . unlines . map formatItem
    where
      formatItem (Type.Item time precipitation temperature windSpeed windDirection sunshine) =
          show time ++ "," ++
          maybe "" show precipitation ++ "," ++
          maybe "" show temperature ++ "," ++
          maybe "" show windSpeed ++ "," ++
          maybe "" show windDirection ++ "," ++
          maybe "" show sunshine


load :: FilePath ->
        IO [Type.Item]
load path = (map parseItem . lines) <$> readFile path
    where
      parseItem = make . splitOn ","
      make [time, precipitation, temperature, windSpeed, windDirection, sunshine] =
          Type.Item (read time)
                    (readMaybe precipitation)
                    (readMaybe temperature)
                    (readMaybe windSpeed)
                    (readMaybe windDirection)
                    (readMaybe sunshine)
      make _ = error "Never happens."


parseHtml :: BL.ByteString ->
             [Type.Item]
parseHtml = catMaybes . map parseItem
                      . splitWhen (~== TagClose tr)
                      . takeWhile (~/= TagClose table)
                      . dropWhile (~/= TagOpen tr [("class", "mtx"), ("style", "")])
                      . parseTags
    where
      parseItem = makeItem . map fromTagText . filter isTagText . dropWhile (~/= TagOpen tr [])


makeItem :: [BL.ByteString] ->
            Maybe Type.Item
makeItem [time, precipitation, temperature, windSpeed, windDirection, maxWindSpeed, maxWindDirection] =
    makeItem [time, precipitation, temperature, windSpeed, windDirection, maxWindSpeed, maxWindDirection, ""]
makeItem [time, precipitation, temperature, windSpeed, windDirection, maxWindSpeed, maxWindDirection, sunshine] =
    makeItem [time, "", "", precipitation, temperature, "", windSpeed, windDirection, maxWindSpeed, maxWindDirection, sunshine]
makeItem [time, surfacePressure, seaLevelPressure, precipitation, temperature, relativeHumidity, windSpeed, windDirection, maxWindSpeed, maxWindDirection] =
    makeItem [time, surfacePressure, seaLevelPressure, precipitation, temperature, relativeHumidity, windSpeed, windDirection, maxWindSpeed, maxWindDirection, ""]
makeItem [time, _, _, precipitation, temperature, _, windSpeed, windDirection, _, _, sunshine] = do
  time' <- parseTime $ BLU.toString time
  return $ Type.Item time'
                     (readMaybe $ BLU.toString precipitation)
                     (readMaybe $ BLU.toString temperature)
                     (readMaybe $ BLU.toString windSpeed)
                     (parseWindDirection $ BLU.toString windDirection)
                     (readMaybe $ BLU.toString sunshine)
makeItem _ = Nothing


parseTime :: String ->
             Maybe Int
parseTime s = case break (== ':') s of
                (_, "") -> Nothing
                (h, _:m) -> ((+) . (* 60)) <$> readMaybe h <*> readMaybe m


parseWindDirection :: String ->
                      Maybe Type.WindDirection
parseWindDirection "北" = Just Type.N
parseWindDirection "北北東" = Just Type.NNE
parseWindDirection "北東" = Just Type.NE
parseWindDirection "東北東" = Just Type.ENE
parseWindDirection "東" = Just Type.E
parseWindDirection "東南東" = Just Type.ESE
parseWindDirection "南東" = Just Type.SE
parseWindDirection "南南東" = Just Type.SSE
parseWindDirection "南" = Just Type.S
parseWindDirection "南南西" = Just Type.SSW
parseWindDirection "南西" = Just Type.SW
parseWindDirection "西南西" = Just Type.WSW
parseWindDirection "西" = Just Type.W
parseWindDirection "西北西" = Just Type.WNW
parseWindDirection "北西" = Just Type.NW
parseWindDirection "北北西" = Just Type.NNW
parseWindDirection "静穏" = Just Type.CALM
parseWindDirection _ = Nothing

table, tr :: BL.ByteString
table = "table"
tr = "tr"
