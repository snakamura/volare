module Service.AMEDAS
    ( Type.Item(..)
    , Type.WindDirection(..)
    , Type.Station(..)
    , download
    , save
    , load
    , parseHtml
    , stations
    , allStations
) where

import Control.Applicative
    ( (<$>)
    , (<*>)
    )
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List.Split
    ( splitOn
    , splitWhen)
import Data.Maybe (mapMaybe)
import qualified Network.HTTP.Conduit as Http
import Pipes
    ( Consumer
    , Producer
    , (>->))
import qualified Pipes.Prelude as Pipes
import Safe (headDef)
import System.IO (Handle)
import Text.HTML.TagSoup
    ( Tag(TagOpen, TagClose)
    , (~==)
    , (~/=)
    , fromTagText
    , isTagText
    , parseTags
    )
import Text.Printf (printf)
import Text.Read (readMaybe)

import qualified Service.AMEDAS.Type as Type
import Service.AMEDAS.Stations
    ( allStations
    , stations
    )


download :: Type.Station ->
            Int ->
            Int ->
            Int ->
            IO [Type.Item]
download station year month day = do
    let c = if Type.block station >= 10000 then 's' else 'a'
        url = printf "http://www.data.jma.go.jp/obd/stats/etrn/view/10min_%c1.php?prec_no=%d&block_no=%04d&year=%d&month=%d&day=%d&view=p1" c (Type.prec station) (Type.block station) year month day
    parseHtml <$> Http.simpleHttp url


save :: MonadIO m =>
        Handle ->
        Consumer Type.Item m ()
save handle = Pipes.map formatItem >-> Pipes.toHandle handle
  where
    formatItem (Type.Item time precipitation temperature windSpeed windDirection sunshine) =
        show time ++ "," ++
        maybe "" show precipitation ++ "," ++
        maybe "" show temperature ++ "," ++
        maybe "" show windSpeed ++ "," ++
        maybe "" show windDirection ++ "," ++
        maybe "" show sunshine


load :: MonadIO m =>
        Handle ->
        Producer Type.Item m ()
load handle = Pipes.fromHandle handle >-> Pipes.map parseItem
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
parseHtml = mapMaybe parseItem
                . tail
                . splitWhen (~== TagOpen tr [])
                . takeWhile (~/= TagClose table)
                . dropWhile (~/= TagOpen tr [("class", "mtx"), ("style", "")])
                . parseTags
  where
    parseItem = makeItem . map (headDef "" . map fromTagText . filter isTagText . takeWhile (~/= TagClose td)) . tail . splitWhen (~== TagOpen td [])


makeItem :: [BL.ByteString] ->
            Maybe Type.Item
makeItem [time, precipitation, temperature, windSpeed, windDirection, maxWindSpeed, maxWindDirection, sunshine] =
    makeItem [time, "", "", precipitation, temperature, "", windSpeed, windDirection, maxWindSpeed, maxWindDirection, sunshine]
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

table, tr, td :: BL.ByteString
table = "table"
tr = "tr"
td = "td"
