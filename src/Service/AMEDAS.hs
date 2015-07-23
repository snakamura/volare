module Service.AMEDAS
    ( Types.Item(..)
    , Types.WindDirection(..)
    , Types.Station(..)
    , download
    , save
    , load
    , parseHtml
    , Stations.station
    , Stations.stations
    ) where

import Control.Applicative
    ( many
    , optional
    )
import Control.Exception (throwIO)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Control.Monad.Trans.Class (lift)
import Data.Attoparsec.ByteString.Char8
    ( char
    , digit
    , notChar
    , rational
    )
import Data.Attoparsec.Combinator (many1)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.UTF8 as BU
import Data.Foldable (for_)
import Data.List.Split (splitWhen)
import Data.Maybe (mapMaybe)
import qualified Data.Text.Lazy as TL
import Formatting ((%))
import qualified Formatting as F
import qualified Network.HTTP.Client as Http
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import Pipes.Attoparsec (parsed)
import Safe (headDef)
import Text.HTML.TagSoup
    ( Tag(TagOpen, TagClose)
    , (~==)
    , (~/=)
    , fromTagText
    , isTagText
    , parseTags
    )
import Text.Read (readMaybe)

import qualified Service.AMEDAS.Types as Types
import qualified Service.AMEDAS.Stations as Stations


download :: (MonadIO m, MonadThrow m) =>
            Types.Station ->
            Int ->
            Int ->
            Int ->
            Http.Manager ->
            P.Producer Types.Item m ()
download station year month day manager = do
    let c = if Types.block station >= 10000 then 's' else 'a'
        url = F.format ("http://www.data.jma.go.jp/obd/stats/etrn/view/10min_" % F.char % "1.php?prec_no=" % F.int % "&block_no=" % F.left 4 '0' % "&year=" % F.int % "&month=" % F.int % "&day=" % F.int % "&view=p1") c (Types.prec station) (Types.block station) year month day
    req <- lift $ Http.parseUrl $ TL.unpack url
    items <- liftIO $ (parseHtml . Http.responseBody) <$> Http.httpLbs req manager
    P.each items


save :: MonadIO m =>
        P.Consumer B.ByteString m () ->
        P.Consumer Types.Item m ()
save consumer = serialize >-> P.map BU.fromString >-> consumer


serialize :: Monad m =>
             P.Pipe Types.Item String m ()
serialize = do
    Types.Item time precipitation temperature windSpeed windDirection sunshine <- P.await
    P.yield $ show time
    P.yield ","
    for_ precipitation $ P.yield . show
    P.yield ","
    for_ temperature $ P.yield . show
    P.yield ","
    for_ windSpeed $ P.yield . show
    P.yield ","
    for_ windDirection $ P.yield . show
    P.yield ","
    for_ sunshine $ P.yield . show
    P.yield "\n"
    serialize


load :: MonadIO m =>
        P.Producer B.ByteString m () ->
        P.Producer Types.Item m ()
load producer = do
    r <- parsed item producer
    case r of
      Left (e, _) -> liftIO $ throwIO e
      Right () -> return ()
  where
    item = Types.Item <$> ((read <$> many1 digit) <* char ',')
                      <*> (optional rational <* char ',')
                      <*> (optional rational <* char ',')
                      <*> (optional rational <* char ',')
                      <*> ((readMaybe <$> many (notChar ',')) <* char ',')
                      <*> (optional rational <* char '\n')

parseHtml :: BL.ByteString ->
             [Types.Item]
parseHtml = mapMaybe parseItem
                . tail
                . splitWhen (~== TagOpen tr [])
                . takeWhile (~/= TagClose table)
                . dropWhile (~/= TagOpen tr [("class", "mtx"), ("style", "")])
                . parseTags
  where
    parseItem = makeItem . map (headDef "" . map fromTagText . filter isTagText . takeWhile (~/= TagClose td)) . tail . splitWhen (~== TagOpen td [])


makeItem :: [BL.ByteString] ->
            Maybe Types.Item
makeItem [time, precipitation, temperature, windSpeed, windDirection, maxWindSpeed, maxWindDirection, sunshine] =
    makeItem [time, "", "", precipitation, temperature, "", windSpeed, windDirection, maxWindSpeed, maxWindDirection, sunshine]
makeItem [time, _, _, precipitation, temperature, _, windSpeed, windDirection, _, _, sunshine] = do
    time' <- parseTime $ BLU.toString time
    return $ Types.Item time'
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
                      Maybe Types.WindDirection
parseWindDirection "北" = Just Types.N
parseWindDirection "北北東" = Just Types.NNE
parseWindDirection "北東" = Just Types.NE
parseWindDirection "東北東" = Just Types.ENE
parseWindDirection "東" = Just Types.E
parseWindDirection "東南東" = Just Types.ESE
parseWindDirection "南東" = Just Types.SE
parseWindDirection "南南東" = Just Types.SSE
parseWindDirection "南" = Just Types.S
parseWindDirection "南南西" = Just Types.SSW
parseWindDirection "南西" = Just Types.SW
parseWindDirection "西南西" = Just Types.WSW
parseWindDirection "西" = Just Types.W
parseWindDirection "西北西" = Just Types.WNW
parseWindDirection "北西" = Just Types.NW
parseWindDirection "北北西" = Just Types.NNW
parseWindDirection "静穏" = Just Types.CALM
parseWindDirection _ = Nothing

table, tr, td :: BL.ByteString
table = "table"
tr = "tr"
td = "td"
