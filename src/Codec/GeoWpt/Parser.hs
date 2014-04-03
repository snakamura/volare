module Codec.GeoWpt.Parser (
    wpt
) where

import Control.Applicative ((<*>),
                            (*>),
                            (<*),
                            (<$>),
                            (<|>),
                            many)
import Data.Attoparsec (Parser,
                        string,
                        skipWhile,
                        take,
                        takeWhile)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Prelude hiding (take,
                       takeWhile)

import Codec.GeoWpt.Types (Item(Item),
                           Wpt(Wpt))
import Codec.Utils (char,
                    digit,
                    digits,
                    many1,
                    newline)


wpt :: Parser Wpt
wpt = header *> (Wpt <$> many item)


header :: Parser ()
header = string "$FormatGEO" *> newline


item :: Parser Item
item = Item <$> (name <* many1 (char ' ')) <*> (latitude <* many1 (char ' ')) <*> (longitude <* many1 (char ' ')) <*> (altitude <* many1 (char ' ')) <*> description <* newline


name :: Parser T.Text
name = (T.strip . T.decodeUtf8) <$> take 9


latitude :: Parser Float
latitude = toDegree <$> ((char 'N' <|> char 'S') <* char ' ') <*> (digits 2 <* char ' ') <*> (digits 2 <* char ' ') <*> (digits 2 <* char '.') <*> digits 2


longitude :: Parser Float
longitude = toDegree <$> ((char 'W' <|> char 'E') <* char ' ') <*> (digits 3 <* char ' ') <*> (digits 2 <* char ' ') <*> (digits 2 <* char '.') <*> digits 2


altitude :: Parser Int
altitude = toAltitude <$> (skipWhile (== 0x20) *> many digit)
    where
      toAltitude ds = sum $ zipWith (\d e -> d * 10 ^ e) (reverse ds) [(0 :: Int) ..]


description :: Parser T.Text
description = (T.strip . T.decodeUtf8) <$> takeWhile (\b -> b /= 0x0d && b /= 0x0a)


toDegree :: Char ->
            Int ->
            Int ->
            Int ->
            Int ->
            Float
toDegree u d m s ss = unit u * (fromIntegral d + (fromIntegral m / 60) + fromIntegral (s * 100 + ss) / 360000)
    where
      unit 'N' =  1
      unit 'S' = -1
      unit 'W' = -1
      unit 'E' =  1
      unit _   = undefined
