module Codec.IGC.Parser
    ( igc
    ) where

import Control.Applicative
    ( (<*>)
    , (*>)
    , (<*)
    , (<$>)
    , (<|>)
    , many
    )
import Data.Maybe (catMaybes)
import Data.Time
    ( Day
    , DiffTime
    , fromGregorian
    , secondsToDiffTime
    )
import Data.Attoparsec.ByteString
    ( Parser
    , inClass
    , satisfy
    , string
    )

import Codec.IGC.Types
    ( IGC(IGC)
    , Position(Position)
    , Record(Record)
    )
import Codec.Utils.Attoparsec
    ( char
    , digits
    , line
    , newline
    )


igc :: Parser IGC
igc = recordA *> (IGC <$> headers <*> (catMaybes <$> many record)) <* many recordG


headers :: Parser Day
headers = hfdte <* many recordH


record :: Parser (Maybe Record)
record = recordB <|> other


recordA :: Parser ()
recordA = char 'A' *> line


recordB :: Parser (Maybe Record)
recordB = Just <$> (char 'B' *> (Record <$> time <*> position) <* line)


recordG :: Parser ()
recordG = char 'G' *> line


recordH :: Parser ()
recordH = char 'H' *> line


hfdte :: Parser Day
hfdte = string "HFDTE" *> (toDay <$> digits 2 <*> digits 2 <*> digits 2) <* newline
    where
      toDay d m y = fromGregorian (fromIntegral (2000 + y)) m d


position :: Parser Position
position = Position <$> latitude <*> longitude <*> altitude


time :: Parser DiffTime
time = toDiffTime <$> hour <*> minute <*> second
  where
    toDiffTime h m s = secondsToDiffTime $ fromIntegral $ h * 60 * 60 + m * 60 + s


hour :: Parser Int
hour = digits 2


minute :: Parser Int
minute = digits 2


second :: Parser Int
second = digits 2


latitude :: Parser Float
latitude = toDegree <$> digits 2 <*> digits 5 <*> (char 'N' <|> char 'S')


longitude :: Parser Float
longitude = toDegree <$> digits 3 <*> digits 5 <*> (char 'W' <|> char 'E')


altitude :: Parser Float
altitude = ((fromIntegral .) . select) <$> ((char 'A' <|> char 'V') *> pressure) <*> gnss
  where
    pressure = digits 5 <|> (char '-' *> (negate <$> digits 4))
    gnss = digits 5
    select :: Int -> Int -> Int
    select p 0 = p
    select _ g = g


other :: Parser (Maybe Record)
other = const Nothing <$> (satisfy (inClass "CDEFHIJLMNOPQR") *> line)


toDegree :: Int ->
            Int ->
            Char ->
            Float
toDegree d m u = unit u * (fromIntegral d + (fromIntegral m / 60000))
  where
    unit 'N' =  1
    unit 'S' = -1
    unit 'W' = -1
    unit 'E' =  1
    unit _   = undefined
