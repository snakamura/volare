{-# LANGUAGE NoMonomorphismRestriction,
             OverloadedStrings #-}

module Codec.IGC.Parser (
    igc
) where

import Control.Applicative ((<*>),
                            (*>),
                            (<*),
                            (<$>),
                            (<|>),
                            many,
                            pure)
import Data.Maybe (catMaybes)
import Data.Time (Day,
                  DiffTime,
                  fromGregorian,
                  secondsToDiffTime)
import Data.Attoparsec (Parser,
                        inClass,
                        option,
                        satisfy,
                        skipWhile,
                        string,
                        word8)

import Codec.IGC.Types (IGC(IGC),
                        Position(Position),
                        Record(Record))


igc :: Parser IGC
igc = a *> (IGC <$> headers <*> (catMaybes <$> many record)) <* g


headers :: Parser Day
headers = hfdte <* many h


record :: Parser (Maybe Record)
record = b <|> other


a :: Parser ()
a = char 'A' *> line


b :: Parser (Maybe Record)
b = Just <$> (char 'B' *> (Record <$> time <*> position) <* line)


g :: Parser ()
g = char 'G' *> line


h :: Parser ()
h = char 'H' *> line


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
altitude = (char 'A' <|> char 'V') *> pressure *> gnss
    where
      pressure = digits 5 <|> (char '-' *> (negate <$> digits 4))
      gnss = fromIntegral <$> digits 5


other :: Parser (Maybe Record)
other = const Nothing <$> (satisfy (inClass "CDEFHIJLMNOPQR") *> line)


line :: Parser ()
line = skipWhile (\b -> b /= 0x0d && b /= 0x0a) *> newline


newline :: Parser ()
newline = const (const ()) <$> (option 0x0d $ word8 0x0d) <*> word8 0x0a


char :: Char ->
        Parser Char
char c = const c <$> word8 (fromIntegral (fromEnum c))


digits :: Int ->
          Parser Int
digits n = digits' n 0


digits' :: Int ->
           Int ->
           Parser Int
digits' 0 r = pure r
digits' n r = do
  m <- digit
  digits' (n - 1) (r * 10 + m)


digit :: Parser Int
digit = toInt <$> satisfy (inClass "0-9")
    where
      toInt n = fromEnum n - fromEnum '0'


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
