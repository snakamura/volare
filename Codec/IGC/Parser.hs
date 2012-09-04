{-# LANGUAGE NoMonomorphismRestriction #-}

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
import qualified Data.Text as T
import Data.Time (DiffTime)
import Data.Attoparsec (Parser,
                        inClass,
                        satisfy,
                        skipWhile,
                        word8)

import Codec.IGC.Types (IGC(IGC),
                        Position(Position),
                        Record(Record))


igc :: T.Text ->
       Parser IGC
igc name = IGC name <$> (a *> (catMaybes <$> many record) <* g)


record :: Parser (Maybe Record)
record = b <|> other


a :: Parser ()
a = char 'A' *> line


b :: Parser (Maybe Record)
b = Just <$> (char 'B' *> (Record <$> time <*> position) <* line)


g :: Parser ()
g = char 'G' *> line


position :: Parser Position
position = Position <$> latitude <*> longitude <*> altitude


time :: Parser DiffTime
time = makeDiffTime <$> hour <*> minute <*> second
    where
      makeDiffTime h m s = toEnum $ h * 60 * 60 + m * 60 + s


hour :: Parser Int
hour = digits 2


minute :: Parser Int
minute = digits 2


second :: Parser Int
second = digits 2


latitude :: Parser Float
latitude = toDegree <$> digits 2 <*> digits 2 <*> digits 3 <*> (char 'N' <|> char 'S')


longitude :: Parser Float
longitude = toDegree <$> digits 3 <*> digits 2 <*> digits 3 <*> (char 'W' <|> char 'E')


altitude :: Parser Float
altitude = (char 'A' <|> char 'V') *> pressure *> gnss
    where
      pressure = digits 5 <|> (char '-' *> (negate <$> digits 4))
      gnss = fromIntegral <$> digits 5


other :: Parser (Maybe Record)
other = const Nothing <$> (satisfy (inClass "CDEFHIJLMNOPQR") *> line)


line :: Parser ()
line = const (const (const ()))  <$> skipWhile (/= 0x0d) <*> word8 0x0d <*> word8 0x0a


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
            Int ->
            Char ->
            Float
toDegree d m s u = unit u * (fromIntegral d + (fromIntegral m / 60) + (fromIntegral s / 10 / 3600))
    where
      unit 'N' =  1
      unit 'S' = -1
      unit 'W' = -1
      unit 'E' =  1
      unit _   = undefined
