module Codec.Utils (
    many1,
    line,
    newline,
    char,
    digit,
    digits
) where

import Control.Applicative ((<*>),
                            (*>),
                            (<$>),
                            many,
                            pure)
import Data.Attoparsec (Parser,
                        inClass,
                        option,
                        satisfy,
                        skipWhile,
                        word8)


many1 :: Parser a ->
         Parser [a]
many1 parser = (:) <$> parser <*> many parser


line :: Parser ()
line = skipWhile (\b -> b /= 0x0d && b /= 0x0a) *> newline


newline :: Parser ()
newline = const (const ()) <$> (option 0x0d $ word8 0x0d) <*> word8 0x0a


char :: Char ->
        Parser Char
char c = const c <$> word8 (fromIntegral (fromEnum c))


digit :: Parser Int
digit = toInt <$> satisfy (inClass "0-9")
    where
      toInt n = fromEnum n - fromEnum '0'


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
