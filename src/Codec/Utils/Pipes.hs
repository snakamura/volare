module Codec.Utils.Pipes (makeParser) where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import Data.Functor ((<$>))
import Data.Maybe.HT (toMaybe)
import Pipes.Attoparsec
    ( isEndOfParserInput
    , parse
    )
import qualified Pipes.Parse as P


-- $setup
-- >>> import Control.Monad.State.Strict (evalStateT)
-- >>> import Data.Attoparsec.ByteString.Char8 (string)
-- >>> import Pipes (each)


-- |
-- >>> evalStateT (makeParser (string "abcdef")) (each ["ab", "cdef"])
-- Just "abcdef"
-- >>> evalStateT (makeParser (string "abcdef")) (each ["a", "bc"])
-- Nothing
-- >>> evalStateT (makeParser (string "abcdef")) (each ["abcdefg"])
-- Nothing
makeParser :: (Functor m, Monad m) =>
              A.Parser a ->
              P.Parser B.ByteString m (Maybe a)
makeParser parser = do
    p <- parse parser
    case p of
        Just (Right p') -> flip toMaybe p' <$> isEndOfParserInput
        _ -> return Nothing
