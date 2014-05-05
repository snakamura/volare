module Codec.GeoWpt
    ( parser
    , module Codec.GeoWpt.Parser
    , module Codec.GeoWpt.Types
    ) where

import qualified Data.ByteString as BL
import Data.Maybe.HT (toMaybe)
import Pipes.Attoparsec
    ( isEndOfParserInput
    , parse
    )
import Pipes.Parse (Parser)

import Codec.GeoWpt.Parser
import Codec.GeoWpt.Types


parser :: Monad m =>
          Parser BL.ByteString m (Maybe Wpt)
parser = do
    r <- parse wpt
    case r of
        Just (Right w) -> do
            b <- isEndOfParserInput
            return $ toMaybe b w
        _ -> return Nothing
