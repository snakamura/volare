module Codec.IGC
    ( parser
    , module Codec.IGC.Parser
    , module Codec.IGC.Types
    , module Codec.IGC.Utils
    ) where

import qualified Data.ByteString as BL
import Data.Maybe.HT (toMaybe)
import Pipes.Attoparsec
    ( isEndOfParserInput
    , parse
    )
import Pipes.Parse (Parser)

import Codec.IGC.Parser
import Codec.IGC.Types
import Codec.IGC.Utils


parser :: Monad m =>
          Parser BL.ByteString m (Maybe IGC)
parser = do
    r <- parse igc
    case r of
        Just (Right w) -> do
            b <- isEndOfParserInput
            return $ toMaybe b w
        _ -> return Nothing
