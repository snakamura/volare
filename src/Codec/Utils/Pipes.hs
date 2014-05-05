module Codec.Utils.Pipes (parse) where

import qualified Data.Attoparsec as Attoparsec
import qualified Data.ByteString as BL
import Data.Maybe.HT (toMaybe)
import qualified Pipes.Attoparsec as PipesA
import Pipes.Parse (Parser)


parse :: Monad m =>
         Attoparsec.Parser a ->
         Parser BL.ByteString m (Maybe a)
parse parser = do
    p <- PipesA.parse parser
    case p of
        Just (Right p') -> do
            b <- PipesA.isEndOfParserInput
            return $ toMaybe b p'
        _ -> return Nothing
