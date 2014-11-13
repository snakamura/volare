module Codec.IGC
    ( parser
    , module Codec.IGC.Parser
    , module Codec.IGC.Types
    , module Codec.IGC.Utils
    ) where

import qualified Data.ByteString as B
import qualified Pipes.Parse as P

import Codec.IGC.Parser
import Codec.IGC.Types
import Codec.IGC.Utils
import Codec.Utils.Pipes (makeParser)


parser :: (Functor m, Monad m) =>
          P.Parser B.ByteString m (Maybe IGC)
parser = makeParser igc
