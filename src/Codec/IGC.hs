module Codec.IGC
    ( parser
    , module Codec.IGC.Parser
    , module Codec.IGC.Types
    , module Codec.IGC.Utils
    ) where

import qualified Data.ByteString as BL
import Pipes.Parse (Parser)

import Codec.IGC.Parser
import Codec.IGC.Types
import Codec.IGC.Utils
import Codec.Utils.Pipes (parse)


parser :: Monad m =>
          Parser BL.ByteString m (Maybe IGC)
parser = parse igc
