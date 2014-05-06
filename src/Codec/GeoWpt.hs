module Codec.GeoWpt
    ( parser
    , module Codec.GeoWpt.Parser
    , module Codec.GeoWpt.Types
    ) where

import qualified Data.ByteString as BL
import qualified Pipes.Parse as P

import Codec.GeoWpt.Parser
import Codec.GeoWpt.Types
import Codec.Utils.Pipes (makeParser)


parser :: (Functor m, Monad m) =>
          P.Parser BL.ByteString m (Maybe Wpt)
parser = makeParser wpt
