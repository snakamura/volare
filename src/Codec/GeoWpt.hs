module Codec.GeoWpt
    ( parser
    , module Codec.GeoWpt.Parser
    , module Codec.GeoWpt.Types
    ) where

import qualified Data.ByteString as BL
import Pipes.Parse (Parser)

import Codec.GeoWpt.Parser
import Codec.GeoWpt.Types
import Codec.Utils.Pipes (parse)


parser :: Monad m =>
          Parser BL.ByteString m (Maybe Wpt)
parser = parse wpt
