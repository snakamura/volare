module Codec.IGC.Utils
    ( distance
    ) where

import Codec.IGC.Types
    ( Position
    , latitude
    , longitude
    )

-- $setup
-- >>> import Codec.IGC.Types (Position(Position))


-- |
-- Distance between two positions.
--
-- >>> round $ distance (Position 35.685162 139.752788 100) (Position 36.309610 140.178402 50) :: Int
-- 79381
distance :: Position ->
            Position ->
            Double
distance position1 position2 =
    let r = 6378137
        dx = (realToFrac (longitude position1) - realToFrac (longitude position2)) / 180 * pi
        y1 = realToFrac (latitude position1) / 180 * pi
        y2 = realToFrac (latitude position2) / 180 * pi
    in r * acos (sin y1 * sin y2 + cos y1 * cos y2 * cos dx)
