module Service.MSM.Barometric
   ( Item(..)
   ) where

import Data.Aeson.TH
    ( defaultOptions
    , deriveJSON)
import Foreign.C
    ( CFloat
    , CInt)
import Foreign.Storable (Storable(..))

#include "../../../msm/msm.h"


data Item = Item
    { latitude                        :: Float
    , longitude                       :: Float
    , airPressure                     :: Int
    , geopotentialHeight              :: Float
    , lagrangianTendencyOfAirPressure :: Float
    , eastwardWind                    :: Float
    , northwardWind                   :: Float
    , airTemperature                  :: Float
    , relativeHumidity                :: Int
    } deriving Show

instance Storable Item where
    sizeOf _ = #{size barometric_item}
    alignment _ = alignment (undefined :: CFloat)
    peek p = Item <$> (#{peek barometric_item, latitude} p)
                  <*> (#{peek barometric_item, longitude} p)
                  <*> fmap cIntToInt (#{peek barometric_item, air_pressure} p)
                  <*> (#{peek barometric_item, geopotential_height} p)
                  <*> (#{peek barometric_item, lagrangian_tendency_of_air_pressure} p)
                  <*> (#{peek barometric_item, eastward_wind} p)
                  <*> (#{peek barometric_item, northward_wind} p)
                  <*> (#{peek barometric_item, air_temperature} p)
                  <*> fmap cIntToInt (#{peek barometric_item, relative_humidity} p)
      where
        cIntToInt :: CInt -> Int
        cIntToInt = fromIntegral
    poke p s = do
      (#{poke barometric_item, latitude} p $ latitude s)
      (#{poke barometric_item, longitude} p $ longitude s)
      (#{poke barometric_item, air_pressure} p $ airPressure s)
      (#{poke barometric_item, geopotential_height} p $ geopotentialHeight s)
      (#{poke barometric_item, lagrangian_tendency_of_air_pressure} p $ lagrangianTendencyOfAirPressure s)
      (#{poke barometric_item, eastward_wind} p $ eastwardWind s)
      (#{poke barometric_item, northward_wind} p $ northwardWind s)
      (#{poke barometric_item, air_temperature} p $ airTemperature s)
      (#{poke barometric_item, relative_humidity} p $ relativeHumidity s)

deriveJSON defaultOptions ''Item
