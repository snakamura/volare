module Volare.MSM.Pressure (
    Pressure(..)
) where

import Control.Applicative ((<$>),
                            (<*>))
import Data.Aeson.TH (deriveJSON)
import Foreign.C (CFloat,
                  CInt)
import Foreign.Storable (Storable(..))

#include "../../../msm/msm.h"


data Pressure = Pressure {
    latitude                        :: Float,
    longitude                       :: Float,
    airPressure                     :: Int,
    geopotentialHeight              :: Float,
    lagrangianTendencyOfAirPressure :: Float,
    eastwardWind                    :: Float,
    northwardWind                   :: Float,
    airTemperature                  :: Float,
    relativeHumidity                :: Int
} deriving Show

instance Storable Pressure where
    sizeOf _ = #{size Pressure}
    alignment _ = alignment (undefined :: CFloat)
    peek p = Pressure <$> (#{peek Pressure, latitude} p)
                      <*> (#{peek Pressure, longitude} p)
                      <*> fmap cIntToInt (#{peek Pressure, airPressure} p)
                      <*> (#{peek Pressure, geopotentialHeight} p)
                      <*> (#{peek Pressure, lagrangianTendencyOfAirPressure} p)
                      <*> (#{peek Pressure, eastwardWind} p)
                      <*> (#{peek Pressure, northwardWind} p)
                      <*> (#{peek Pressure, airTemperature} p)
                      <*> fmap cIntToInt (#{peek Pressure, relativeHumidity} p)
        where
          cIntToInt :: CInt -> Int
          cIntToInt = fromIntegral
    poke p s = do
      (#{poke Pressure, latitude} p $ latitude s)
      (#{poke Pressure, longitude} p $ longitude s)
      (#{poke Pressure, airPressure} p $ airPressure s)
      (#{poke Pressure, geopotentialHeight} p $ geopotentialHeight s)
      (#{poke Pressure, lagrangianTendencyOfAirPressure} p $ lagrangianTendencyOfAirPressure s)
      (#{poke Pressure, eastwardWind} p $ eastwardWind s)
      (#{poke Pressure, northwardWind} p $ northwardWind s)
      (#{poke Pressure, airTemperature} p $ airTemperature s)
      (#{poke Pressure, relativeHumidity} p $ relativeHumidity s)

deriveJSON id ''Pressure
