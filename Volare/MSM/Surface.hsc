module Volare.MSM.Surface (
    Surface(..)
) where

import Control.Applicative ((<$>),
                            (<*>))
import Data.Aeson.TH (deriveJSON)
import Foreign.C (CFloat,
                  CInt)
import Foreign.Storable (Storable(..))

#include "../../../msm/msm.h"


data Surface = Surface {
    latitude           :: Float,
    longitude          :: Float,
    airPressure        :: Float,
    surfaceAirPressure :: Float,
    eastwardWind       :: Float,
    northwardWind      :: Float,
    airTemperature     :: Float,
    relativeHumidity   :: Int,
    rainfallRate       :: Float,
    upperCloudiness    :: Int,
    midCloudiness      :: Int,
    lowCloudiness      :: Int,
    cloudAmount        :: Int
} deriving Show

instance Storable Surface where
    sizeOf _ = #{size Surface}
    alignment _ = alignment (undefined :: CFloat)
    peek p = Surface <$> (#{peek Surface, latitude} p)
                     <*> (#{peek Surface, longitude} p)
                     <*> (#{peek Surface, airPressure} p)
                     <*> (#{peek Surface, surfaceAirPressure} p)
                     <*> (#{peek Surface, eastwardWind} p)
                     <*> (#{peek Surface, northwardWind} p)
                     <*> (#{peek Surface, airTemperature} p)
                     <*> fmap cIntToInt (#{peek Surface, relativeHumidity} p)
                     <*> (#{peek Surface, rainfallRate} p)
                     <*> fmap cIntToInt (#{peek Surface, upperCloudiness} p)
                     <*> fmap cIntToInt (#{peek Surface, midCloudiness} p)
                     <*> fmap cIntToInt (#{peek Surface, lowCloudiness} p)
                     <*> fmap cIntToInt (#{peek Surface, cloudAmount} p)
        where
          cIntToInt :: CInt -> Int
          cIntToInt = fromIntegral
    poke p s = do
      (#{poke Surface, latitude} p $ latitude s)
      (#{poke Surface, longitude} p $ longitude s)
      (#{poke Surface, airPressure} p $ airPressure s)
      (#{poke Surface, surfaceAirPressure} p $ surfaceAirPressure s)
      (#{poke Surface, eastwardWind} p $ eastwardWind s)
      (#{poke Surface, northwardWind} p $ northwardWind s)
      (#{poke Surface, airTemperature} p $ airTemperature s)
      (#{poke Surface, relativeHumidity} p $ relativeHumidity s)
      (#{poke Surface, rainfallRate} p $ rainfallRate s)
      (#{poke Surface, upperCloudiness} p $ upperCloudiness s)
      (#{poke Surface, midCloudiness} p $ midCloudiness s)
      (#{poke Surface, lowCloudiness} p $ lowCloudiness s)
      (#{poke Surface, cloudAmount} p $ cloudAmount s)

deriveJSON id ''Surface
