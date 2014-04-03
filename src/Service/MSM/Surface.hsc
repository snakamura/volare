module Service.MSM.Surface (
    Item(..)
) where

import Control.Applicative ((<$>),
                            (<*>))
import Data.Aeson.TH (defaultOptions,
                      deriveJSON)
import Foreign.C (CFloat,
                  CInt)
import Foreign.Storable (Storable(..))

#include "../../../msm/msm.h"


data Item = Item {
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

instance Storable Item where
    sizeOf _ = #{size surface_item}
    alignment _ = alignment (undefined :: CFloat)
    peek p = Item <$> (#{peek surface_item, latitude} p)
                  <*> (#{peek surface_item, longitude} p)
                  <*> (#{peek surface_item, air_pressure} p)
                  <*> (#{peek surface_item, surface_air_pressure} p)
                  <*> (#{peek surface_item, eastward_wind} p)
                  <*> (#{peek surface_item, northward_wind} p)
                  <*> (#{peek surface_item, air_temperature} p)
                  <*> fmap cIntToInt (#{peek surface_item, relative_humidity} p)
                  <*> (#{peek surface_item, rainfall_rate} p)
                  <*> fmap cIntToInt (#{peek surface_item, upper_cloudiness} p)
                  <*> fmap cIntToInt (#{peek surface_item, mid_cloudiness} p)
                  <*> fmap cIntToInt (#{peek surface_item, low_cloudiness} p)
                  <*> fmap cIntToInt (#{peek surface_item, cloud_amount} p)
        where
          cIntToInt :: CInt -> Int
          cIntToInt = fromIntegral
    poke p s = do
      (#{poke surface_item, latitude} p $ latitude s)
      (#{poke surface_item, longitude} p $ longitude s)
      (#{poke surface_item, air_pressure} p $ airPressure s)
      (#{poke surface_item, surface_air_pressure} p $ surfaceAirPressure s)
      (#{poke surface_item, eastward_wind} p $ eastwardWind s)
      (#{poke surface_item, northward_wind} p $ northwardWind s)
      (#{poke surface_item, air_temperature} p $ airTemperature s)
      (#{poke surface_item, relative_humidity} p $ relativeHumidity s)
      (#{poke surface_item, rainfall_rate} p $ rainfallRate s)
      (#{poke surface_item, upper_cloudiness} p $ upperCloudiness s)
      (#{poke surface_item, mid_cloudiness} p $ midCloudiness s)
      (#{poke surface_item, low_cloudiness} p $ lowCloudiness s)
      (#{poke surface_item, cloud_amount} p $ cloudAmount s)

deriveJSON defaultOptions ''Item
