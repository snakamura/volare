module Volare.MSM (
    Surface(..),
    getSurface
) where

import Control.Applicative ((<$>),
                            (<*>))
import Control.Exception (Exception,
                          throwIO)
import Data.Aeson.TH (deriveJSON)
import Data.Typeable (Typeable)
import Foreign.C (CFloat(..),
                  CInt(..),
                  CSize(..),
                  CString,
                  withCString)
import Foreign.Marshal (allocaArray,
                        peekArray)
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr,
                    nullPtr)

#include "../../../msm/msm.h"

data MSMException = MSMException deriving (Show, Typeable)

instance Exception MSMException


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


getSurface :: FilePath ->
              (Float, Float) ->
              (Float, Float) ->
              Int ->
              IO [Surface]
getSurface path (nwLatitude, nwLongitude) (seLatitude, seLongitude) time =
  withCString path $ \cpath -> do
      count <- surface cpath (CFloat nwLatitude) (CFloat nwLongitude) (CFloat seLatitude) (CFloat seLongitude) (fromIntegral time) nullPtr 0
      case count of
        -1 -> throwIO MSMException
        0 -> return []
        _ -> allocaArray (fromIntegral count) $ \surfaces ->
               do readCount <- surface cpath (CFloat nwLatitude) (CFloat nwLongitude) (CFloat seLatitude) (CFloat seLongitude) (fromIntegral time) surfaces count
                  case readCount of
                    -1 -> throwIO MSMException
                    0 -> return []
                    _ -> peekArray (fromIntegral readCount) surfaces


foreign import ccall surface :: CString ->
                                CFloat ->
                                CFloat ->
                                CFloat ->
                                CFloat ->
                                CInt ->
                                Ptr Surface ->
                                CSize ->
                                IO CSize
