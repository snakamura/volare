module Volare.MSM (
    getSurface,
    getPressure
) where

import Control.Exception (Exception,
                          throwIO)
import Data.Typeable (Typeable)
import Foreign.C (CFloat(..),
                  CInt(..),
                  CSize(..),
                  CString,
                  withCString)
import Foreign.Marshal (allocaArray,
                        peekArray)
import Foreign.Ptr (Ptr,
                    nullPtr)
import Foreign.Storable (Storable)

import Volare.MSM.Pressure (Pressure)
import Volare.MSM.Surface (Surface)

#include "../../../msm/msm.h"

data MSMException = MSMException deriving (Show, Typeable)

instance Exception MSMException


getSurface :: FilePath ->
              (Float, Float) ->
              (Float, Float) ->
              Int ->
              IO [Surface]
getSurface = get surface


getPressure :: FilePath ->
               (Float, Float) ->
               (Float, Float) ->
               Int ->
               IO [Pressure]
getPressure = get pressure


get :: Storable a =>
       (CString -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr a -> CSize -> IO CSize) ->
       FilePath ->
       (Float, Float) ->
       (Float, Float) ->
       Int ->
       IO [a]
get f path (nwLatitude, nwLongitude) (seLatitude, seLongitude) time =
  withCString path $ \cpath -> do
    let g = f cpath (CFloat nwLatitude) (CFloat nwLongitude) (CFloat seLatitude) (CFloat seLongitude) (fromIntegral time)
    count <- g nullPtr 0
    case count of
      -1 -> throwIO MSMException
      0 -> return []
      _ -> allocaArray (fromIntegral count) $ \values ->
             do readCount <- g values count
                case readCount of
                  -1 -> throwIO MSMException
                  0 -> return []
                  _ -> peekArray (fromIntegral readCount) values


foreign import ccall surface :: CString ->
                                CFloat ->
                                CFloat ->
                                CFloat ->
                                CFloat ->
                                CInt ->
                                Ptr Surface ->
                                CSize ->
                                IO CSize

foreign import ccall pressure :: CString ->
                                 CFloat ->
                                 CFloat ->
                                 CFloat ->
                                 CFloat ->
                                 CInt ->
                                 Ptr Pressure ->
                                 CSize ->
                                 IO CSize
