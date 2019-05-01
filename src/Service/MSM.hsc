module Service.MSM
    ( Family(..)
    , getSurfaceItems
    , getBarometricItems
    , download
    , downloadLatest
    ) where

import Control.Exception
    ( Exception
    , throwIO)
import qualified Data.ByteString as B
import Data.Typeable (Typeable)
import Foreign.C
    ( CFloat(..)
    , CInt(..)
    , CSize(..)
    , CString
    , withCString
    )
import Foreign.Marshal
    ( allocaArray
    , peekArray
    )
import Foreign.Ptr
    ( Ptr
    , nullPtr
    )
import Foreign.Storable (Storable)
import qualified Network.HTTP.Client as Http
import Pipes (Producer)
import Pipes.HTTP (withHTTP)
import Text.Printf (printf)

import qualified Service.MSM.Barometric as Barometric
import qualified Service.MSM.Surface as Surface

#include "msm.h"

data Family = Surface
            | Barometric

data MSMException = MSMException deriving (Show, Typeable)

instance Exception MSMException


getSurfaceItems :: FilePath ->
                   (Float, Float) ->
                   (Float, Float) ->
                   Int ->
                   IO [Surface.Item]
getSurfaceItems = getItems get_surface_items


getBarometricItems :: FilePath ->
                      (Float, Float) ->
                      (Float, Float) ->
                      Int ->
                      IO [Barometric.Item]
getBarometricItems = getItems get_barometric_items


getItems :: Storable a =>
            (CString -> CFloat -> CFloat -> CFloat -> CFloat -> CInt -> Ptr a -> CSize -> IO CSize) ->
            FilePath ->
            (Float, Float) ->
            (Float, Float) ->
            Int ->
            IO [a]
getItems f path (nwLatitude, nwLongitude) (seLatitude, seLongitude) time =
    withCString path $ \cpath -> do
        let g = f cpath (CFloat nwLatitude) (CFloat nwLongitude) (CFloat seLatitude) (CFloat seLongitude) (fromIntegral time)
        count <- g nullPtr 0
        case count of
            -1 -> throwIO MSMException
            0 -> return []
            _ -> allocaArray (fromIntegral count) $ \values -> do
                readCount <- g values count
                case readCount of
                    -1 -> throwIO MSMException
                    0 -> return []
                    _ -> peekArray (fromIntegral readCount) values


download :: Family ->
            Int ->
            Int ->
            Int ->
            (Producer B.ByteString IO () -> IO r) ->
            IO r
download family year month day process = do
    let url = printf "http://database.rish.kyoto-u.ac.jp/arch/jmadata/data/gpv/netcdf/MSM-%c/%04d/%02d%02d.nc" (symbol family) year month day
    req <- Http.parseUrlThrow url
    manager <- Http.newManager Http.defaultManagerSettings
    withHTTP req manager $ process . Http.responseBody


downloadLatest :: Family ->
                  Int ->
                  Int ->
                  Int ->
                  Int ->
                  (Producer B.ByteString IO () -> IO r) ->
                  IO r
downloadLatest family year month day hour process = do
    let url = printf "http://database.rish.kyoto-u.ac.jp/arch/jmadata/data/gpv/latest/%04d%02d%02d/MSM%04d%02d%02d%02d%c.nc" year month day year month day hour (symbol family)
    req <- Http.parseUrlThrow url
    manager <- Http.newManager Http.defaultManagerSettings
    withHTTP req manager $ process . Http.responseBody


foreign import ccall get_surface_items :: CString ->
                                          CFloat ->
                                          CFloat ->
                                          CFloat ->
                                          CFloat ->
                                          CInt ->
                                          Ptr Surface.Item ->
                                          CSize ->
                                          IO CSize

foreign import ccall get_barometric_items :: CString ->
                                             CFloat ->
                                             CFloat ->
                                             CFloat ->
                                             CFloat ->
                                             CInt ->
                                             Ptr Barometric.Item ->
                                             CSize ->
                                             IO CSize


symbol :: Family ->
          Char
symbol Surface = 'S'
symbol Barometric = 'P'
