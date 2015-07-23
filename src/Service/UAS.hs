module Service.UAS
    ( Types.Station(..)
    , Types.Observation(..)
    , Types.Item(..)
    , Types.Plane(..)
    , Types.Pressure(..)
    , Types.Entry(..)
    , download
    , parser
    , Stations.station
    , Stations.stations
    , Stations.allStations
    ) where

import Codec.Utils.Pipes (makeParser)
import qualified Data.ByteString as B
import qualified Data.Text.Lazy as TL
import Formatting ((%))
import qualified Formatting as F
import qualified Network.HTTP.Client as Http
import Pipes (Producer)
import Pipes.HTTP (withHTTP)
import qualified Pipes.Parse as P

import Service.UAS.Parser (observation)
import qualified Service.UAS.Types as Types
import qualified Service.UAS.Stations as Stations


download :: Types.Station ->
            Int ->
            Int ->
            Int ->
            Int ->
            (Producer B.ByteString IO () -> IO r) ->
            IO r
download station year month day hour process = do
    let baseURL = "http://weather.uwyo.edu/cgi-bin/sounding?TYPE=TEXT%3ARAW"
        url = F.format (F.stext % "&YEAR=" % F.left 4 '0'
                                % "&MONTH=" % F.left 2 '0'
                                % "&FROM=" % F.left 2 '0' % F.left 2 '0'
                                % "&TO=" % F.left 2 '0' % F.left 2 '0'
                                % "&STNM=" % F.int)
                       baseURL year month day hour day hour (Types.id station)
    req <- Http.parseUrl $ TL.unpack url
    manager <- Http.newManager Http.defaultManagerSettings
    withHTTP req manager $ process . Http.responseBody


parser :: (Functor m, Monad m) =>
          P.Parser B.ByteString m (Maybe Types.Observation)
parser = makeParser observation
