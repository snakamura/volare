module Volare.Handler.Route
    ( postRoutesR
    , getRouteR
    , deleteRouteR
    ) where

import Control.Applicative ((<*>))
import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import Data.Functor ((<$>))
import Data.Monoid (mempty)
import Yesod.Core.Json (requireJsonBody)
import Yesod.Persist (runDB)

import qualified Volare.Domain as D
import Volare.Foundation
import qualified Volare.Model as M


data NewRoute = NewRoute [NewRouteItem]

instance JSON.FromJSON NewRoute where
    parseJSON a@(JSON.Array _) = NewRoute <$> JSON.parseJSON a
    parseJSON _ = mempty


data NewRouteItem = NewRouteItem M.WaypointItemId Int

instance JSON.FromJSON NewRouteItem where
    parseJSON (JSON.Object o) = NewRouteItem <$> o .: "waypointItemId"
                                             <*> o .: "radius"
    parseJSON _ = mempty


postRoutesR :: Handler JSON.Value
postRoutesR = do
    NewRoute items <- requireJsonBody
    route <- runDB $ do
        routeId <- D.addRoute $ map (\(NewRouteItem waypointItemId radius) -> (waypointItemId, radius)) items
        D.getRoute routeId
    return $ JSON.toJSON route


getRouteR :: M.RouteId ->
             Handler JSON.Value
getRouteR routeId = runDB $ JSON.toJSON <$> D.getRouteWithWaypoints routeId


deleteRouteR :: M.RouteId ->
                Handler JSON.Value
deleteRouteR routeId = do
    runDB $ D.deleteRoute routeId
    return $ JSON.toJSON ()
