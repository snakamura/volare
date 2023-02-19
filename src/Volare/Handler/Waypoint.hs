module Volare.Handler.Waypoint
    ( getWaypointsR
    , postWaypointsR
    , getWaypointR
    , putWaypointR
    , deleteWaypointR
    ) where

import qualified Codec.GeoWpt as GeoWpt
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Aeson
    ( (.=)
    , (.:)
    )
import qualified Data.Aeson as JSON
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Database.Persist
    ( Entity
    , entityVal
    )
import qualified Pipes.ByteString as PB
import Text.Blaze.Html (toHtml)
import Yesod.Core (defaultLayout)
import Yesod.Core.Handler
    ( invalidArgs
    , provideRep
    , selectRep
    )
import Yesod.Core.Json (requireCheckJsonBody)
import Yesod.Core.Types (TypedContent)
import Yesod.Core.Widget
    ( addStylesheet
    , setTitle
    )
import Yesod.Persist (runDB)

import qualified Volare.Domain as D
import Volare.Foundation
import Volare.Handler.Utils
    ( addCommonLibraries
    , addGoogleApiKey
    , addRequireJS
    , maybeNotFound
    )
import qualified Volare.Model as M
import Volare.Settings (widgetFile)
import qualified Volare.Static as S
import qualified Volare.Widget as W


getWaypointsR :: Handler TypedContent
getWaypointsR =
    selectRep $ do
        provideRep $ defaultLayout $ do
            setTitle "Waypoints - Volare"
            addCommonLibraries
            addRequireJS "waypoints/index"
            addStylesheet $ StaticR S.css_common_css
            $(widgetFile "waypoints/index")
        provideRep $ runDB $ JSON.toJSON <$> D.getWaypoints


data NewWaypoint = NewWaypoint T.Text B.ByteString

instance JSON.FromJSON NewWaypoint where
    parseJSON (JSON.Object o) = NewWaypoint <$> o .: "name"
                                            <*> (T.encodeUtf8 <$> (o .: "wpt"))
    parseJSON _ = mempty


postWaypointsR :: Handler JSON.Value
postWaypointsR = do
    NewWaypoint name wptBytes <- requireCheckJsonBody
    w <- evalStateT GeoWpt.parser $ PB.fromLazy $ BL.fromStrict wptBytes
    case w of
        Just wpt -> do
            waypoint <- runDB $ do
                waypointId <- D.addWaypoint name wpt
                D.getWaypoint waypointId
            return $ JSON.toJSON waypoint
        Nothing -> invalidArgs ["wpt"]


data Waypoint = Waypoint M.WaypointId M.Waypoint [Entity M.WaypointItem]

instance JSON.ToJSON Waypoint where
    toJSON (Waypoint waypointId waypoint items) =
        JSON.object [ "id" .= waypointId
                    , "name" .= M.waypointName waypoint
                    , "items" .= items
                    ]


getWaypointR :: M.WaypointId ->
                Handler TypedContent
getWaypointR waypointId =
    maybeNotFound (runDB $ D.getWaypoint waypointId) $ \waypointEntity -> do
        let waypoint = entityVal waypointEntity
        selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle $ toHtml $ M.waypointName waypoint <> " - Waypoints - Volare"
                addCommonLibraries
                addGoogleApiKey
                addRequireJS "waypoints/show"
                addStylesheet $ StaticR S.css_common_css
                $(widgetFile "waypoints/show")
            provideRep $ do
                items <- runDB $ D.getWaypointItems waypointId
                return $ JSON.toJSON $ Waypoint waypointId waypoint items


data EditWaypoint = EditWaypoint T.Text

instance JSON.FromJSON EditWaypoint where
    parseJSON (JSON.Object o) = EditWaypoint <$> o .: "name"
    parseJSON _ = mempty


putWaypointR :: M.WaypointId ->
                Handler JSON.Value
putWaypointR waypointId = do
    EditWaypoint name <- requireCheckJsonBody
    waypoint <- runDB $ do
        D.updateWaypoint waypointId (Just name)
        D.getWaypoint waypointId
    return $ JSON.toJSON waypoint


deleteWaypointR :: M.WaypointId ->
                   Handler JSON.Value
deleteWaypointR waypointId = do
    runDB $ D.deleteWaypoint waypointId
    return $ JSON.toJSON ()
