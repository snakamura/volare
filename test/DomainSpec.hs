module DomainSpec (spec) where

import qualified Codec.GeoWpt as GeoWpt
import qualified Codec.IGC as IGC
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT)
import Data.Maybe
    ( fromJust
    , isJust
    , isNothing
    )
import Data.Time
    ( UTCTime(UTCTime)
    , fromGregorian
    )
import qualified Database.Persist as P
import qualified Pipes.ByteString as PB
import System.IO
    ( IOMode(ReadMode)
    , withFile
    )
import Test.Hspec
    ( Spec
    , context
    , describe
    , it
    , shouldSatisfy
    )

import qualified Volare.Domain as D
import qualified Volare.Model as M

import SpecUtils


spec :: Spec
spec = do
    let loadIGC = withFile "test/test.igc" ReadMode $ \handle -> do
                      igc <- evalStateT IGC.parser $ PB.fromHandle handle
                      igc `shouldSatisfy` isJust
                      return $ fromJust igc

    let loadWpt = withFile "test/test.wpt" ReadMode $ \handle -> do
                      wpt <- evalStateT GeoWpt.parser $ PB.fromHandle handle
                      wpt `shouldSatisfy` isJust
                      return $ fromJust wpt

    let addRoute = do wpt <- liftIO loadWpt
                      waypointId <- D.addWaypoint "Test" wpt
                      items <- D.getWaypointItems waypointId
                      let routeItems = [ (P.entityKey (items !! 10), 3000)
                                       , (P.entityKey (items !! 10), 400)
                                       , (P.entityKey (items !! 3), 400)
                                       , (P.entityKey (items !! 33), 400)
                                       , (P.entityKey (items !! 55), 400)
                                       , (P.entityKey (items !! 4), 3000)
                                       ]
                      D.addRoute routeItems


    describe "getFlights" $ do
        context "when there is no flight" $ do
            it "returns no flight" $ runDB $ do
                flights <- D.getFlights
                length flights `shouldBe'` 0

        it "returns all flights" $ runDB $ do
            igc <- liftIO loadIGC
            _ <- D.addFlight "Test 1" igc
            _ <- D.addFlight "Test 2" igc
            flights <- D.getFlights
            length flights `shouldBe'` 2

    describe "getFlight" $ do
        it "returns a flight" $ runDB $ do
            let name = "Test"
            igc <- liftIO loadIGC
            flightId <- D.addFlight name igc
            flight <- D.getFlight flightId
            flight `shouldSatisfy'` isJust
            P.entityKey (fromJust flight) `shouldBe'` flightId
            let flightVal = P.entityVal $ fromJust flight
            M.flightName flightVal `shouldBe'` name
            M.flightTime flightVal `shouldBe'` UTCTime (fromGregorian 2014 3 15) (2*60*60 + 8*60 + 35)
            M.flightDuration flightVal `shouldBe'` 10344
            M.flightMinLatitude flightVal `shouldApproximatelyBe'` 36.313835144043
            M.flightMaxLatitude flightVal `shouldApproximatelyBe'` 36.826000213623
            M.flightMinLongitude flightVal `shouldApproximatelyBe'` 140.117538452148
            M.flightMaxLongitude flightVal `shouldApproximatelyBe'` 140.188369750977
            M.flightMinAltitude flightVal `shouldApproximatelyBe'` 148.0
            M.flightMaxAltitude flightVal `shouldApproximatelyBe'` 1966.0

    describe "getFlightRecords" $ do
        it "returns records" $ runDB $ do
            igc <- liftIO loadIGC
            flightId <- D.addFlight "Test" igc
            records <- D.getFlightRecords flightId
            length records `shouldBe'` 5151
            let record = P.entityVal $ records !! 100
            M.recordFlightId record `shouldBe'` flightId
            M.recordIndex record `shouldBe'` 101
            M.recordTime record `shouldBe'` UTCTime (fromGregorian 2014 3 15) (2*60*60 + 12*60 + 25)
            M.recordLatitude record `shouldApproximatelyBe'` 36.3156318664551
            M.recordLongitude record `shouldApproximatelyBe'` 140.179046630859
            M.recordAltitude record `shouldApproximatelyBe'` 264.0

    describe "addFlight" $ do
        it "adds a flight" $ runDB $ do
            let name = "Test"
            igc <- liftIO loadIGC
            flightId <- D.addFlight name igc
            flight <- head <$> D.getFlights
            P.entityKey flight `shouldBe'` flightId

    describe "updateFlight" $ do
        it "updates a name of a flight" $ runDB $ do
            let nameBefore = "Test 1"
            igc <- liftIO loadIGC
            flightId <- D.addFlight nameBefore igc
            let nameAfter = "Test 2"
            D.updateFlight flightId (Just nameAfter)
            flight <- D.getFlight flightId
            flight `shouldSatisfy'` isJust
            M.flightName (P.entityVal (fromJust flight)) `shouldBe'` nameAfter

        it "doesn't updates a name of a flight" $ runDB $ do
            let name = "Test 1"
            igc <- liftIO loadIGC
            flightId <- D.addFlight name igc
            D.updateFlight flightId Nothing
            flight <- D.getFlight flightId
            flight `shouldSatisfy'` isJust
            M.flightName (P.entityVal (fromJust flight)) `shouldBe'` name

    describe "deleteFlight" $ do
        it "deletes a flight" $ runDB $ do
            let name = "Test 2"
            igc <- liftIO loadIGC
            flightId <- D.addFlight "Test 1" igc
            _ <- D.addFlight name igc
            D.deleteFlight flightId
            flights <- D.getFlights
            length flights `shouldBe'` 1
            let flight = head flights
            M.flightName (P.entityVal flight) `shouldBe'` name

        it "does nothing when there is no such flight" $ runDB $ do
            igc <- liftIO loadIGC
            flightId <- D.addFlight "Test 1" igc
            _ <- D.addFlight "Test 2" igc
            D.deleteFlight flightId
            D.deleteFlight flightId
            flights <- D.getFlights
            length flights `shouldBe'` 1

    describe "getWorkspaces" $ do
        context "when there is no workspace" $ do
            it "returns no workspace" $ runDB $ do
                workspaces <- D.getWorkspaces
                length workspaces `shouldBe'` 0

        it "returns all workspaces" $ runDB $ do
            _ <- D.addWorkspace "Test 1"
            _ <- D.addWorkspace "Test 2"
            workspaces <- D.getWorkspaces
            length workspaces `shouldBe'` 2

    describe "getWorkspace" $ do
        it "returns a workspace" $ runDB $ do
            let name = "Test"
            workspaceId <- D.addWorkspace name
            workspace <- D.getWorkspace workspaceId
            workspace `shouldSatisfy'` isJust
            P.entityKey (fromJust workspace) `shouldBe'` workspaceId
            M.workspaceName (P.entityVal (fromJust workspace)) `shouldBe'` name

    describe "addWorkspace" $ do
        it "adds a workspace" $ runDB $ do
            let name = "Test"
            workspaceId <- D.addWorkspace name
            workspace <- head <$> D.getWorkspaces
            P.entityKey workspace `shouldBe'` workspaceId
            M.workspaceName (P.entityVal workspace) `shouldBe'` name

    describe "updateWorkspace" $ do
        it "updates a name of a workspace" $ runDB $ do
            let nameBefore = "Test 1"
            workspaceId <- D.addWorkspace nameBefore
            let nameAfter = "Test 2"
            D.updateWorkspace workspaceId (Just nameAfter) Nothing
            workspace <- D.getWorkspace workspaceId
            workspace `shouldSatisfy'` isJust
            M.workspaceName (P.entityVal (fromJust workspace)) `shouldBe'` nameAfter

        it "doesn't updates a name of a workspace" $ runDB $ do
            let name = "Test 1"
            workspaceId <- D.addWorkspace name
            D.updateWorkspace workspaceId Nothing Nothing
            workspace <- D.getWorkspace workspaceId
            workspace `shouldSatisfy'` isJust
            M.workspaceName (P.entityVal (fromJust workspace)) `shouldBe'` name

        it "updates a route of a workspace" $ runDB $ do
            workspaceId <- D.addWorkspace "Test"
            routeId <- addRoute
            D.updateWorkspace workspaceId Nothing (Just (Just routeId))
            workspace1 <- D.getWorkspace workspaceId
            workspace1 `shouldSatisfy'` isJust
            M.workspaceRoute (P.entityVal (fromJust workspace1)) `shouldBe'` Just routeId
            D.updateWorkspace workspaceId Nothing (Just Nothing)
            workspace2 <- D.getWorkspace workspaceId
            workspace2 `shouldSatisfy'` isJust
            M.workspaceRoute (P.entityVal (fromJust workspace2)) `shouldBe'` Nothing

        it "doesn't update a route of a workspace" $ runDB $ do
            workspaceId <- D.addWorkspace "Test"
            routeId <- addRoute
            D.updateWorkspace workspaceId Nothing (Just (Just routeId))
            workspace1 <- D.getWorkspace workspaceId
            workspace1 `shouldSatisfy'` isJust
            M.workspaceRoute (P.entityVal (fromJust workspace1)) `shouldBe'` Just routeId
            D.updateWorkspace workspaceId Nothing Nothing
            workspace2 <- D.getWorkspace workspaceId
            workspace2 `shouldSatisfy'` isJust
            M.workspaceRoute (P.entityVal (fromJust workspace2)) `shouldBe'` Just routeId

        it "deletes an old route of a workspace" $ runDB $ do
            workspaceId <- D.addWorkspace "Test"
            oldRouteId <- addRoute
            D.updateWorkspace workspaceId Nothing (Just (Just oldRouteId))
            newRouteId <- addRoute
            D.updateWorkspace workspaceId Nothing (Just (Just newRouteId))
            oldRoute <- D.getRoute oldRouteId
            oldRoute `shouldSatisfy'` isNothing
            D.updateWorkspace workspaceId Nothing (Just Nothing)
            newRoute <- D.getRoute newRouteId
            newRoute `shouldSatisfy'` isNothing

        it "doesn't delete an old route of a workspace when setting the same route" $ runDB $ do
            workspaceId <- D.addWorkspace "Test"
            routeId <- addRoute
            D.updateWorkspace workspaceId Nothing (Just (Just routeId))
            D.updateWorkspace workspaceId Nothing (Just (Just routeId))
            route <- D.getRoute routeId
            route `shouldSatisfy'` isJust

    describe "deleteWorkspace" $ do
        it "deletes a workspace" $ runDB $ do
            let name = "Test 2"
            workspaceId <- D.addWorkspace "Test 1"
            _ <- D.addWorkspace name
            D.deleteWorkspace workspaceId
            workspaces <- D.getWorkspaces
            length workspaces `shouldBe'` 1
            let workspace = head workspaces
            M.workspaceName (P.entityVal workspace) `shouldBe'` name

        it "does nothing when there is no such workspace" $ runDB $ do
            workspaceId <- D.addWorkspace "Test 1"
            _ <- D.addWorkspace "Test 2"
            D.deleteWorkspace workspaceId
            D.deleteWorkspace workspaceId
            workspaces <- D.getWorkspaces
            length workspaces `shouldBe'` 1

        it "deletes a route of a workspace" $ runDB $ do
            workspaceId <- D.addWorkspace "Test"
            routeId <- addRoute
            D.updateWorkspace workspaceId Nothing (Just (Just routeId))
            D.deleteWorkspace workspaceId
            route <- D.getRoute routeId
            route `shouldSatisfy'` isNothing

    describe "getWorkspaceFlights" $ do
        context "when there is no flights" $ do
            it "returns no flight" $ runDB $ do
                workspaceId <- D.addWorkspace "Test"
                workspaceFlights <- D.getWorkspaceFlights workspaceId
                length workspaceFlights `shouldBe'` 0

        it "returns all flights" $ runDB $ do
            workspaceId <- D.addWorkspace "Test"
            igc <- liftIO loadIGC
            let name = "Test 1"
            flightId1 <- D.addFlight name igc
            flightId2 <- D.addFlight "Test 2" igc
            _ <- D.addWorkspaceFlight workspaceId [flightId1, flightId2]
            workspaceFlights <- D.getWorkspaceFlights workspaceId
            length workspaceFlights `shouldBe'` 2
            let D.WorkspaceFlight flightId flight color = head workspaceFlights
            flightId `shouldBe'` flightId1
            M.flightName flight `shouldBe'` name
            color `shouldBe'` "red"

    describe "addWorkspaceFlight" $ do
        it "adds a flight to a workspace" $ runDB $ do
            workspaceId <- D.addWorkspace "Test"
            igc <- liftIO loadIGC
            let name = "Test"
            flightId <- D.addFlight name igc
            _ <- D.addWorkspaceFlight workspaceId [flightId]
            workspaceFlights <- D.getWorkspaceFlights workspaceId
            length workspaceFlights `shouldBe'` 1
            let D.WorkspaceFlight flightId' flight color = head workspaceFlights
            flightId' `shouldBe'` flightId
            M.flightName flight `shouldBe'` name
            color `shouldBe'` "red"

    describe "deleteWorkspaceFlight" $ do
        it "removes a flight from a workspace" $ runDB $ do
            workspaceId <- D.addWorkspace "Test"
            igc <- liftIO loadIGC
            let name = "Test 2"
            flightId1 <- D.addFlight "Test 1" igc
            flightId2 <- D.addFlight name igc
            _ <- D.addWorkspaceFlight workspaceId [flightId1, flightId2]
            D.deleteWorkspaceFlight workspaceId flightId1
            workspaceFlights <- D.getWorkspaceFlights workspaceId
            length workspaceFlights `shouldBe'` 1
            let D.WorkspaceFlight flightId flight color = head workspaceFlights
            flightId `shouldBe'` flightId2
            M.flightName flight `shouldBe'` name
            color `shouldBe'` "blue"

        it "does nothing when there is no such flight in a workspace" $ runDB $ do
            workspaceId <- D.addWorkspace "Test"
            igc <- liftIO loadIGC
            flightId1 <- D.addFlight "Test 1" igc
            flightId2 <- D.addFlight "Test 2" igc
            _ <- D.addWorkspaceFlight workspaceId [flightId1]
            D.deleteWorkspaceFlight workspaceId flightId2
            workspaceFlights <- D.getWorkspaceFlights workspaceId
            length workspaceFlights `shouldBe'` 1

    describe "getWaypoints" $ do
        context "when there is no waypoint" $ do
            it "returns no waypoint" $ runDB $ do
                waypoints <- D.getWaypoints
                length waypoints `shouldBe'` 0

        it "returns all waypoints" $ runDB $ do
            wpt <- liftIO loadWpt
            _ <- D.addWaypoint "Test 1" wpt
            _ <- D.addWaypoint "Test 2" wpt
            waypoints <- D.getWaypoints
            length waypoints `shouldBe'` 2

    describe "getWaypoint" $ do
        it "returns a waypoint" $ runDB $ do
            let name = "Test"
            wpt <- liftIO loadWpt
            waypointId <- D.addWaypoint name wpt
            waypoint <- D.getWaypoint waypointId
            waypoint `shouldSatisfy'` isJust
            P.entityKey (fromJust waypoint) `shouldBe'` waypointId
            M.waypointName (P.entityVal (fromJust waypoint)) `shouldBe'` name

    describe "getWaypointItems" $ do
        it "returns items" $ runDB $ do
            wpt <- liftIO loadWpt
            waypointId <- D.addWaypoint "Test" wpt
            items <- D.getWaypointItems waypointId
            length items `shouldBe'` 122
            let item = P.entityVal $ items !! 43
            M.waypointItemWaypointId item `shouldBe'` waypointId
            M.waypointItemName item `shouldBe'` "B44002"
            M.waypointItemLatitude item `shouldApproximatelyBe'` 36.1041641235352
            M.waypointItemLongitude item `shouldApproximatelyBe'` 140.08723449707
            M.waypointItemAltitude item `shouldApproximatelyBe'` 21.0
            M.waypointItemDescription item `shouldBe'` "KOK"

    describe "addWaypoint" $ do
        it "adds a waypoint" $ runDB $ do
            let name = "Test"
            wpt <- liftIO loadWpt
            waypointId <- D.addWaypoint name wpt
            waypoint <- head <$> D.getWaypoints
            P.entityKey waypoint `shouldBe'` waypointId

    describe "updateWaypoint" $ do
        it "updates a name of a waypoint" $ runDB $ do
            let nameBefore = "Test 1"
            wpt <- liftIO loadWpt
            waypointId <- D.addWaypoint nameBefore wpt
            let nameAfter = "Test 2"
            D.updateWaypoint waypointId (Just nameAfter)
            waypoint <- D.getWaypoint waypointId
            waypoint `shouldSatisfy'` isJust
            M.waypointName (P.entityVal (fromJust waypoint)) `shouldBe'` nameAfter

        it "doesn't updates a name of a waypoint" $ runDB $ do
            let name = "Test 1"
            wpt <- liftIO loadWpt
            waypointId <- D.addWaypoint name wpt
            D.updateWaypoint waypointId Nothing
            waypoint <- D.getWaypoint waypointId
            waypoint `shouldSatisfy'` isJust
            M.waypointName (P.entityVal (fromJust waypoint)) `shouldBe'` name

    describe "deleteWaypoint" $ do
        it "deletes a waypoint" $ runDB $ do
            let name = "Test 2"
            wpt <- liftIO loadWpt
            waypointId <- D.addWaypoint "Test 1" wpt
            _ <- D.addWaypoint name wpt
            D.deleteWaypoint waypointId
            waypoints <- D.getWaypoints
            length waypoints `shouldBe'` 1
            let waypoint = head waypoints
            M.waypointName (P.entityVal waypoint) `shouldBe'` name

        it "does nothing when there is no such waypoint" $ runDB $ do
            wpt <- liftIO loadWpt
            waypointId <- D.addWaypoint "Test 1" wpt
            _ <- D.addWaypoint "Test 2" wpt
            D.deleteWaypoint waypointId
            D.deleteWaypoint waypointId
            waypoints <- D.getWaypoints
            length waypoints `shouldBe'` 1

    describe "getRoute" $ do
        it "returns a route" $ runDB $ do
            routeId <- addRoute
            route <- D.getRoute routeId
            route `shouldSatisfy'` isJust
            P.entityKey (fromJust route) `shouldBe'` routeId

    describe "getRouteWithWaypoints" $ do
        it "returns a route with waypoints" $ runDB $ do
            routeId <- addRoute
            route <- D.getRouteWithWaypoints routeId
            route `shouldSatisfy'` isJust
            let D.Route routeId' items = fromJust route
            routeId' `shouldBe'` routeId
            length items `shouldBe'` 6
            let D.RouteItem _ (P.Entity _ item) radius = items !! 1
            M.waypointItemName item `shouldBe'` "B09012"
            M.waypointItemLatitude item `shouldApproximatelyBe'` 36.3802833557129
            M.waypointItemLongitude item `shouldApproximatelyBe'` 140.035491943359
            M.waypointItemAltitude item `shouldApproximatelyBe'` 124.0
            M.waypointItemDescription item `shouldBe'` "GOL"
            radius `shouldBe'` 400

    describe "addRoute" $ do
        it "adds a route" $ runDB $ do
            routeId <- addRoute
            route <- D.getRoute routeId
            route `shouldSatisfy'` isJust
            P.entityKey (fromJust route) `shouldBe'` routeId

    describe "deleteRoute" $ do
        it "deletes a route" $ runDB $ do
            routeId <- addRoute
            D.deleteRoute routeId
            route <- D.getRoute routeId
            route `shouldSatisfy'` isNothing

        it "does nothing when there is no such route" $ runDB $ do
            routeId <- addRoute
            D.deleteRoute routeId
            D.deleteRoute routeId
            route <- D.getRoute routeId
            route `shouldSatisfy'` isNothing
