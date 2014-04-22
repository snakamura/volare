module DomainSpec (spec) where

import Control.Applicative ((<$>))
import qualified Database.Persist as P
import Test.Hspec

import qualified Volare.Domain as D
import qualified Volare.Model as M

import SpecUtils


spec :: Spec
spec = do
    describe "getWorkspaces" $ do
        context "when there is no workspace" $ do
            it "returns no workspace" $ runDB $ do
                workspaces <- D.getWorkspaces
                length workspaces @== 0

        it "returns all workspaces" $ runDB $ do
            _ <- D.addWorkspace "Test 1"
            _ <- D.addWorkspace "Test 2"
            workspaces <- D.getWorkspaces
            length workspaces @== 2

    describe "getWorkspace" $ do
        it "returns a workspace" $ runDB $ do
            let name = "Test"
            workspaceId <- D.addWorkspace name
            workspace <- D.getWorkspace workspaceId
            P.entityKey <$> workspace @== Just workspaceId
            (M.workspaceName . P.entityVal) <$> workspace @== Just name

    describe "addWorkspace" $ do
        it "adds a workspace" $ runDB $ do
            let name = "Test"
            workspaceId <- D.addWorkspace name
            workspace <- head <$> D.getWorkspaces
            P.entityKey workspace @== workspaceId
            M.workspaceName (P.entityVal workspace) @== name

    describe "updateWorkspace" $ do
        it "updates a name of a workspace"$ runDB $ do
            let nameBefore = "Test 1"
            workspaceId <- D.addWorkspace nameBefore
            let nameAfter = "Test 2"
            D.updateWorkspace workspaceId (Just nameAfter) Nothing
            workspace <- D.getWorkspace workspaceId
            (M.workspaceName . P.entityVal) <$> workspace @== Just nameAfter

        it "doesn't updates a name of a workspace"$ runDB $ do
            let name = "Test 1"
            workspaceId <- D.addWorkspace name
            D.updateWorkspace workspaceId Nothing Nothing
            workspace <- D.getWorkspace workspaceId
            (M.workspaceName . P.entityVal) <$> workspace @== Just name

    describe "deleteWorkspace" $ do
        it "deletes a workspace" $ runDB $ do
            let name = "Test 2"
            workspaceId <- D.addWorkspace "Test 1"
            _ <- D.addWorkspace name
            D.deleteWorkspace workspaceId
            workspaces <- D.getWorkspaces
            length workspaces @== 1
            let workspace = head workspaces
            M.workspaceName (P.entityVal workspace) @== name

        it "does nothing when there is no such workspace" $ runDB $ do
            workspaceId <- D.addWorkspace "Test 1"
            _ <- D.addWorkspace "Test 2"
            D.deleteWorkspace workspaceId
            D.deleteWorkspace workspaceId
            workspaces <- D.getWorkspaces
            length workspaces @== 1
