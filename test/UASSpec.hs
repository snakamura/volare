module UASSpec (spec) where

import Test.Hspec

import qualified Service.UAS as UAS

import SpecUtils


spec :: Spec
spec = do
    describe "download" $ do
        it "downloads raw data" $ do
            let Just station = UAS.station 47646
            hash <- UAS.download station 2014 04 26 0 sha1
            hash `shouldBe` "c54fc17de4f436c0742f8ffa25af7d23d11842d7"
