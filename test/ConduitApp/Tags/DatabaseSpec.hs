module ConduitApp.Tags.DatabaseSpec
  ( spec
  ) where

import ConduitApp.Spec.Database (withConnection)
import ConduitApp.Tags.Database (TagT(..), create)
import Data.Function (($))
import Data.List (map)
import qualified Data.Set as Set
import Test.Hspec (Spec, around, describe, it, shouldBe)

spec :: Spec
spec =
  around withConnection $
  describe "create" $ do
    it "creates Tags with the supplied names" $ \conn -> do
      res <- create conn (Set.fromList ["bar", "foo"])
      map tagName res `shouldBe` ["bar", "foo"]
    it "no-ops whens asked to create an existing tag" $ \conn -> do
      resA <- create conn (Set.singleton "foo")
      resB <- create conn (Set.singleton "foo")
      resA `shouldBe` resB
