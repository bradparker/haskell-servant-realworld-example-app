module ConduitApp.Users.DatabaseSpec
  ( spec
  ) where

import qualified ConduitApp.Users.Database.User as User
import ConduitApp.Spec.Database (withConnection)
import ConduitApp.Users.Database
  ( create
  , update
  )
import ConduitApp.Users.Database.Attributes (Attributes(Attributes))
import qualified ConduitApp.Users.Database.Attributes as Attributes
import Crypto.Scrypt (EncryptedPass(EncryptedPass), Pass(Pass), verifyPass')
import Data.Bool (Bool)
import Data.Function (($))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Beam (primaryKey)
import Test.Hspec
  ( Spec
  , around
  , describe
  , it
  , shouldBe
  , shouldNotBe
  , shouldSatisfy
  )
import Data.Functor.Identity (Identity)

createParams :: Attributes Identity
createParams =
  Attributes
    { Attributes.password = "password123"
    , Attributes.email = "user@example.com"
    , Attributes.username = "Username"
    , Attributes.bio = ""
    , Attributes.image = Nothing
    }

encryptedPassMatches :: Text -> Text -> Bool
encryptedPassMatches a b =
  verifyPass' (Pass (encodeUtf8 a)) (EncryptedPass (encodeUtf8 b))

spec :: Spec
spec =
  around withConnection $ do
    describe "create" $
      it "creates a User with the supplied params" $ \conn -> do
        user <- create conn createParams
        User.email user `shouldBe` "user@example.com"
        User.username user `shouldBe` "Username"
        User.bio user `shouldBe` ""
        User.image user `shouldBe` Nothing
    describe "update" $
      it "updates a user specified by username with new attributes" $ \conn -> do
        user <- create conn createParams
        updated <-
          update
            conn
            user
            Attributes
              { Attributes.password = Nothing
              , Attributes.email = Nothing
              , Attributes.username = Nothing
              , Attributes.bio = Just "Now with a bio"
              , Attributes.image = Just (Just "http://an.image/woot.jpg")
              }
        primaryKey user `shouldBe` primaryKey updated
        User.username updated `shouldBe` User.username user
        User.email updated `shouldBe` User.email user
        User.bio updated `shouldBe` "Now with a bio"
        User.image updated `shouldBe` Just "http://an.image/woot.jpg"
