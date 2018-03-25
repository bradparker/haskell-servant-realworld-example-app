module ConduitApp.Comments.DatabaseSpec
  ( spec
  ) where

import ConduitApp.Articles.Database (ArticleId)
import ConduitApp.Comments.Database (create)
import qualified ConduitApp.Comments.Database.Comment as Comment
import ConduitApp.Spec.Database
  ( createArticle
  , userCreateParams
  , withConnection
  )
import qualified ConduitApp.Users.Database as Users
import qualified ConduitApp.Users.Database.Attributes as UserAttributes
import Data.Function (($))
import Data.Functor ((<$>))
import Database.Beam (primaryKey)
import Test.Hspec (Spec, around, describe, it, shouldBe)

spec :: Spec
spec =
  around withConnection $
    describe "create" $
      it "creates a Comment with the supplied params" $ \conn -> do
        author <-
          Users.create
            conn
            userCreateParams
              { UserAttributes.username = "different"
              , UserAttributes.email = "also@different.com"
              }
        articleId <- primaryKey <$> createArticle conn id id
        comment <- create conn (primaryKey author) articleId "This is great!"
        Comment.body comment `shouldBe` "This is great!"
