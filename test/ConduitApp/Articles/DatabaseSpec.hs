module ConduitApp.Articles.DatabaseSpec
  ( spec
  ) where

import ConduitApp.Articles.Database
  ( ArticleT(..)
  , QueryParams(..)
  , assignTags
  , create
  , favorite
  , query
  )
import ConduitApp.Spec.Database (userCreateParams, withConnection)
import qualified ConduitApp.Users.Database as Users
import Control.Monad (forM_, void)
import Data.Int (Int)
import Data.List (sort)
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.Function (($))
import Data.Text (pack)
import Database.Beam (primaryKey)
import Test.Hspec (Spec, around, describe, it, shouldBe)
import qualified ConduitApp.Articles.Database.Attributes as Attributes
import ConduitApp.Articles.Database.Attributes (Attributes(Attributes))
import ConduitApp.Articles.Database.Decorated (Decorated(Decorated))
import qualified ConduitApp.Articles.Database.Article as Article
import Data.Functor.Identity (Identity)
import qualified ConduitApp.Users.Database.Attributes as UserAttributes

createParams :: Attributes Identity
createParams =
  Attributes
    { Attributes.title = "Some title"
    , Attributes.slug = "some-title"
    , Attributes.description = "A thing that was writ"
    , Attributes.body = "A longer thing that was writ"
    }

spec :: Spec
spec =
  around withConnection $ do
    describe "create" $
      it "creates an Article with the supplied params" $ \conn -> do
        author <- Users.create conn userCreateParams
        article <- create conn (primaryKey author) createParams
        Article.slug article `shouldBe` "some-title"
    describe "query" $
      it
        "returns a tuple of Article, Author, Favorites Count and Current User fav status" $ \conn -> do
        currentUser <-
          Users.create
            conn
            userCreateParams
              { UserAttributes.email = "current-user@example.com"
              , UserAttributes.username = "current-user"
              }
        author <-
          Users.create
            conn
            userCreateParams
              { UserAttributes.email = "author@example.com"
              , UserAttributes.username = "author"
              }
        articleA <-
          create
            conn
            (primaryKey author)
            createParams
              {Attributes.title = "Article a", Attributes.slug = "article-a"}
        assignTags conn (primaryKey articleA) (Set.singleton "foo")
        articleB <-
          create
            conn
            (primaryKey author)
            createParams
              {Attributes.title = "Article b", Attributes.slug = "article-b"}
        articleC <-
          create
            conn
            (primaryKey author)
            createParams
              {Attributes.title = "Article c", Attributes.slug = "article-c"}
        assignTags conn (primaryKey articleC) (Set.singleton "foo")
        forM_ ([0 .. 4] :: [Int]) $ \n -> do
          user <-
            Users.create
              conn
              userCreateParams
                { UserAttributes.email =
                    "user" <> pack (show n) <> "@example.com"
                , UserAttributes.username = "username" <> pack (show n)
                }
          favorite conn (primaryKey articleA) (primaryKey user)
        void (favorite conn (primaryKey articleB) (primaryKey currentUser))
        found <- query conn (Just currentUser) (QueryParams 1000 0 [] [] [])
        sort found `shouldBe`
          sort
            [ Decorated articleA author ["foo"] 5 False
            , Decorated articleB author [] 1 True
            , Decorated articleC author ["foo"] 0 False
            ]
