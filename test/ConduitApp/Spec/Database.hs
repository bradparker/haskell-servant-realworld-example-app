module ConduitApp.Spec.Database
  ( withConnection
  , userCreateParams
  , articleCreateParams
  , createArticle
  ) where

import Control.Applicative (pure)
import ConduitApp.Articles.Database (Article)
import qualified ConduitApp.Articles.Database as Articles
import qualified ConduitApp.Articles.Database.Attributes as Article
import ConduitApp.Database (openConduitDb)
import qualified ConduitApp.Users.Database.Attributes as User
import qualified ConduitApp.Users.Database as Users
import Control.Exception (bracket)
import Database.Beam (primaryKey)
import Database.PostgreSQL.Simple (Connection, begin, close, rollback)
import System.IO (IO)
import Data.Functor.Identity (Identity)

withConnection :: (Connection -> IO a) -> IO a
withConnection =
  bracket
    (do
      conn <- openConduitDb
      begin conn
      pure conn)
    (\conn -> do
      rollback conn
      close conn)

userCreateParams :: User.Attributes Identity
userCreateParams =
  User.Attributes
    { User.password = "password123"
    , User.email = "user@example.com"
    , User.username = "Username"
    , User.bio = ""
    , User.image = Nothing
    }

articleCreateParams :: Article.Attributes Identity
articleCreateParams =
  Article.Attributes
    { Article.title = "Some title"
    , Article.slug = "some-title"
    , Article.description = "A thing that was writ"
    , Article.body = "A longer thing that was writ"
    }

createArticle ::
     Connection
  -> (User.Attributes Identity -> User.Attributes Identity)
  -> (Article.Attributes Identity -> Article.Attributes Identity)
  -> IO Article
createArticle conn modAuthor modArticle = do
  author <- Users.create conn (modAuthor userCreateParams)
  Articles.create conn (primaryKey author) (modArticle articleCreateParams)
