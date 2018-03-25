module ConduitApp.Comments.Database
  ( create
  , destroy
  , find
  , forArticle
  ) where

import ConduitApp.Articles.Database.Article (ArticleId)
import ConduitApp.Comments.Database.Comment
  ( Comment
  , CommentId
  , CommentT(Comment)
  )
import qualified ConduitApp.Comments.Database.Comment as Comment
import ConduitApp.Database
  ( ConduitDb(conduitComments, conduitUsers)
  , conduitDb
  , insertOne
  )
import ConduitApp.Users.Database.User (User, UserId)
import qualified ConduitApp.Users.Database.User as User
import Control.Applicative (pure)
import Data.Function (($), (.))
import Data.Maybe (Maybe)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Beam
  ( (==.)
  , all_
  , default_
  , delete
  , filter_
  , insertExpressions
  , primaryKey
  , runDelete
  , runSelectReturningList
  , runSelectReturningOne
  , select
  , val_
  )
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection)
import System.IO (IO)

create :: Connection -> UserId -> ArticleId -> Text -> IO Comment
create conn authorId articleId body = do
  currentTime <- getCurrentTime
  insertOne
    conn
    (conduitComments conduitDb)
    (insertExpressions
       [ Comment
           { Comment._id = default_
           , Comment.body = val_ body
           , Comment.article = val_ articleId
           , Comment.author = val_ authorId
           , Comment.createdAt = val_ currentTime
           , Comment.updatedAt = val_ currentTime
           }
       ])

destroy :: Connection -> CommentId -> IO ()
destroy conn comment =
  runBeamPostgres conn $
  runDelete $
  delete (conduitComments conduitDb) $ \candidate ->
    primaryKey candidate ==. val_ comment

find :: Connection -> CommentId -> IO (Maybe (User, Comment))
find conn commentId =
  runBeamPostgres conn $
  runSelectReturningOne $
  select $ do
    comment <-
      filter_
        ((val_ commentId ==.) . primaryKey)
        (all_ (conduitComments conduitDb))
    user <-
      filter_
        ((Comment.author comment ==.) . primaryKey)
        (all_ (conduitUsers conduitDb))
    pure (user, comment)

forArticle :: Connection -> ArticleId -> IO [(User, Comment)]
forArticle conn articleId =
  runBeamPostgres conn $
  runSelectReturningList $
  select $ do
    comment <-
      filter_
        ((val_ articleId ==.) . Comment.article)
        (all_ (conduitComments conduitDb))
    user <-
      filter_
        ((Comment.author comment ==.) . primaryKey)
        (all_ (conduitUsers conduitDb))
    pure (user, comment)
