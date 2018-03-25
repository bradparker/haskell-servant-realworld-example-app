module ConduitApp.Articles.Database.Commands
  ( create
  , update
  , favorite
  , unfavorite
  , assignTags
  , replaceTags
  , destroy
  ) where

import qualified ConduitApp.Articles.Database.Article as Article
import ConduitApp.Articles.Database.Article (Article, ArticleId, ArticleT(..))
import ConduitApp.Articles.Database.ArticleTag (ArticleTagT(..))
import ConduitApp.Articles.Database.Attributes (Attributes(..))
import ConduitApp.Articles.Database.Favorite (Favorite, FavoriteT(..))
import ConduitApp.Database (ConduitDb(..), conduitDb, insertOne)
import qualified ConduitApp.Tags.Database as Tag
import ConduitApp.Users.Database.User (UserId)
import Control.Applicative ((*>))
import Control.Monad (void)
import Data.Function (($), (.), id)
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity)
import Data.List (map)
import Data.Maybe (Maybe, catMaybes, fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Database.Beam
  ( (&&.)
  , (<-.)
  , (==.)
  , default_
  , delete
  , insertExpressions
  , insertValues
  , primaryKey
  , runDelete
  , val_
  )
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamUpdateReturning(..))
import Database.Beam.Postgres (runBeamPostgres)
import Database.Beam.Postgres.Conduit (runInsert)
import Database.Beam.Postgres.Full
  ( conflictingFields
  , insert
  , onConflict
  , onConflictDoNothing
  )
import Database.PostgreSQL.Simple (Connection)
import System.IO (IO)

create :: Connection -> UserId -> Attributes Identity -> IO Article
create conn authorId Attributes{slug, title, description, body} = do
  currentTime <- getCurrentTime
  insertOne
    conn
    (conduitArticles conduitDb)
    (insertExpressions
       [ Article
           { _id = default_
           , slug = val_ slug
           , title = val_ title
           , description = val_ description
           , body = val_ body
           , createdAt = val_ currentTime
           , updatedAt = val_ currentTime
           , author = val_ authorId
           }
       ])

update :: Connection -> Article -> Attributes Maybe -> IO Article
update conn currentArticle Attributes{slug, title, description, body} =
  fromMaybe currentArticle . listToMaybe <$>
  runBeamPostgres
    conn
    (runUpdateReturningList
       (conduitArticles conduitDb)
       (\article ->
          catMaybes
            [ (Article.slug article <-.) . val_ <$> slug
            , (Article.title article <-.) . val_ <$> title
            , (Article.description article <-.) . val_ <$> description
            , (Article.body article <-.) . val_ <$> body
            ])
       (\article -> primaryKey article ==. val_ (primaryKey currentArticle)))

assignTags :: Connection -> ArticleId -> Set Text -> IO ()
assignTags conn article tagNames = do
  tagIds <- map primaryKey <$> Tag.create conn tagNames
  void
    (runInsert
       conn
       (insert
          (conduitArticleTags conduitDb)
          (insertValues (map (ArticleTag article) tagIds))
          (onConflict (conflictingFields id) onConflictDoNothing)))

deleteTags :: Connection -> ArticleId -> IO ()
deleteTags conn article =
  runBeamPostgres conn $
  runDelete $
  delete
    (conduitArticleTags conduitDb)
    (\at -> articletagArticle at ==. val_ article)

replaceTags :: Connection -> ArticleId -> Set Text -> IO ()
replaceTags conn article tagList =
  deleteTags conn article *> assignTags conn article tagList

favorite :: Connection -> ArticleId -> UserId -> IO Favorite
favorite conn article user =
  insertOne
    conn
    (conduitFavorites conduitDb)
    (insertValues [Favorite article user])

unfavorite :: Connection -> ArticleId -> UserId -> IO ()
unfavorite conn article user =
  runBeamPostgres conn $
  runDelete $
  delete (conduitFavorites conduitDb) $ \(Favorite favArticle favUser) ->
    favUser ==. val_ user &&. favArticle ==. val_ article

destroy :: Connection -> ArticleId -> IO ()
destroy conn article =
  runBeamPostgres conn $
  runDelete $
  delete (conduitArticles conduitDb) $ \candidate ->
    primaryKey candidate ==. val_ article
