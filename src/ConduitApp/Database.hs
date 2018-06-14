{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ConduitApp.Database
  ( ConduitDb(..)
  , conduitDb
  , openConduitDb
  , insertOne
  , findBy
  ) where

import qualified ConduitApp.Articles.Database.Article as Article
import ConduitApp.Articles.Database.Article (ArticleT)
import ConduitApp.Articles.Database.ArticleTag (ArticleTagT)
import ConduitApp.Articles.Database.Favorite (FavoriteT)
import qualified ConduitApp.Comments.Database.Comment as Comment
import ConduitApp.Comments.Database.Comment (CommentT)
import ConduitApp.Tags.Database.Tag (TagT)
import ConduitApp.Users.Database.Follow (FollowT)
import ConduitApp.Users.Database.User (UserT)
import Control.Applicative (pure)
import Control.Exception (Exception, throwIO)
import Control.Monad ((<=<), (=<<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Char8 (pack)
import Data.Function (($), (.), id)
import Data.Functor (fmap)
import Data.Maybe (Maybe, listToMaybe, maybe)
import Database.Beam
  ( Beamable
  , Database
  , DatabaseEntity
  , DatabaseSettings
  , HasSqlEqualityCheck
  , Identity
  , Q
  , QExpr
  , SqlInsertValues
  , TableEntity
  , (==.)
  , dbModification
  , defaultDbSettings
  , fieldNamed
  , filter_
  , modifyTable
  , runSelectReturningOne
  , select
  , tableModification
  , val_
  , withDbModification
  )
import Database.Beam.Backend.SQL.BeamExtensions (MonadBeamInsertReturning(..))
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax)
import Database.Beam.Backend.Types (Exposed, FromBackendRow)
import Database.Beam.Postgres (Postgres, runBeamPostgres)
import Database.Beam.Postgres.Syntax
  ( PgExpressionSyntax
  , PgInsertValuesSyntax
  , PgSelectSyntax
  , PgValueSyntax
  )
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import GHC.Generics (Generic)
import System.Environment (getEnv)
import System.IO (IO)
import Text.Show (Show)

data ConduitDb f = ConduitDb
  { conduitArticles :: f (TableEntity ArticleT)
  , conduitFavorites :: f (TableEntity FavoriteT)
  , conduitUsers :: f (TableEntity UserT)
  , conduitFollows :: f (TableEntity FollowT)
  , conduitComments :: f (TableEntity CommentT)
  , conduitTags :: f (TableEntity TagT)
  , conduitArticleTags :: f (TableEntity ArticleTagT)
  } deriving (Generic)

instance Database Postgres ConduitDb

conduitDb :: DatabaseSettings Postgres ConduitDb
conduitDb =
  defaultDbSettings `withDbModification`
  dbModification
    { conduitArticles =
        modifyTable id $
        tableModification
          { Article.createdAt = fieldNamed "created_at"
          , Article.updatedAt = fieldNamed "updated_at"
          }
    , conduitComments =
        modifyTable id $
        tableModification
          { Comment.createdAt = fieldNamed "created_at"
          , Comment.updatedAt = fieldNamed "updated_at"
          }
    }

openConduitDb :: MonadIO m => m Connection
openConduitDb = liftIO (connectPostgreSQL . pack =<< getEnv "DATABASE_URL")

data UnexpectedEmptyReturn =
  UnexpectedEmptyReturn
  deriving (Show)

instance Exception UnexpectedEmptyReturn

insertOne ::
     ( FromBackendRow Postgres (table Identity)
     , Generic (table Exposed)
     , Generic (table Identity)
     , Beamable table
     , MonadBeamInsertReturning syntax Postgres Connection m
     )
  => Connection
  -> DatabaseEntity Postgres ConduitDb (TableEntity table)
  -> SqlInsertValues PgInsertValuesSyntax (table (QExpr PgExpressionSyntax s))
  -> IO (table Identity)
insertOne conn table =
  maybe (throwIO UnexpectedEmptyReturn) pure <=<
  fmap listToMaybe . runBeamPostgres conn . runInsertReturningList table

findBy ::
     ( HasSqlValueSyntax PgValueSyntax a
     , HasSqlEqualityCheck PgExpressionSyntax a
     , FromBackendRow Postgres (table Identity)
     , Generic (table Exposed)
     , Generic (table Identity)
     , Beamable table
     )
  => Connection
  -> Q PgSelectSyntax ConduitDb _ (table (QExpr PgExpressionSyntax s))
  -> (table (QExpr PgExpressionSyntax s) -> QExpr PgExpressionSyntax _ a)
  -> a
  -> IO (Maybe (table Identity))
findBy conn scope column val =
  runBeamPostgres conn $
  runSelectReturningOne $ select $ filter_ ((val_ val ==.) . column) scope
