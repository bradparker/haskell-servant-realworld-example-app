{-# LANGUAGE PartialTypeSignatures #-}

module ConduitApp.Articles.Database.Queries
  ( QueryParams(..)
  , query
  , decorate
  , feed
  , findBySlug
  , findByTitle
  ) where

import ConduitApp.Articles.Database.Article (Article, ArticleT(..))
import qualified ConduitApp.Articles.Database.Article as Article
import ConduitApp.Articles.Database.ArticleTag (ArticleTagT(..))
import ConduitApp.Articles.Database.Decorated (Decorated(..))
import ConduitApp.Articles.Database.Favorite (FavoriteT(..))
import ConduitApp.Database (ConduitDb(..), conduitDb, findBy)
import ConduitApp.Tags.Database.Tag (TagT(..))
import ConduitApp.Users.Database (followersAndFollowees)
import ConduitApp.Users.Database.User
  ( PrimaryKey(unUserId)
  , User
  , UserT(username)
  )
import Control.Applicative ((<*>), pure)
import Control.Lens (_1, _2, _3, _4, _5, view)
import Control.Monad ((=<<), unless)
import Data.Bool (Bool(False), otherwise)
import Data.Eq ((/=))
import Data.Foldable (foldr, null)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (map)
import Data.Maybe (Maybe(Just), fromMaybe, listToMaybe, maybe, maybeToList)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.Tuple (fst, snd)
import Database.Beam
  ( ManyToMany
  , Nullable
  , Q
  , QExpr
  , (&&.)
  , (==.)
  , aggregate_
  , all_
  , count_
  , desc_
  , filter_
  , group_
  , guard_
  , in_
  , just_
  , leftJoin_
  , limit_
  , manyToMany_
  , offset_
  , orderBy_
  , primaryKey
  , references_
  , runSelectReturningList
  , select
  , val_
  )
import Database.Beam.Postgres (runBeamPostgres)
import Database.Beam.Postgres.Syntax (PgExpressionSyntax, PgSelectSyntax)
import Database.PostgreSQL.Simple (Connection)
import Prelude (Integer)
import System.IO (IO)
import Text.Show (Show)

findByTitle :: Connection -> Text -> IO (Maybe Article)
findByTitle conn = findBy conn (all_ (conduitArticles conduitDb)) Article.title

findBySlug :: Connection -> Text -> IO (Maybe Article)
findBySlug conn = findBy conn (all_ (conduitArticles conduitDb)) Article.slug

data QueryParams = QueryParams
  { queryParamsLimit :: Integer
  , queryParamsOffset :: Integer
  , queryParamsTags :: [Text]
  , queryParamsAuthors :: [Text]
  , queryParamsFavorited :: [Text]
  } deriving (Show)

type QueryExpression s = QExpr PgExpressionSyntax s

authors ::
     ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (UserT (QueryExpression s))
authors Article{author} =
  filter_
    ((author ==.) . primaryKey)
    (all_ (conduitUsers conduitDb))

articletagRelationship :: ManyToMany ConduitDb ArticleT TagT
articletagRelationship =
  manyToMany_ (conduitArticleTags conduitDb) articletagArticle articletagTag

articlesAndTags ::
     Q PgSelectSyntax ConduitDb s ( ArticleT (QExpr PgExpressionSyntax s)
                                  , TagT (QExpr PgExpressionSyntax s))
articlesAndTags =
  articletagRelationship
    (all_ (conduitArticles conduitDb))
    (all_ (conduitTags conduitDb))

tags ::
     ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (TagT (Nullable (QueryExpression s)))
tags article =
  snd <$>
  leftJoin_ articlesAndTags ((primaryKey article ==.) . primaryKey . fst)

favorites ::
     ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (FavoriteT (Nullable (QueryExpression s)))
favorites article =
  leftJoin_
    (all_ (conduitFavorites conduitDb))
    ((`references_` article) . favoriteArticle)

byAuthors ::
     [Text]
  -> ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (ArticleT (QueryExpression s))
byAuthors usernames article = do
  unless (null usernames) $ do
    author <- authors article
    guard_ (username author `in_` map val_ usernames)
  pure article

taggedWith ::
     [Text]
  -> ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (ArticleT (QueryExpression s))
taggedWith tagNames article = do
  unless (null tagNames) $ do
    tag <- tags article
    guard_ (tagName tag `in_` map (just_ . val_) tagNames)
  pure article

favoritedBy ::
     [Text]
  -> ArticleT (QueryExpression s)
  -> Q PgSelectSyntax ConduitDb s (ArticleT (QueryExpression s))
favoritedBy usernames article = do
  unless (null usernames) $ do
    fav <- favorites article
    user <- all_ (conduitUsers conduitDb)
    guard_
      (username user `in_` map val_ usernames &&. just_ (primaryKey user) ==.
       favoriteUser fav)
  pure article

allMatching ::
     QueryParams -> Q PgSelectSyntax ConduitDb s (ArticleT (QueryExpression s))
allMatching (QueryParams limit offset tagNames authorNames usersFavorited) =
  favoritedBy usersFavorited =<<
  taggedWith tagNames =<<
  byAuthors authorNames =<<
  orderBy_
    (desc_ . createdAt)
    (limit_ limit (offset_ offset (all_ (conduitArticles conduitDb))))

query ::
     Connection
  -> Maybe User
  -> QueryParams
  -> IO [Decorated]
query conn currentUser = findDecorated conn currentUser . allMatching

decorate ::
     Connection -> User -> Article -> IO Decorated
decorate conn user article =
  fromMaybe (Decorated article user [] 0 False) . listToMaybe <$>
  findDecorated
    conn
    (Just user)
    (filter_
       ((val_ (primaryKey article) ==.) . primaryKey)
       (all_ (conduitArticles conduitDb)))

byFollowing ::
     User
  -> Integer
  -> Integer
  -> Q PgSelectSyntax ConduitDb s (ArticleT (QueryExpression s))
byFollowing user limit offset = do
  article <- allMatching (QueryParams limit offset [] [] [])
  following <-
    snd <$>
    filter_
      ((val_ (primaryKey user) ==.) . primaryKey . fst)
      followersAndFollowees
  guard_ (Article.author article ==. primaryKey following)
  pure article

feed :: Connection -> User -> Integer -> Integer -> IO [Decorated]
feed conn user limit offset =
  findDecorated conn (Just user) (byFollowing user limit offset)

aggregateRows :: [(Article, User, Maybe Text, Int, Bool)] -> [Decorated]
aggregateRows = foldr (aggregate . toDecorated) []
  where
    toDecorated =
      Decorated
        <$> view _1
        <*> view _2
        <*> maybeToList . view _3
        <*> view _4
        <*> view _5
    aggregate a [] = [a]
    aggregate a@Decorated {article = artA} ~(b@Decorated {article = artB}:bs)
      | artA /= artB = a : b : bs
      | otherwise = a <> b : bs

findDecorated ::
     Connection
  -> Maybe User
  -> Q PgSelectSyntax ConduitDb _ (ArticleT (QueryExpression _))
  -> IO [Decorated]
findDecorated conn currentUser scope =
  aggregateRows <$>
  runBeamPostgres
    conn
    (runSelectReturningList $
     select $
     aggregate_
       (\(article, author, tag, fav, currentUserFavorited) ->
          ( group_ article
          , group_ author
          , group_ (tagName tag)
          , count_ (unUserId (favoriteUser fav))
          , group_ currentUserFavorited)) $ do
       article <- scope
       author <- authors article
       tag <- tags article
       fav <- favorites article
       pure
         ( article
         , author
         , tag
         , fav
         , maybe
             (val_ False)
             ((favoriteUser fav ==.) . just_ . val_ . primaryKey)
             currentUser))
