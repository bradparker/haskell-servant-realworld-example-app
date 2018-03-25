module ConduitApp.Articles.Web.Article
  ( Article(..)
  , fromDecorated
  ) where

import qualified ConduitApp.Articles.Database.Article as Persisted
import ConduitApp.Articles.Database.Decorated (Decorated(..))
import qualified ConduitApp.Articles.Database.Decorated as Decorated
import ConduitApp.Users.Web.Profile (Profile, fromUser)
import Control.Applicative ((<*>))
import Data.Aeson.Extended (ToJSON(..))
import Data.Bool (Bool)
import Data.Function ((.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data Article = Article
  { slug :: Text
  , title :: Text
  , description :: Text
  , body :: Text
  , tagList :: [Text]
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , favorited :: Bool
  , favoritesCount :: Int
  , author :: Profile
  }

deriving instance Generic Article
deriving instance ToJSON Article

fromDecorated :: Decorated -> Article
fromDecorated =
  Article
    <$> Persisted.slug . Decorated.article
    <*> Persisted.title . Decorated.article
    <*> Persisted.description . Decorated.article
    <*> Persisted.body . Decorated.article
    <*> Decorated.tagList
    <*> Persisted.createdAt . Decorated.article
    <*> Persisted.updatedAt . Decorated.article
    <*> Decorated.favorited
    <*> Decorated.favoriteCount
    <*> fromUser . Decorated.author
