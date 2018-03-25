module ConduitApp.Comments.Web.Comment
  ( Comment(..)
  , fromPersisted
  ) where

import qualified ConduitApp.Comments.Database.Comment as Persisted
import ConduitApp.Users.Database.User (User)
import ConduitApp.Users.Web.Profile (Profile, fromUser)
import Data.Aeson (ToJSON)
import Data.Int (Int)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Data.Function (const)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))

data Comment = Comment
  { id :: Int
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , body :: Text
  , author :: Profile
  }

deriving instance Generic Comment
deriving instance ToJSON Comment

fromPersisted :: User -> Persisted.Comment -> Comment
fromPersisted user =
  Comment
    <$> Persisted._id
    <*> Persisted.createdAt
    <*> Persisted.updatedAt
    <*> Persisted.body
    <*> const (fromUser user)
