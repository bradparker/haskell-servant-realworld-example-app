module ConduitApp.Comments.Database.Comment
  ( CommentT(..)
  , Comment
  , CommentId
  , PrimaryKey(CommentId, unCommentId)
  ) where

import ConduitApp.Articles.Database.Article (ArticleT)
import ConduitApp.Users.Database.User (UserT)
import Data.Function ((.))
import Data.Int (Int)
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table(..))
import GHC.Generics (Generic)
import Text.Show (Show)

data CommentT f = Comment
  { _id :: Columnar f Int
  , createdAt :: Columnar f UTCTime
  , updatedAt :: Columnar f UTCTime
  , body :: Columnar f Text
  , article :: PrimaryKey ArticleT f
  , author :: PrimaryKey UserT f
  }

deriving instance Generic (CommentT f)
deriving instance Beamable CommentT

type Comment = CommentT Identity

deriving instance Show Comment

instance Table CommentT where
  data PrimaryKey CommentT f = CommentId
    { unCommentId :: Columnar f Int
    }
  primaryKey = CommentId . _id

deriving instance Generic (PrimaryKey CommentT f)
deriving instance Beamable (PrimaryKey CommentT)

type CommentId = PrimaryKey CommentT Identity
