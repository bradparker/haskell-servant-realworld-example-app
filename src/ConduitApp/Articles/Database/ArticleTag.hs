{-# LANGUAGE TypeFamilies #-}

module ConduitApp.Articles.Database.ArticleTag
  ( ArticleTagT(..)
  , ArticleTag
  ) where

import ConduitApp.Articles.Database.Article (ArticleT)
import ConduitApp.Tags.Database.Tag (TagT)
import Control.Applicative ((<*>))
import Data.Eq (Eq)
import Data.Functor ((<$>))
import Database.Beam (Beamable, Identity, PrimaryKey, Table(..))
import GHC.Generics (Generic)
import Text.Show (Show)

data ArticleTagT f = ArticleTag
  { articletagArticle :: PrimaryKey ArticleT f
  , articletagTag :: PrimaryKey TagT f
  } deriving (Generic)

type ArticleTag = ArticleTagT Identity

deriving instance Show ArticleTag

deriving instance Eq ArticleTag

instance Beamable ArticleTagT

instance Beamable (PrimaryKey ArticleTagT)

instance Table ArticleTagT where
  data PrimaryKey ArticleTagT f
    = ArticleTagId
        (PrimaryKey ArticleT f)
        (PrimaryKey TagT f)
    deriving Generic
  primaryKey =
    ArticleTagId
      <$> articletagArticle
      <*> articletagTag
