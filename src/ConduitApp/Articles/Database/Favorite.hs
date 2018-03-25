{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module ConduitApp.Articles.Database.Favorite
  ( FavoriteT(..)
  , Favorite
  , FavoriteId
  ) where

import ConduitApp.Articles.Database.Article (ArticleT)
import ConduitApp.Users.Database.User (UserT)
import Control.Applicative ((<*>))
import Data.Eq (Eq)
import Data.Functor ((<$>))
import Database.Beam (Beamable, Identity, PrimaryKey, Table(..))
import GHC.Generics (Generic)
import Text.Show (Show)

data FavoriteT f = Favorite
  { favoriteArticle :: PrimaryKey ArticleT f
  , favoriteUser :: PrimaryKey UserT f
  }

deriving instance Generic (FavoriteT f)
deriving instance Beamable FavoriteT

type Favorite = FavoriteT Identity

deriving instance Show Favorite

instance Table FavoriteT where
  data PrimaryKey FavoriteT f
    = FavoriteId
        (PrimaryKey ArticleT f)
        (PrimaryKey UserT f)
  primaryKey = FavoriteId <$> favoriteArticle <*> favoriteUser

deriving instance Generic (PrimaryKey FavoriteT f)
deriving instance Beamable (PrimaryKey FavoriteT)

type FavoriteId = PrimaryKey FavoriteT Identity

deriving instance Show FavoriteId
deriving instance Eq FavoriteId
