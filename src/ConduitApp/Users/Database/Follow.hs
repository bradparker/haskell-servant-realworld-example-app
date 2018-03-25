{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

module ConduitApp.Users.Database.Follow
  ( FollowT(..)
  , Follow
  ) where

import ConduitApp.Users.Database.User (UserT)
import Control.Applicative ((<*>))
import Data.Functor ((<$>))
import Database.Beam (Beamable, Identity, PrimaryKey, Table(..))
import GHC.Generics (Generic)

data FollowT f = Follow
  { followFollower :: PrimaryKey UserT f
  , followFollowee :: PrimaryKey UserT f
  }

deriving instance Generic (FollowT f)
deriving instance Beamable FollowT

type Follow = FollowT Identity

instance Table FollowT where
  data PrimaryKey FollowT f
    = FollowId
        (PrimaryKey UserT f)
        (PrimaryKey UserT f)
  primaryKey = FollowId <$> followFollower <*> followFollowee

deriving instance Generic (PrimaryKey FollowT f)
deriving instance Beamable (PrimaryKey FollowT)
