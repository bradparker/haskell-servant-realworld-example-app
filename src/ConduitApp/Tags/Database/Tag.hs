module ConduitApp.Tags.Database.Tag
  ( TagT(..)
  , Tag
  , TagId
  , PrimaryKey(..)
  ) where

import Data.Eq (Eq)
import Data.Function ((.))
import Data.Int (Int)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table(..))
import GHC.Generics (Generic)
import Text.Show (Show)

data TagT f = Tag
  { tagId :: Columnar f Int
  , tagName :: Columnar f Text
  }

deriving instance Generic (TagT f)
deriving instance Beamable TagT

type Tag = TagT Identity

deriving instance Show Tag
deriving instance Eq Tag

instance Table TagT where
  data PrimaryKey TagT f = TagId
    { unTagId :: Columnar f Int
    }
  primaryKey = TagId . tagId

deriving instance Generic (PrimaryKey TagT f)
deriving instance Beamable (PrimaryKey TagT)

type TagId = PrimaryKey TagT Identity

deriving instance Show TagId
deriving instance Eq TagId
