module ConduitApp.Users.Web.Profile
  ( Profile(..)
  , fromUser
  ) where

import ConduitApp.Users.Database.User (User)
import qualified ConduitApp.Users.Database.User as User
import Control.Applicative ((<*>))
import Data.Aeson.Extended (ToJSON)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Text (Text)
import GHC.Generics (Generic)

data Profile = Profile
  { username :: Text
  , bio :: Text
  , image :: Maybe Text
  } deriving (Generic)

deriving instance ToJSON Profile

fromUser :: User -> Profile
fromUser =
  Profile
    <$> User.username
    <*> User.bio
    <*> User.image
