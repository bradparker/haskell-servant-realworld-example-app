module ConduitApp.Users.Web.Account
  ( Account(..)
  , fromUser
  ) where

import ConduitApp.Users.Database.User (User)
import qualified ConduitApp.Users.Database.User as User
import Control.Applicative ((<*>))
import Data.Aeson.Extended
  ( FromJSON(..)
  , ToJSON(..)
  , Value(String)
  , fieldLabelPrefixedBy
  , genericToJSON
  )
import Data.Default (def)
import Data.Function (($), (.))
import Data.Functor ((<$>))
import qualified Data.Map as Map
import Data.Maybe (Maybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Text.Show (Show)
import Web.JWT (Algorithm(HS256), Secret, encodeSigned, unregisteredClaims)

data Account = Account
  { email :: Text
  , token :: Text
  , username :: Text
  , bio :: Text
  , image :: Maybe Text
  } deriving (Show, Generic)

deriving instance ToJSON Account

deriveToken :: Secret -> Text -> Text
deriveToken key username =
  encodeSigned
    HS256
    key
    def {unregisteredClaims = Map.fromList [("username", String username)]}

fromUser :: Secret -> User -> Account
fromUser secret =
  Account
    <$> User.email
    <*> deriveToken secret . User.username
    <*> User.username
    <*> User.bio
    <*> User.image
