module ConduitApp.Users.Database.Attributes
  ( Attributes(..)
  , ValidationFailure(..)
  , insertAttributes
  , updateAttributes
  ) where

import ConduitApp.Attribute (Attribute)
import ConduitApp.Users.Database.Queries (findByEmail, findByUsername)
import ConduitApp.Users.Database.User (User, UserT(..))
import qualified ConduitApp.Users.Database.User as User
import Control.Applicative ((<*>), pure)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Crypto.Scrypt
  ( EncryptedPass(getEncryptedPass)
  , Pass(Pass)
  , encryptPassIO'
  )
import Data.Bool (otherwise)
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Ord ((<))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Traversable (traverse)
import Data.Validation (Validation(Failure, Success), toEither)
import Database.PostgreSQL.Simple (Connection)
import System.IO (IO)
import Text.Show (Show)

data Attributes f = Attributes
  { password :: Attribute f Text
  , email :: Attribute f Text
  , username :: Attribute f Text
  , bio :: Attribute f Text
  , image :: Attribute f (Maybe Text)
  }

data ValidationFailure
  = EmailTaken
  | UsernameTaken
  | PasswordLessThan8Chars

deriving instance Show ValidationFailure

encryptPassword :: Text -> IO Text
encryptPassword pass =
  decodeUtf8 . getEncryptedPass <$> encryptPassIO' (Pass (encodeUtf8 pass))

makePassword :: Text -> Compose IO (Validation [ValidationFailure]) Text
makePassword value
  | Text.length value < 8 = Compose $ pure $ Failure [PasswordLessThan8Chars]
  | otherwise = Compose $ Success <$> encryptPassword value

insertEmail ::
     Connection
  -> Text
  -> Compose IO (Validation [ValidationFailure]) Text
insertEmail conn value =
  Compose $ do
    existing <- findByEmail conn value
    pure $
      case existing of
        Nothing -> Success value
        Just _ -> Failure [EmailTaken]

insertUsername ::
     Connection
  -> Text
  -> Compose IO (Validation [ValidationFailure]) Text
insertUsername conn value =
  Compose $ do
    existing <- findByUsername conn value
    pure $
      case existing of
        Nothing -> Success value
        Just _ -> Failure [EmailTaken]

insertAttributes ::
     Connection
  -> Text
  -> Text
  -> Text
  -> Text
  -> Maybe Text
  -> ExceptT [ValidationFailure] IO (Attributes Identity)
insertAttributes conn password email username bio image =
  ExceptT . (toEither <$>) . getCompose $
  Attributes
    <$> makePassword password
    <*> insertEmail conn email
    <*> insertUsername conn username
    <*> pure bio
    <*> pure image

updateEmail ::
     Connection
  -> User
  -> Text
  -> Compose IO (Validation [ValidationFailure]) Text
updateEmail conn current value
  | value == User.email current = pure value
  | otherwise = insertEmail conn value

updateUsername ::
     Connection
  -> User
  -> Text
  -> Compose IO (Validation [ValidationFailure]) Text
updateUsername conn current value
  | value == User.username current = pure value
  | otherwise = insertUsername conn value

updateAttributes ::
     Connection
  -> User
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> Maybe (Maybe Text)
  -> ExceptT [ValidationFailure] IO (Attributes Maybe)
updateAttributes conn current password email username bio image =
  ExceptT . (toEither <$>) . getCompose $
  Attributes
    <$> traverse makePassword password
    <*> traverse (updateEmail conn current) email
    <*> traverse (updateUsername conn current) username
    <*> pure bio
    <*> pure image
