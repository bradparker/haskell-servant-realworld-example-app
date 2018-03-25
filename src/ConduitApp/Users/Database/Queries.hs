module ConduitApp.Users.Database.Queries
  ( findByEmail
  , findByUsername
  , followersAndFollowees
  , findByCredentials
  , Credentials(..)
  ) where

import ConduitApp.Database
  ( ConduitDb(conduitUsers, conduitFollows)
  , conduitDb
  , findBy
  )
import ConduitApp.Users.Database.User (User, UserT(..))
import Control.Applicative ((<*>), pure)
import Crypto.Scrypt
  ( EncryptedPass(EncryptedPass)
  , Pass(Pass)
  , verifyPass'
  )
import Data.Aeson.Extended (FromJSON(..), (.:), withObject)
import Data.Bool (Bool(False))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing), maybe)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import ConduitApp.Users.Database.Follow (FollowT(..))
import Database.Beam
  ( Q
  , QExpr
  , all_
  , manyToMany_
  )
import Database.Beam.Postgres.Syntax (PgExpressionSyntax, PgSelectSyntax)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import System.IO (IO)

findByEmail :: Connection -> Text -> IO (Maybe User)
findByEmail conn = findBy conn (all_ (conduitUsers conduitDb)) email

findByUsername :: Connection -> Text -> IO (Maybe User)
findByUsername conn = findBy conn (all_ (conduitUsers conduitDb)) username

data Credentials = Credentials
  { credentialsEmail :: Text
  , credentialsPassword :: Text
  } deriving (Generic)

instance FromJSON Credentials where
  parseJSON =
    withObject "Credentials" $ \v ->
      Credentials <$> v .: "email" <*> v .: "password"

encryptedPassMatches :: Text -> Text -> Bool
encryptedPassMatches a b =
  verifyPass' (Pass (encodeUtf8 a)) (EncryptedPass (encodeUtf8 b))

findByCredentials :: Connection -> Credentials -> IO (Maybe User)
findByCredentials conn Credentials {credentialsEmail, credentialsPassword} = do
  found <- findByEmail conn credentialsEmail
  if maybe False (encryptedPassMatches credentialsPassword . password) found
    then pure found
    else pure Nothing

followersAndFollowees ::
     Q PgSelectSyntax ConduitDb s ( UserT (QExpr PgExpressionSyntax s)
                                  , UserT (QExpr PgExpressionSyntax s))
followersAndFollowees =
  manyToMany_
    (conduitFollows conduitDb)
    followFollower
    followFollowee
    (all_ (conduitUsers conduitDb))
    (all_ (conduitUsers conduitDb))
