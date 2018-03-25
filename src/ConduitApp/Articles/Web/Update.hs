module ConduitApp.Articles.Web.Update
  ( handler
  , Update
  ) where

import qualified ConduitApp.Articles.Database as Database
import qualified ConduitApp.Articles.Database.Article as Persisted
import ConduitApp.Articles.Database.Attributes
  ( ValidationFailure
  , updateAttributes
  )
import ConduitApp.Articles.Web.Article (Article, fromDecorated)
import ConduitApp.Users.Database.User (User)
import ConduitApp.Web.Auth (RequireAuth)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Applicative (pure)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
  ( ExceptT(ExceptT)
  , runExceptT
  , throwE
  , withExceptT
  )
import Data.Aeson (FromJSON)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Either (Either(Left, Right))
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor ((<$>), void)
import Data.Maybe (Maybe, maybe)
import Data.Pool (withResource)
import Data.Text (Text)
import Database.Beam (primaryKey)
import Database.PostgreSQL.Simple (Connection)
import GHC.Generics (Generic)
import Servant (Handler, err403, err404, err422, errBody, throwError)
import Servant.API ((:>), Capture, JSON, Put, ReqBody)
import System.IO (IO)
import Text.Show (show)
import Data.Set (Set)
import Data.Traversable (traverse)

data Params = Params
  { title :: Maybe Text
  , description :: Maybe Text
  , body :: Maybe Text
  , tagList :: Maybe (Set Text)
  }

deriving instance Generic Params
deriving instance FromJSON Params

type Update =
  RequireAuth :>
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  ReqBody '[JSON] (Namespace "article" Params) :>
  Put '[JSON] (Namespace "article" Article)

data Error
  = NotFound
  | NotAuthorized
  | FailedValidation [ValidationFailure]

handler ::
     Handle
  -> User
  -> Text
  -> Namespace "article" Params
  -> Handler (Namespace "article" Article)
handler handle user slug (Namespace params) = do
  result <- liftIO $ runExceptT $ update handle user slug params
  case result of
    Left NotFound -> throwError err404
    Left NotAuthorized -> throwError err403
    Left (FailedValidation errors) ->
      throwError err422 {errBody = pack (show errors)}
    Right article -> pure . Namespace $ article

load :: Connection -> Text -> ExceptT Error IO Persisted.Article
load conn slug =
  ExceptT (maybe (Left NotFound) Right <$> Database.findBySlug conn slug)

authorize :: User -> Persisted.Article -> ExceptT Error IO ()
authorize user article =
  unless (Persisted.author article == primaryKey user) (throwE NotAuthorized)

update :: Handle -> User -> Text -> Params -> ExceptT Error IO Article
update Handle {connectionPool} user slug params =
  withResource connectionPool $ \conn -> do
    article <- load conn slug
    authorize user article
    attributes <-
      withExceptT FailedValidation $
      updateAttributes
        conn
        article
        (title params)
        (description params)
        (body params)
    lift $ do
      updated <- Database.update conn article attributes
      void $
        traverse
          (Database.replaceTags conn (primaryKey article))
          (tagList params)
      fromDecorated <$> Database.decorate conn user updated
