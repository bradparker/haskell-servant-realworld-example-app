module ConduitApp.Articles.Web.Unfavorite
  ( handler
  , Unfavorite
  ) where

import qualified ConduitApp.Articles.Database as Database
import ConduitApp.Articles.Web.Article (Article, fromDecorated)
import ConduitApp.Users.Database.User (User)
import ConduitApp.Web.Auth (RequireAuth)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), maybeToExceptT)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Functor ((<$>), void)
import Data.Pool (withResource)
import Data.Text (Text)
import Database.Beam (primaryKey)
import Servant (Handler, err404, throwError)
import Servant.API ((:>), Capture, Delete, JSON)
import System.IO (IO)

type Unfavorite =
  RequireAuth :>
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "favorite" :>
  Delete '[JSON] (Namespace "article" Article)

handler :: Handle -> User -> Text -> Handler (Namespace "article" Article)
handler handle user slug = do
  result <- liftIO $ runExceptT $ unfavorite handle user slug
  case result of
    Left NotFound -> throwError err404
    Right article -> pure . Namespace $ article

data Error =
  NotFound

unfavorite :: Handle -> User -> Text -> ExceptT Error IO Article
unfavorite Handle {connectionPool} user slug =
  maybeToExceptT NotFound $
  withResource connectionPool $ \conn -> do
    article <- MaybeT $ Database.findBySlug conn slug
    lift $ do
      void $ Database.unfavorite conn (primaryKey article) (primaryKey user)
      fromDecorated <$> Database.decorate conn user article
