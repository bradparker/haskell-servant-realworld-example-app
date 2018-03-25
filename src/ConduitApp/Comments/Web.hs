module ConduitApp.Comments.Web
  ( commentsAPIServer
  , CommentsAPI
  ) where

import qualified ConduitApp.Articles.Database as Articles
import qualified ConduitApp.Comments.Database as Database
import ConduitApp.Comments.Database.Comment (PrimaryKey(..))
import ConduitApp.Comments.Web.Comment (Comment, fromPersisted)
import ConduitApp.Users.Database.User (User)
import ConduitApp.Web.Auth (RequireAuth)
import ConduitApp.Web.Internal (Handle(..))
import ConduitApp.Web.Namespace (Namespace(Namespace))
import Control.Applicative (pure)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Data.Aeson (FromJSON)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Pool (withResource)
import Data.Text (Text)
import Data.Tuple (uncurry)
import Database.Beam (primaryKey)
import GHC.Generics (Generic)
import Servant (Handler, Server, err404, throwError)
import Servant.API
  ( (:<|>)((:<|>))
  , (:>)
  , Capture
  , Delete
  , Get
  , JSON
  , PostCreated
  , ReqBody
  )

newtype Attributes = Attributes { body :: Text }

deriving instance Generic Attributes
deriving instance FromJSON Attributes

type Create =
  RequireAuth :>
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "comments" :>
  ReqBody '[JSON] (Namespace "comment" Attributes) :>
  PostCreated '[JSON] (Namespace "comment" Comment)

create ::
     Handle
  -> User
  -> Text
  -> Namespace "comment" Attributes
  -> Handler (Namespace "comment" Comment)
create Handle {connectionPool} user slug (Namespace Attributes {body}) = do
  result <-
    liftIO $
    runMaybeT $
    withResource connectionPool $ \conn -> do
      article <- MaybeT $ Articles.findBySlug conn slug
      lift $
        fromPersisted user <$>
        Database.create conn (primaryKey user) (primaryKey article) body
  case result of
    Nothing -> throwError err404
    Just created -> pure $ Namespace created

type Destroy =
  RequireAuth :>
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "comments" :>
  Capture "id" Int :>
  Delete '[JSON] (Namespace "comment" Comment)

destroy ::
     Handle
  -> User
  -> Text
  -> Int
  -> Handler (Namespace "comment" Comment)
destroy Handle {connectionPool} user _ commentId = do
  result <-
    liftIO $
    runMaybeT $
    withResource connectionPool $ \conn -> do
      (_, comment) <- MaybeT $ Database.find conn (CommentId commentId)
      lift $ do
        Database.destroy conn (CommentId commentId)
        pure $ fromPersisted user comment
  case result of
    Nothing -> throwError err404
    Just destroyed -> pure $ Namespace destroyed

type ForArticle =
  "api" :>
  "articles" :>
  Capture "slug" Text :>
  "comments" :>
  Get '[JSON] (Namespace "comments" [Comment])

forArticle :: Handle -> Text -> Handler (Namespace "comments" [Comment])
forArticle Handle {connectionPool} articleSlug = do
  result <-
    liftIO $
    runMaybeT $
    withResource connectionPool $ \conn -> do
      article <- MaybeT $ Articles.findBySlug conn articleSlug
      lift $ Database.forArticle conn (primaryKey article)
  case result of
    Nothing -> throwError err404
    Just comments -> pure $ Namespace (uncurry fromPersisted <$> comments)

type CommentsAPI = Create :<|> Destroy :<|> ForArticle

commentsAPIServer :: Handle -> Server CommentsAPI
commentsAPIServer handle =
  create handle :<|>
  destroy handle :<|>
  forArticle handle
