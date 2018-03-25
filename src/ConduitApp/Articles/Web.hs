module ConduitApp.Articles.Web
  ( ArticlesAPI
  , articlesAPIServer
  ) where

import ConduitApp.Articles.Web.Create (Create)
import qualified ConduitApp.Articles.Web.Create as Create
import ConduitApp.Articles.Web.Favorite (Favorite)
import qualified ConduitApp.Articles.Web.Favorite as Favorite
import ConduitApp.Articles.Web.Unfavorite (Unfavorite)
import qualified ConduitApp.Articles.Web.Unfavorite as Unfavorite
import ConduitApp.Articles.Web.Feed (Feed)
import qualified ConduitApp.Articles.Web.Feed as Feed
import ConduitApp.Articles.Web.QueryAll (QueryAll)
import qualified ConduitApp.Articles.Web.QueryAll as QueryAll
import ConduitApp.Articles.Web.Update (Update)
import qualified ConduitApp.Articles.Web.Update as Update
import ConduitApp.Articles.Web.Destroy (Destroy)
import qualified ConduitApp.Articles.Web.Destroy as Destroy
import ConduitApp.Web.Internal (Handle)
import Servant (Server)
import Servant.API ((:<|>)((:<|>)))

type ArticlesAPI =
  Create :<|>
  Update :<|>
  Destroy :<|>
  QueryAll :<|>
  Feed :<|>
  Favorite :<|>
  Unfavorite

articlesAPIServer :: Handle -> Server ArticlesAPI
articlesAPIServer handle =
  Create.handler handle :<|>
  Update.handler handle :<|>
  Destroy.handler handle :<|>
  QueryAll.handler handle :<|>
  Feed.handler handle :<|>
  Favorite.handler handle :<|>
  Unfavorite.handler handle
