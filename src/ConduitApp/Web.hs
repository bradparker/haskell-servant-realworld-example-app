{-# LANGUAGE TypeOperators #-}

module ConduitApp.Web
  ( conduitAPIApp
  ) where

import ConduitApp.Articles.Web (ArticlesAPI, articlesAPIServer)
import ConduitApp.Users.Web (UsersAPI, usersAPIServer)
import ConduitApp.Tags.Web (TagAPI, tagAPIServer)
import ConduitApp.Comments.Web (CommentsAPI, commentsAPIServer)
import ConduitApp.Web.Auth (optionallyAuthorize, strictlyAuthorize)
import ConduitApp.Web.Internal (Handle(..))
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Servant
  ( (:<|>)((:<|>))
  , Context((:.), EmptyContext)
  , Server
  , serveWithContext
  )

type ConduitAPI = UsersAPI :<|> ArticlesAPI :<|> TagAPI :<|> CommentsAPI

conduitAPI :: Proxy ConduitAPI
conduitAPI = Proxy

conduitAPIServer :: Handle -> Server ConduitAPI
conduitAPIServer handle =
  usersAPIServer handle :<|>
  articlesAPIServer handle :<|>
  tagAPIServer handle :<|>
  commentsAPIServer handle

conduitAPIApp :: Handle -> Application
conduitAPIApp handle =
  serveWithContext
    conduitAPI
    (optionallyAuthorize handle :. strictlyAuthorize handle :. EmptyContext)
    (conduitAPIServer handle)
