{-# LANGUAGE QuasiQuotes #-}

module ConduitApp.WebSpec
  ( spec
  ) where

import ConduitApp.Spec.Web (withHandle)
import ConduitApp.Web (conduitAPIApp)
import Control.Lens ((^.), (^?))
import Control.Monad (forM, mapM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens
  ( Primitive(NullPrim)
  , _Array
  , _Bool
  , _Primitive
  , _String
  , _Number
  , key
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (map)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as Vector
import Network.HTTP.Types
  ( Header
  , hAuthorization
  , hContentType
  , methodDelete
  , methodGet
  , methodPost
  , methodPut
  , status200
  , status201
  , status400
  , status401
  , status403
  , status422
  )
import Network.Wai (Application)
import Network.Wai.Test (SResponse(simpleBody, simpleStatus))
import Test.Hspec
  ( ActionWith
  , Spec
  , around
  , context
  , describe
  , it
  , shouldBe
  , shouldSatisfy
  )
import Test.Hspec.Wai (WaiSession, request)
import Test.Hspec.Wai.JSON (json)

post :: ByteString -> [Header] -> LB.ByteString -> WaiSession SResponse
post path headers =
  request methodPost path ((hContentType, "application/json") : headers)

put :: ByteString -> [Header] -> LB.ByteString -> WaiSession SResponse
put path headers =
  request methodPut path ((hContentType, "application/json") : headers)

get :: ByteString -> [Header] -> WaiSession SResponse
get path headers =
  request methodGet path ((hContentType, "application/json") : headers) ""

delete :: ByteString -> [Header] -> WaiSession SResponse
delete path headers =
  request methodDelete path ((hContentType, "application/json") : headers) ""

withApp :: ActionWith Application -> IO ()
withApp action = withHandle (action . conduitAPIApp)

isUserResponse :: LB.ByteString -> IO ()
isUserResponse body = do
  body ^. key "user" . key "email" . _String `shouldBe` "e@mail.com"
  body ^. key "user" . key "username" . _String `shouldBe` "aname"
  body ^? key "user" . key "bio" . _String `shouldBe` Just ""
  body ^? key "user" . key "image" . _Primitive `shouldBe` Just NullPrim
  body ^? key "user" . key "token" . _String `shouldSatisfy` isJust

createUser :: Text -> Text -> Text -> WaiSession LB.ByteString
createUser name email password =
  simpleBody <$>
  post
    "/api/users"
    []
    [json|{
      user: {
        username: #{name},
        email: #{email},
        password: #{password}
      }
    }|]

createUserDefault :: WaiSession LB.ByteString
createUserDefault = createUser "aname" "e@mail.com" "secret123"

createArticle ::
     Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> [Text]
  -> WaiSession LB.ByteString
createArticle name email password title desc body tagList = do
  author <- createUser name email password
  simpleBody <$>
    post
      "/api/articles"
      [ ( hAuthorization
        , encodeUtf8 ("Token " <> author ^. key "user" . key "token" . _String))
      ]
      [json|{
        article: {
          title: #{title},
          description: #{desc},
          body: #{body},
          tagList: #{tagList}
        }
      }|]

createArticleDefault :: WaiSession LB.ByteString
createArticleDefault =
  createArticle
    "User"
    "user@user.com"
    "foobar123"
    "Title"
    "Description"
    "Body"
    []

createArticles :: Int -> Text -> [Text] -> WaiSession [LB.ByteString]
createArticles count prefix tagList =
  forM
    (map (pack . show) [1 .. count])
    (\n ->
       createArticle
         (prefix <> "-user-" <> n)
         (prefix <> "-user-" <> n <> "@example.com")
         "password123"
         (prefix <> " Article " <> n)
         ("Description " <> n)
         ("Body " <> n)
         tagList)

spec :: Spec
spec =
  around withApp $ do
    describe "POST /api/users" $ do
      context "when provided a valid body" $
        it "responds with 201 and a json encoded user response" $ do
          res <-
            post
              "/api/users"
              []
              [json|{
                user: {
                  username: "aname",
                  email: "e@mail.com",
                  password: "secret123"
                }
              }|]
          liftIO $ do
            let status = simpleStatus res
                body = simpleBody res
            status `shouldBe` status201
            isUserResponse body

      context "when provided an invalid body" $
        it "responds with a 400" $ do
          res <-
            post
              "/api/users"
              []
              [json|{
                user: {
                  wrong: "params"
                }
              }|]
          liftIO $ do
            let status = simpleStatus res
                body = simpleBody res
            status `shouldBe` status400
            body `shouldBe` "Error in $.user: key \"password\" not present"

    describe "POST /api/users/login" $ do
      context "when provided the correct credentials" $
        it "responds with 200 and a json encoded user response" $ do
          void createUserDefault
          res <-
            post
              "/api/users/login"
              []
              [json|{
              user: {
                email: "e@mail.com",
                password: "secret123"
              }
            }|]
          liftIO $ do
            let status = simpleStatus res
                body = simpleBody res
            status `shouldBe` status200
            isUserResponse body

      context "when provided incorrect credentials" $
        it "responds with a 401" $ do
          void createUserDefault
          res <-
            post
              "/api/users/login"
              []
              [json|{
              user: {
                email: "e@mail.com",
                password: "wrong just wrong"
              }
            }|]
          liftIO $ simpleStatus res `shouldBe` status401

    describe "GET /api/user" $ do
      context "when provided a valid token" $
        it "responds with 200 and the authorized user" $ do
          user <- createUserDefault
          res <-
            get
              "/api/user"
              [ ( hAuthorization
                , encodeUtf8
                    ("Token " <> user ^. key "user" . key "token" . _String))
              ]
          liftIO $ do
            simpleStatus res `shouldBe` status200
            isUserResponse (simpleBody res)

      context "when provided an invalid token" $
        it "responds with 403" $ do
          void createUserDefault
          res <- get "/api/user" [(hAuthorization, "Token invalid")]
          liftIO $ simpleStatus res `shouldBe` status403

    describe "PUT /api/user" $ context "when provided a valid body" $
      it "responds with 200 and a json encoded user response" $ do
        user <- createUserDefault
        res <-
          put
            "/api/user"
            [ ( hAuthorization
              , encodeUtf8
                  ("Token " <> user ^. key "user" . key "token" . _String))
            ]
            [json|{
              user: {
                email: "changed@mail.com"
              }
            }|]
        liftIO $ do
          let status = simpleStatus res
              body = simpleBody res
          status `shouldBe` status200
          body ^. key "user" . key "email" . _String `shouldBe` "changed@mail.com"

    describe "GET /api/profiles/:username" $
      it "gets the specified profile" $ do
        user <- createUser "pro" "profile@example.com" "superSecret"
        res <-
          get
            ("/api/profiles/" <>
             encodeUtf8 (user ^. key "user" . key "username" . _String))
            []
        liftIO $ simpleStatus res `shouldBe` status200

    describe "POST /api/profiles/:username/follow" $
      it "follows the specified user" $ do
        follower <- createUser "follower" "follow@er.com" "secret123"
        void $ createUser "followee" "follow@ee.com" "secret123"
        let token =
              encodeUtf8
                ("Token " <> follower ^. key "user" . key "token" . _String)
        res <- post "/api/profiles/followee/follow" [(hAuthorization, token)] ""
        liftIO $ simpleStatus res `shouldBe` status200

    describe "DELETE /api/profiles/:username/follow" $
      it "unfollows the specified user" $ do
        follower <- createUser "follower" "follow@er.com" "secret123"
        void $ createUser "followee" "follow@ee.com" "secret123"
        let token =
              encodeUtf8
                ("Token " <> follower ^. key "user" . key "token" . _String)
        void $ post "/api/profiles/followee/follow" [(hAuthorization, token)] ""
        res <- delete "/api/profiles/followee/follow" [(hAuthorization, token)]
        liftIO $ simpleStatus res `shouldBe` status200

    describe "POST /api/articles" $ do
      context "when provided an invalid user" $
        it "responds 403" $ do
          res <-
            post
              "/api/articles"
              [(hAuthorization, "Token foobar")]
              [json|{
                article: {
                  title: "A title",
                  description: "A thing",
                  body: "A longer thing",
                  tagList: ["cats", "dogs", "etc"]
                }
              }|]
          liftIO $ simpleStatus res `shouldBe` status403

      context "when provided an invalid payload" $
        it "responds 422" $ do
          user <- createUserDefault
          void $
            post
              "/api/articles"
              [ ( hAuthorization
                , encodeUtf8
                    ("Token " <> user ^. key "user" . key "token" . _String))
              ]
              [json|{
                article: {
                  title: "A title",
                  description: "A thing",
                  body: "A longer thing",
                  tagList: ["cats", "dogs", "etc"]
                }
              }|]
          res <-
            post
              "/api/articles"
              [ ( hAuthorization
                , encodeUtf8
                    ("Token " <> user ^. key "user" . key "token" . _String))
              ]
              [json|{
                article: {
                  title: "A title",
                  description: "A different thing",
                  body: "A longer, but still, different thing",
                  tagList: ["cats", "dogs", "etc"]
                }
              }|]
          liftIO $ do
            let status = simpleStatus res
                body = simpleBody res
            status `shouldBe` status422
            body `shouldBe`
              "[TitleWouldProduceDuplicateSlug \"a-title\"]"

      context "when provided a valid body and user" $
        it "responds with 201 and a json encoded article response" $ do
          user <- createUserDefault
          res <-
            post
              "/api/articles"
              [ ( hAuthorization
                , encodeUtf8
                    ("Token " <> user ^. key "user" . key "token" . _String))
              ]
              [json|{
                article: {
                  title: "A title",
                  description: "A thing",
                  body: "A longer thing",
                  tagList: ["cats", "dogs", "etc"]
                }
              }|]
          liftIO $ do
            let status = simpleStatus res
                body = simpleBody res
            status `shouldBe` status201
            body ^. key "article" . key "title" . _String `shouldBe` "A title"
            body ^. key "article" . key "description" . _String `shouldBe`
              "A thing"
            body ^. key "article" . key "body" . _String `shouldBe`
              "A longer thing"
            body ^. key "article" . key "tagList" . _Array `shouldBe`
              Vector.fromList ["cats", "dogs", "etc"]

    describe "PUT /api/articles/:slug" $ do
      context "when provided a valid body and an invalid user" $
        it "responds with 401" $ do
        user <- createUserDefault
        author <- createUser "author" "author@example.com" "secret123"
        void $
          post
            "/api/articles"
            [ ( hAuthorization
              , encodeUtf8
                  ("Token " <> author ^. key "user" . key "token" . _String))
            ]
            [json|{
              article: {
                title: "A title",
                description: "A thing",
                body: "A longer thing",
                tagList: ["cats", "dogs", "etc"]
              }
            }|]
        res <-
          put
            "/api/articles/a-title"
            [ ( hAuthorization
              , encodeUtf8
                  ("Token " <> user ^. key "user" . key "token" . _String))
            ]
            [json|{
              article: {
                title: "A changed title"
              }
            }|]
        liftIO $ simpleStatus res `shouldBe` status403

      context "when provided a valid body and user" $
        it "responds with 200 and a json encoded article response" $ do
          user <- createUserDefault
          void $
            post
              "/api/articles"
              [ ( hAuthorization
                , encodeUtf8
                    ("Token " <> user ^. key "user" . key "token" . _String))
              ]
              [json|{
                article: {
                  title: "A title",
                  description: "A thing",
                  body: "A longer thing",
                  tagList: ["cats", "dogs", "etc"]
                }
              }|]
          res <-
            put
              "/api/articles/a-title"
              [ ( hAuthorization
                , encodeUtf8
                    ("Token " <> user ^. key "user" . key "token" . _String))
              ]
              [json|{
                article: {
                  title: "A changed title"
                }
              }|]
          liftIO $ do
            let status = simpleStatus res
                body = simpleBody res
            status `shouldBe` status200
            body ^. key "article" . key "title" . _String `shouldBe`
              "A changed title"

    describe "POST /api/articles/:slug/favorite" $
      it "creates a favorite for the requested article an current user" $ do
        user <- createUserDefault
        let token =
              encodeUtf8 ("Token " <> user ^. key "user" . key "token" . _String)
        article <- createArticleDefault
        let slug = encodeUtf8 (article ^. key "article" . key "slug" . _String)
        res <-
          post
            ("/api/articles/" <> slug <> "/favorite")
            [(hAuthorization, token)]
            ""
        liftIO $ simpleStatus res `shouldBe` status200

    describe "DELETE /api/articles/:slug/favorite" $
      it "unfavorites the specified article" $ do
        user <- createUserDefault
        let token =
              encodeUtf8 ("Token " <> user ^. key "user" . key "token" . _String)
        article <- createArticleDefault
        let slug = encodeUtf8 (article ^. key "article" . key "slug" . _String)
        void $ post
          ("/api/articles/" <> slug <> "/favorite")
          [(hAuthorization, token)]
          ""
        res <-
          delete
            ("/api/articles/" <> slug <> "/favorite")
            [(hAuthorization, token)]
        liftIO $ simpleStatus res `shouldBe` status200

    describe "GET /api/articles" $ do
      context "when provided no query params" $
        it "returns a maximum of 20 articles" $ do
          void $ createArticles 30 "" []
          res <- get "/api/articles" []
          liftIO $ do
            simpleStatus res `shouldBe` status200
            Vector.length (simpleBody res ^. key "articles" . _Array) `shouldBe`
              20

      context "when provided a limit param" $
        it "returns articles up to that limit" $ do
          void $ createArticles 30 "" []
          res <- get "/api/articles?limit=25" []
          liftIO $ do
            let status = simpleStatus res
                body = simpleBody res
            status `shouldBe` status200
            Vector.length (body ^. key "articles" . _Array) `shouldBe` 25

      context "when provided a tag param" $
        it "returns articles with that tag" $ do
          void $ createArticles 15 "a" ["foo"]
          void $ createArticles 15 "b" ["bar"]
          res <- get "/api/articles?limit=30&tag=foo" []
          liftIO $ do
            let status = simpleStatus res
                body = simpleBody res
            status `shouldBe` status200
            Vector.length (body ^. key "articles" . _Array) `shouldBe` 15

      context "when provided a valid token" $
        it "includes whether or not the article has been favorited by that user" $ do
          void $ createArticles 5 "" []
          user <- createUserDefault
          let token =
                encodeUtf8
                  ("Token " <> user ^. key "user" . key "token" . _String)
          article <- createArticleDefault
          let slug = encodeUtf8 (article ^. key "article" . key "slug" . _String)
          void $
            post
              ("/api/articles/" <> slug <> "/favorite")
              [(hAuthorization, token)]
              ""
          res <- get "/api/articles" [(hAuthorization, token)]
          liftIO $ do
            let status = simpleStatus res
                articles = simpleBody res ^. key "articles" . _Array
            status `shouldBe` status200
            Vector.find
              (fromMaybe False . (^? (key "favorited" . _Bool)))
              articles `shouldSatisfy`
              isJust

    describe "GET /api/articles/feed" $
      it "returns articles by followed authors" $ do
        user <- createUserDefault
        let token =
              encodeUtf8 ("Token " <> user ^. key "user" . key "token" . _String)
        void $ createArticles 10 "unfollowed" []
        usernames <-
          map (^. (key "article" . key "author" . key "username" . _String)) <$>
          createArticles 10 "followed" []
        mapM_
          (\username ->
             post
               ("/api/profiles/" <> encodeUtf8 username <> "/follow")
               [(hAuthorization, token)]
               "")
          usernames
        res <- get "/api/articles/feed" [(hAuthorization, token)]
        liftIO $ do
          simpleStatus res `shouldBe` status200
          let body = simpleBody res
          Vector.length (body ^. key "articles" . _Array) `shouldBe` 10

    describe "DELETE /api/articles/:slug" $
      it "deletes the specified article" $ do
        user <- createUserDefault
        let token =
              encodeUtf8 ("Token " <> user ^. key "user" . key "token" . _String)
        article <- createArticleDefault
        let slug = encodeUtf8 (article ^. key "article" . key "slug" . _String)
        res <- delete ("/api/articles/" <> slug) [(hAuthorization, token)]
        liftIO $ simpleStatus res `shouldBe` status200

    describe "POST /api/articles/:slug/comments" $
      it "creates a comment for the specified article" $ do
        user <- createUserDefault
        let token =
              encodeUtf8 ("Token " <> user ^. key "user" . key "token" . _String)
        article <- createArticleDefault
        let slug = encodeUtf8 (article ^. key "article" . key "slug" . _String)
        res <-
          post
            ("/api/articles/" <> slug <> "/comments")
            [(hAuthorization, token)]
            [json|{
              comment: {
                body: "MY OPINIONS!!!"
              }
            }|]
        liftIO $ simpleStatus res `shouldBe` status201

    describe "GET /api/articles/:slug/comments" $
      it "creates a comment for the specified article" $ do
        user <- createUserDefault
        let token =
              encodeUtf8 ("Token " <> user ^. key "user" . key "token" . _String)
        article <- createArticleDefault
        let slug = encodeUtf8 (article ^. key "article" . key "slug" . _String)
        void $
          post
            ("/api/articles/" <> slug <> "/comments")
            [(hAuthorization, token)]
            ""
        void $
          post
            ("/api/articles/" <> slug <> "/comments")
            [(hAuthorization, token)]
            ""
        res <- get ("/api/articles/" <> slug <> "/comments") []
        liftIO $ simpleStatus res `shouldBe` status200

    describe "DELETE /api/articles/:slug/comments/:id" $
      it "deletes the specified comment" $ do
        user <- createUserDefault
        let token =
              encodeUtf8 ("Token " <> user ^. key "user" . key "token" . _String)
        article <- createArticleDefault
        let slug = encodeUtf8 (article ^. key "article" . key "slug" . _String)
        comment <-
          post
            ("/api/articles/" <> slug <> "/comments")
            [(hAuthorization, token)]
            [json|{
              comment: {
                body: "MY OPINIONS!!!"
              }
            }|]
        let commentId =
              maybe (0 :: Int) floor $ simpleBody comment ^?
              key "comment" .
              key "id" .
              _Number
        res <-
          delete
            ("/api/articles/" <> slug <> "/comments/" <> encodeUtf8 (pack (show commentId)))
            [(hAuthorization, token)]
        liftIO $ simpleStatus res `shouldBe` status200

    describe "GET /api/tags" $
      it "returns all available tags" $ do
        void $
          createArticle
            "name1"
            "e1@mail.com"
            "password123"
            "title1"
            "desc"
            "body"
            ["tagA"]
        void $
          createArticle
            "name2"
            "e2@mail.com"
            "password123"
            "title2"
            "desc"
            "body"
            ["tagA", "tagB"]
        void $
          createArticle
            "name3"
            "e3@mail.com"
            "password123"
            "title3"
            "desc"
            "body"
            ["tagB", "tagC"]
        res <- get "/api/tags" []
        liftIO $ simpleStatus res `shouldBe` status200
