module ConduitApp.Articles.Database.Attributes
  ( Attributes(..)
  , ValidationFailure(..)
  , insertAttributes
  , updateAttributes
  ) where

import ConduitApp.Articles.Database.Article (Article, ArticleT(..))
import qualified ConduitApp.Articles.Database.Article as Article
import ConduitApp.Articles.Database.Queries (findBySlug)
import ConduitApp.Attribute (Attribute)
import Control.Applicative ((<*>), pure)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.Bool ((||), otherwise)
import Data.Char (isAlphaNum, isSpace)
import Data.Eq ((==))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (traverse)
import Data.Validation (Validation(Failure, Success), toEither)
import Database.PostgreSQL.Simple (Connection)
import System.IO (IO)
import Text.Show (Show)

data Attributes f = Attributes
  { slug :: Attribute f Text
  , title :: Attribute f Text
  , description :: Attribute f Text
  , body :: Attribute f Text
  }

data ValidationFailure
  = TitleWouldProduceDuplicateSlug Text
  | TitleRequired
  | DescriptionRequired
  | BodyRequired

deriving instance Show ValidationFailure

generateSlug :: Text -> Text
generateSlug =
  Text.intercalate "-" .
  Text.words . Text.toLower . Text.filter ((||) <$> isAlphaNum <*> isSpace)

insertSlug ::
     Connection
  -> Text
  -> Compose IO (Validation [ValidationFailure]) Text
insertSlug conn title =
  Compose $ do
    existing <- findBySlug conn value
    pure $
      case existing of
        Nothing -> Success value
        Just _ -> Failure [TitleWouldProduceDuplicateSlug value]
  where
    value = generateSlug title

require :: ValidationFailure -> Text -> Validation [ValidationFailure] Text
require err value =
  if Text.null value
    then Failure [err]
    else Success value

makeTitle :: Text -> Validation [ValidationFailure] Text
makeTitle = require TitleRequired

makeDescription :: Text -> Validation [ValidationFailure] Text
makeDescription = require DescriptionRequired

makeBody :: Text -> Validation [ValidationFailure] Text
makeBody = require BodyRequired

insertAttributes ::
     Connection
  -> Text
  -> Text
  -> Text
  -> ExceptT [ValidationFailure] IO (Attributes Identity)
insertAttributes conn title description body =
  ExceptT . (toEither <$>) . getCompose $
  Attributes
    <$> insertSlug conn title
    <*> Compose (pure (makeTitle title))
    <*> Compose (pure (makeDescription description))
    <*> Compose (pure (makeBody body))

updateSlug ::
     Connection
  -> Article
  -> Text
  -> Compose IO (Validation [ValidationFailure]) Text
updateSlug conn current title
  | value == Article.slug current = pure value
  | otherwise = insertSlug conn title
  where
    value = generateSlug title

updateAttributes ::
     Connection
  -> Article
  -> Maybe Text
  -> Maybe Text
  -> Maybe Text
  -> ExceptT [ValidationFailure] IO (Attributes Maybe)
updateAttributes conn current title description body =
  ExceptT . (toEither <$>) . getCompose $
  Attributes
    <$> traverse (updateSlug conn current) title
    <*> traverse (Compose . pure . makeTitle) title
    <*> traverse (Compose . pure . makeDescription) description
    <*> traverse (Compose . pure . makeBody) body
