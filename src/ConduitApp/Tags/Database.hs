{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module ConduitApp.Tags.Database
  ( create
  , query
  , TagT(..)
  , Tag
  , TagId
  ) where

import Conduit (sourceToList)
import ConduitApp.Database (ConduitDb(..), conduitDb)
import ConduitApp.Tags.Database.Tag (Tag, TagId, TagT(..))
import Data.Foldable (toList)
import Data.Function (($), (.), id)
import Data.List (map)
import Data.Maybe (Maybe(Just))
import Data.Set (Set)
import Data.Text (Text)
import Database.Beam
  ( all_
  , default_
  , insertExpressions
  , runSelectReturningList
  , select
  , val_
  )
import Database.Beam.Postgres (runBeamPostgres)
import Database.Beam.Postgres.Conduit (runInsertReturning)
import Database.Beam.Postgres.Full
  ( conflictingFields
  , insertReturning
  , onConflict
  , onConflictUpdateInstead
  )
import Database.PostgreSQL.Simple (Connection)
import System.IO (IO)

query :: Connection -> IO [Tag]
query conn =
  runBeamPostgres conn $
  runSelectReturningList $ select $ all_ $ conduitTags conduitDb

create :: Connection -> Set Text -> IO [Tag]
create conn names =
  runInsertReturning
    conn
    (insertReturning
       (conduitTags conduitDb)
       (insertExpressions (map (Tag default_ . val_) (toList names)))
       (onConflict (conflictingFields tagName) (onConflictUpdateInstead tagName))
       (Just id))
    sourceToList
