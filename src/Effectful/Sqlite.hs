{-# LANGUAGE TemplateHaskell #-}

module Effectful.Sqlite (
  Sqlite,
  withConnection,
  runSqlite,
  runSqliteWithPool,
  execute,
  execute_,
  executeMany,
  query,
  query_,
  queryNamed,
  queryNamed_,
  lastInsertRowId,
) where

import Database.SQLite.Simple (Connection, NamedParam, Query)
import Database.SQLite.Simple qualified as SQLite
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH (makeEffect)
import UnliftIO.Pool as Pool

data Sqlite :: Effect where
  WithConnection :: (Connection -> m a) -> Sqlite m a

makeEffect ''Sqlite

runSqlite ::
  Connection ->
  Eff (Sqlite : es) a ->
  Eff es a
runSqlite conn =
  interpret $ \env -> \case
    WithConnection f ->
      localSeqUnlift env $ \unlift -> unlift $ f conn

runSqliteWithPool ::
  (IOE :> es) =>
  Pool Connection ->
  Eff (Sqlite : es) a ->
  Eff es a
runSqliteWithPool pool = interpret $ \env -> \case
  WithConnection f ->
    localSeqUnlift env $ \unlift -> do
      Pool.withResource pool $ unlift . f

-- mkSqlitePool ::
--   (IOE :> es) =>
--   Int ->
--   NominalDiffTime ->
--   Int ->
--   FilePath ->
--   Eff es (Pool Connection)
-- mkSqlitePool stripes keepAlive poolSize path =
--   liftIO $
--     createPool
--       (SQLite.open path)
--       SQLite.close
--       stripes
--       keepAlive
--       poolSize

execute ::
  (IOE :> es, Sqlite :> es, SQLite.ToRow params) =>
  Query ->
  params ->
  Eff es ()
execute q params =
  withConnection $ \conn -> liftIO $ SQLite.execute conn q params

execute_ ::
  (IOE :> es, Sqlite :> es) =>
  Query ->
  Eff es ()
execute_ q =
  withConnection $ \conn -> liftIO $ SQLite.execute_ conn q

executeMany ::
  (IOE :> es, Sqlite :> es, SQLite.ToRow params) =>
  Query ->
  [params] ->
  Eff es ()
executeMany q params =
  withConnection $ \conn -> liftIO $ SQLite.executeMany conn q params

query ::
  (IOE :> es, Sqlite :> es, SQLite.ToRow params, SQLite.FromRow row) =>
  Query ->
  params ->
  Eff es [row]
query q params =
  withConnection $ \conn -> liftIO $ SQLite.query conn q params

query_ ::
  (IOE :> es, Sqlite :> es, SQLite.FromRow row) =>
  Query ->
  Eff es [row]
query_ q =
  withConnection $ \conn -> liftIO $ SQLite.query_ conn q

queryNamed ::
  (IOE :> es, Sqlite :> es, SQLite.FromRow row) =>
  Query ->
  [NamedParam] ->
  Eff es [row]
queryNamed q params =
  withConnection $ \conn -> liftIO $ SQLite.queryNamed conn q params

queryNamed_ ::
  (IOE :> es, Sqlite :> es, SQLite.FromRow row) =>
  Query ->
  Eff es [row]
queryNamed_ q =
  withConnection $ \conn -> liftIO $ SQLite.queryNamed_ conn q

lastInsertRowId ::
  (IOE :> es, Sqlite :> es) =>
  Eff es Int64
lastInsertRowId =
  withConnection $ \conn -> liftIO $ SQLite.lastInsertRowId conn
