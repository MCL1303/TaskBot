{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module DB where

import Control.Monad.Logger         (NoLoggingT)
import Control.Monad.Reader         (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Int                     (Int32)
import Data.Text                    (Text)
import Database.Persist.Sql         (SqlBackend, runMigration)
import Database.Persist.Sqlite      (runSqlite)
import Database.Persist.TH          (mkMigrate, mkPersist, persistLowerCase,
                                     share, sqlSettings)

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
        User
            telegramId  Int32
            UniqueTelegramId telegramId
        Note
            text        Text
            owner       UserId
    |]

runDB
    :: Text -- ^ database address (filename)
    -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a -- ^ database action
    -> IO a
runDB dbfile action =
    runSqlite dbfile $ do
        runMigration migrateAll
        action
