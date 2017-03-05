{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Tools
    (
    -- * deriving tools
      drvJS
    -- * I/O tools
    , loadOffset
    , loadToken
    , saveOffset
    -- * Log tool
    , putLog
    -- * Database
    , NoteId
    , runDB
    , UserId
    ) where

import           Control.Exception            (IOException, throwIO, try)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Logger         (NoLoggingT)
import           Control.Monad.Reader         (ReaderT)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Resource (ResourceT)
import           Data.Aeson.TH                (Options (constructorTagModifier, fieldLabelModifier),
                                               defaultOptions, deriveJSON)
import           Data.Char                    (toLower)
import           Data.Int                     (Int32)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text, strip)
import qualified Data.Text.IO                 as Text
import           Database.Persist.Sql         (SqlBackend, runMigration)
import           Database.Persist.Sqlite      (runSqlite)
import           Database.Persist.TH          (mkMigrate, mkPersist,
                                               persistLowerCase, share,
                                               sqlSettings)
import           Language.Haskell.TH.Syntax   (Dec, Name, Q)
import           System.IO                    (IOMode (ReadWriteMode),
                                               hGetContents, hPutStrLn,
                                               openFile, stderr)
import           Web.Telegram.API.Bot         (Token (Token))

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
        User
            telegramId  Int32
        Note
            text        Text
            owner       UserId
    |]

-- | Puts message in log
putLog :: String -> IO()
putLog = hPutStrLn stderr

drvJS :: Name -> Q [Dec]
drvJS = deriveJSON options
  where
    options = defaultOptions
        { fieldLabelModifier = drop 3 . map toLower
        , constructorTagModifier = map toLower
        }

loadToken :: FilePath -> IO Token
loadToken fileName = do
    eToken <- try (Text.readFile fileName) :: IO (Either IOException Text)
    case eToken of
        Right rawToken -> pure (Token ("bot" <> strip rawToken))
        Left e         -> do
            putLog ("Error reading offset from " ++ fileName)
            throwIO e

loadOffset :: FilePath -> IO (Maybe Int)
loadOffset fileName = do
    eOffset <- try readOffset :: IO (Either IOException String)
    case eOffset of
        Right offsetString -> pure (read offsetString)
        Left  e            -> do
            putLog (show e)
            pure Nothing
  where
    readOffset = openFile fileName ReadWriteMode >>= hGetContents

saveOffset :: FilePath -> Int -> IO ()
saveOffset fileName offset = writeFile fileName (show offset)

dbfile :: Text
dbfile = "./db.sqlite"

runDB
    :: (MonadBaseControl IO m, MonadIO m)
    => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -- ^ database action
    -> m a
runDB action =
    runSqlite dbfile $ do
        runMigration migrateAll
        action
