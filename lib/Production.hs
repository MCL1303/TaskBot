{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Production
    ( TokenLoadException (..)
    , loadOffset
    , loadToken
    , runProduction
    ) where

import           Control.Exception      (Exception, IOException, catch, throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Data.Monoid            ((<>))
import qualified Data.Text              as Text
import qualified Data.Text.IO           as Text
import           Network.HTTP.Client    (Manager)
import           Safe                   (readMay)
import           System.IO              (IOMode (ReadWriteMode), hGetContents,
                                         openFile, stderr)
import           Web.Telegram.API.Bot   (Token (..))
import qualified Web.Telegram.API.Bot   as Tg

import           Classes (MonadAppState (..), MonadDB (..), MonadLog (..),
                          MonadTelegram (..))
import qualified Const
import qualified DB

data TokenLoadException = TokenLoadException
    {cause :: IOException, file :: FilePath}
    deriving Show
instance Exception TokenLoadException

newtype Production a = Production (ReaderT (Token, Manager) IO a)
    deriving
        (Applicative, Functor, Monad, MonadIO, MonadReader (Token, Manager))

runProduction :: Token -> Manager -> Production a -> IO a
runProduction token manager (Production r) = runReaderT r (token, manager)

instance MonadAppState Production where
    saveOffset = liftIO . writeFile Const.updateIdFile . show

instance MonadDB Production where
    runDB = liftIO . DB.runDB (Text.pack Const.dbfile)

instance MonadLog Production where
    putLog = liftIO . Text.hPutStrLn stderr

instance MonadTelegram Production where
    sendMessage req = do
        (token, manager) <- ask
        liftIO $ Tg.sendMessage token req manager

readWritableFile :: FilePath -> IO String
readWritableFile fileName = openFile fileName ReadWriteMode >>= hGetContents

loadOffset :: IO (Maybe Int)
loadOffset =
    (readMay <$> readWritableFile Const.updateIdFile)
    `catch` \(_ :: IOException) -> pure Nothing

loadToken :: IO Token
loadToken = do
    rawToken <- Text.readFile Const.tokenFile `catch` handleReadFile
    pure . Token $ "bot" <> Text.strip rawToken
  where
    handleReadFile e =
        throwIO TokenLoadException{cause = e, file = Const.tokenFile}
