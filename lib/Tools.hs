{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tools
(
    -- * I/O tools
    loadOffset,
    loadToken,
    saveOffset,
    -- * Log tool
    putLog,
    -- * Control flow
    untilRight
) where

import           Control.Exception (Exception, IOException, catch, throwIO)
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Safe (readMay)
import           System.IO (IOMode (ReadWriteMode), hGetContents, hPutStrLn,
                            openFile, stderr)
import           Web.Telegram.API.Bot (Token (Token))

-- | Puts message in log
putLog :: String -- ^ Error message
       -> IO()
putLog = hPutStrLn stderr

data TokenLoadException = TokenLoadException
    {cause :: IOException, file :: FilePath}
    deriving Show
instance Exception TokenLoadException

loadToken :: FilePath -> IO Token
loadToken fileName = do
    rawToken <- Text.readFile fileName `catch` handleReadFile
    pure . Token $ "bot" <> Text.strip rawToken
  where
    handleReadFile e = throwIO TokenLoadException{cause = e, file = fileName}

loadOffset :: FilePath -> IO (Maybe Int)
loadOffset fileName =
    do  offsetString <- readWritableFile
        pure $ readMay offsetString
    `catch` \(e :: IOException) -> do
        putLog $ show e
        pure Nothing
  where
    readWritableFile = openFile fileName ReadWriteMode >>= hGetContents

saveOffset :: FilePath -> Int -> IO ()
saveOffset fileName offset = writeFile fileName (show offset)

untilRight :: IO (Either e a) -> (e -> IO ()) -> IO a
untilRight body handler = do
    res <- body
    case res of
        Left e -> do
            handler e
            untilRight body handler
        Right a ->
            pure a
