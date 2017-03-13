    {-# LANGUAGE OverloadedStrings #-}

module Tools
(
    -- * deriving tools
    drvJS,
    -- * I/O tools
    loadOffset,
    loadToken,
    saveOffset,
    -- * Log tool
    putLog,
    -- * Control flow
    untilRight
) where

import           Control.Exception          (IOException, throwIO, try)
import           Data.Char                  (toLower)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, strip)
import qualified Data.Text.IO               as Text
import           System.IO                  (IOMode (ReadWriteMode),
                                             hGetContents, hPutStrLn, openFile,
                                             stderr)
import           Web.Telegram.API.Bot       (Token (Token))

-- | Puts message in log
putLog :: String -> IO()
putLog errorMessage = hPutStrLn stderr errorMessage

loadToken :: FilePath -> IO Token
loadToken fileName = do
    eToken <- try (Text.readFile fileName) :: IO (Either IOException Text)
    case eToken of
        Right rawToken -> pure (Token ("bot" <> (strip rawToken)))
        Left e         -> do
            putLog ("Error reading offset from " ++ fileName)
            throwIO e

loadOffset :: FilePath -> IO (Maybe Int)
loadOffset fileName = do
    eOffset <- try (readOffset) :: IO (Either IOException String)
    case eOffset of
        Right offsetString -> pure (read offsetString)
        Left  e            -> do
            putLog (show e)
            pure Nothing
  where
    readOffset = openFile fileName ReadWriteMode >>= hGetContents

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
