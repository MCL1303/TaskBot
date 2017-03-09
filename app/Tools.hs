{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Control.Exception          (Exception, IOException, catch,
                                             handle, throwIO)
import           Data.Aeson.TH              (Options (constructorTagModifier, fieldLabelModifier),
                                             defaultOptions, deriveJSON)
import           Data.Char                  (toLower)
import           Data.Monoid                ((<>))
import           Data.Text                  (strip)
import qualified Data.Text.IO               as Text
import           Language.Haskell.TH.Syntax (Dec, Name, Q)
import           System.IO                  (IOMode (ReadWriteMode),
                                             hGetContents, hPutStrLn, openFile,
                                             stderr)
import           Web.Telegram.API.Bot       (Token (Token))

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

data TokenLoadException = TokenLoadException
    {tle_cause :: IOException, tle_file :: FilePath}
    deriving Show
instance Exception TokenLoadException

loadToken :: FilePath -> IO Token
loadToken fileName = do
    rawToken <- Text.readFile fileName `catch` handleReadFile
    pure . Token $ "bot" <> strip rawToken
  where
    handleReadFile e =
        throwIO TokenLoadException{tle_cause = e, tle_file = fileName}

loadOffset :: FilePath -> IO (Maybe Int)
loadOffset fileName =
    handle handleReadFile $ do
        offsetString <- readWritableFile
        pure $ read offsetString
  where
    readWritableFile = openFile fileName ReadWriteMode >>= hGetContents

    handleReadFile (e :: IOException) = do
        putLog $ show e
        pure Nothing

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
