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
    putLog
) where

import           Control.Exception          (IOException, try)
import           Data.Aeson.TH              (Options (constructorTagModifier, fieldLabelModifier),
                                             defaultOptions, deriveJSON)
import           Data.Char                  (toLower)
import           Data.Text                  (pack, strip)
import           Data.Monoid                ((<>))
import           Language.Haskell.TH.Syntax (Dec, Name, Q)
import           System.Exit                (exitFailure)
import           System.IO                  (hPutStrLn, stderr, openFile, IOMode(ReadWriteMode), hGetContents)
import           Web.Telegram.API.Bot       (Token (Token))

-- | Puts message in log
putLog :: String -> IO()
putLog errorMessage = hPutStrLn stderr errorMessage

drvJS :: Name -> Q [Dec]
drvJS bm = deriveJSON options bm
  where
    options = defaultOptions
        { fieldLabelModifier = drop 3 . map toLower
        , constructorTagModifier = map toLower
        }

loadToken :: FilePath -> IO Token
loadToken fileName = do
    eToken <- try (readFile fileName) :: IO (Either IOException String)
    case eToken of
        Right rawToken -> pure (Token ("bot" <> strip (pack (rawToken))))
        Left e         -> do
            putLog ("Error reading offset from " ++ fileName)
            putLog (show e)
            exitFailure

loadOffset :: FilePath -> IO (Maybe Int)
loadOffset fileName = do
    eOffset <- try (openFile fileName ReadWriteMode >>= hGetContents) :: IO (Either IOException String)
    case eOffset of
        Right offsetString -> pure (read offsetString)
        Left  e            -> do
            putLog (show e)
            pure Nothing

saveOffset :: FilePath -> Int -> IO ()
saveOffset fileName offset = writeFile fileName (show offset)
