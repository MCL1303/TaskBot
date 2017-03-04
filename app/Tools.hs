module Tools
(
    -- * deriving tools
    drvJS,
    -- * I/O tools
    readOffset,
    readToken,
    saveOffset,
    -- * Log tool
    putLog
) where

import           Control.Exception          (IOException, try)
import           Data.Aeson.TH              (Options (constructorTagModifier, fieldLabelModifier),
                                             defaultOptions, deriveJSON)
import           Data.Char                  (toLower)
import           Data.Text                  (pack, strip)
import           Language.Haskell.TH.Syntax (Dec, Name, Q)
import           System.Exit                (exitFailure)
import           System.IO                  (hPutStrLn, stderr)
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

readToken :: String -> IO Token
readToken fileName = do
    eToken <- try (readFile fileName) :: IO(Either IOException String)
    case eToken of
        Right token -> pure (Token(strip (pack ("bot" ++ token))))
        Left e      -> do
            putLog ("Error reading offset from " ++ fileName)
            putLog (show e)
            exitFailure

readOffset :: String -> IO (Maybe Int)
readOffset fileName = do
    eOffset <- try (readFile fileName) :: IO(Either IOException String)
    case eOffset of
        Right offsetString -> pure(read offsetString)
        Left  e            -> do
            putLog(show e)
            pure Nothing

saveOffset :: String -> Int -> IO ()
saveOffset fileName offset = writeFile fileName (show offset)
   {- case result of
        Right _ -> pure ()
        Left e  -> do
            putLog("Error saving offset into " ++ fileName)
            putLog(show e)
            exitFailure-}
