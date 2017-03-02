module Tools
(
    -- * deriving tools
    drvJS,
    -- * I/O tools
    readParam,
    readParamString,
    writeParam
) where

import           Control.Exception          (IOException, try)
import           Data.Aeson.TH              (Options (constructorTagModifier, fieldLabelModifier),
                                             defaultOptions, deriveJSON)
import           Data.Char                  (toLower)
import           Language.Haskell.TH.Syntax (Dec, Name, Q)

drvJS :: Name -> Q [Dec]
drvJS bm = deriveJSON options bm
  where
    options = defaultOptions
        { fieldLabelModifier = drop 3 . map toLower
        , constructorTagModifier = map toLower
        }

readParam :: (Read a) => String -> IO (Either IOException a)
readParam fileName = do
    param <- readParamString fileName
    let eParam = case param of
            Right string -> Right (read string)
            Left e       -> Left e
    pure eParam

readParamString :: String -> IO (Either IOException String)
readParamString fileName = do
    eResult <- try (readFile fileName) :: IO (Either IOException String)
    pure eResult

writeParam :: (Show a) => String -> a -> IO ()
writeParam fileName param = writeFile fileName (show param)
