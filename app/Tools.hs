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

readParam :: (Read a) => String -> IO (Maybe a)
readParam fileName = do
    param <- readParamString fileName
    case param of
        Just string -> pure (Just(read string))
        Nothing     -> pure Nothing

readParamString :: String -> IO (Maybe String)
readParamString fileName = do
    result <- try (readFile fileName) :: IO (Either IOException String)
    case (result) of
        Right fileParam -> pure (Just fileParam)
        Left _          -> do
            pure Nothing

writeParam :: (Show a) => String -> a -> IO ()
writeParam fileName param = writeFile fileName (show param)
