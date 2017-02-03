module Tools
(
    -- * deriving tools
    drvJS,
    -- * I/O tools
    readParam,
    writeParam
) where

import System.Exit
import Control.Exception             (IOException, try)
import Data.Aeson.TH                 ( defaultOptions
                                     , Options( fieldLabelModifier
                                              , constructorTagModifier
                                              )
                                     , deriveJSON
                                     )
import Data.Char                     (toLower)
import Language.Haskell.TH.Syntax    (Q, Name, Dec)

drvJS :: Name -> Q [Dec]
drvJS bm = deriveJSON options bm
  where
    options = defaultOptions
        { fieldLabelModifier = drop 3 . map toLower
        , constructorTagModifier = map toLower
        }

readParam :: (Read a) => String -> IO (Maybe a)
readParam fileName = do
    result <- try (readFile fileName) :: IO (Either IOException String)
    case (result) of
        Right fileParam -> pure (Just $ read fileParam)
        Left error      -> pure Nothing

writeParam :: (Show a) => String -> a -> IO ()
writeParam fileName param = writeFile fileName (show param)
