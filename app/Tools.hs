module Tools
( 
    drvJS
) where

import Data.Aeson.TH             (defaultOptions, Options(fieldLabelModifier, constructorTagModifier), deriveJSON)
import Data.Char                 (toLower)
import Language.Haskell.TH.Syntax

drvJS :: Name -> Q [Dec]
drvJS bm = deriveJSON options bm
    where options = defaultOptions{fieldLabelModifier = drop 3 . map toLower, constructorTagModifier = map toLower}