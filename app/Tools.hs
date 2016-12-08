module Tools
( 
    drvJS
) where

import Data.Aeson.TH             
import Data.Char                 (toLower)
import Language.Haskell.TH.Syntax

--drvJS :: Name -> Q [Dec]
drvJS bm = deriveJSON defaultOptions{fieldLabelModifier = drop 3 . map toLower, constructorTagModifier = map toLower} bm