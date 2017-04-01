module Tools
    ( tshow
    , untilRight
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text

tshow :: Show a => a -> Text
tshow = Text.pack . show

untilRight :: Monad m => m (Either e a) -> (e -> m ()) -> m a
untilRight body handler = do
    res <- body
    case res of
        Left e -> do
            handler e
            untilRight body handler
        Right a ->
            pure a
