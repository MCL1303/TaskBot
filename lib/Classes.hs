module Classes
    ( MonadAppState (..)
    , MonadDB (..)
    , MonadLog (..)
    , MonadTelegram (..)
    ) where

import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Logger         (NoLoggingT)
import Control.Monad.Reader         (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Text                    (Text)
import Database.Persist.Sql         (SqlBackend)
import Servant.Client               (ServantError)
import Web.Telegram.API.Bot         (MessageResponse, SendMessageRequest)

class MonadAppState m where
    saveOffset :: Int -> m ()

type DBAction a = ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a

-- | all db operations (like 'selectList') require 'IO'
class MonadIO m => MonadDB m where
    runDB :: DBAction a -> m a

class MonadLog m where
    putLog :: Text -> m ()

class MonadTelegram m where
    sendMessage :: SendMessageRequest -> m (Either ServantError MessageResponse)
