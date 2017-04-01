{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Testing
    ( runTesting
    , tMessage
    , tUpdate
    ) where

import Control.Monad.IO.Class      (MonadIO, liftIO)
import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)
import Data.Aeson.Types            (FromJSON, Pair, Value (Number, String),
                                    object, parseEither, parseJSON, (.=))
import Data.Text                   (Text)
import GHC.Stack                   (HasCallStack)
import PseudoMacros                (__LINE__)
import Web.Telegram.API.Bot        (Message, MessageResponse,
                                    SendMessageRequest (..), Update)

import           Classes (MonadAppState (..), MonadDB (..), MonadLog (..),
                          MonadTelegram (..))
import qualified DB

data Trace = Log Text | SaveOffset Int | SendMessage SendMessageRequest
    deriving Show

instance Eq Trace where
    Log text1 == Log text2 = text1 == text2
    SaveOffset n1 == SaveOffset n2 = n1 == n2
    SendMessage _smr1 == SendMessage _smr2 = error "Eq SendMessageRequest"
    _ == _ = False

trace :: MonadWriter [Trace] m => Trace -> m ()
trace = tell . pure

newtype Testing a = Testing (WriterT [Trace] IO a)
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , MonadWriter [Trace]
        )

runTesting :: Testing a -> IO (a, [Trace])
runTesting (Testing w) = runWriterT w

instance MonadAppState Testing where
    saveOffset = trace . SaveOffset

instance MonadDB Testing where
    runDB = liftIO . DB.runDB ":memory:"

instance MonadLog Testing where
    putLog = trace . Log

instance MonadTelegram Testing where
    sendMessage req = do
        trace $ SendMessage req
        pure $ Right tResponse

tObject :: (FromJSON a, HasCallStack) => [Pair] -> a
tObject = either error id . parseEither parseJSON . object

tMessage :: Message
tMessage = tObject
    [ "chat" .= object ["id" .= Number $__LINE__, "type" .= String "private"]
    , "date" .= Number $__LINE__
    , "message_id" .= Number $__LINE__
    ]

tResponse :: MessageResponse
tResponse = tObject []

tUpdate :: Message -> Update
tUpdate msg = tObject ["update_id" .= Number $__LINE__, "message" .= msg]
