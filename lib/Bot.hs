{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot
    ( handleUpdates
    , runBot
    ) where

import Control.Concurrent      (threadDelay)
import Control.Monad.IO.Class  (liftIO)
import Data.Foldable           (for_)
import Data.Monoid             ((<>))
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Safe                    (lastMay)
import Web.Telegram.API.Bot    (Chat (..), Message (..), Response (..), Token,
                                Update (..), User (..), getUpdates)

import BotCommands (BotCmd (..), addNote, readCommand, showOld)
import Classes     (MonadAppState, MonadDB, MonadLog, MonadTelegram, putLog,
                    saveOffset)
import Const       (timeout)
import Production  (loadOffset, loadToken, runProduction)
import Tools       (tshow)

runBot :: IO ()
runBot = do
    token   <- loadToken
    manager <- newManager tlsManagerSettings
    offset  <- loadOffset
    bot token offset manager

bot :: Token
    -> Maybe Int -- ^ Offset (update id)
    -> Manager
    -> IO ()
bot token curOffset manager = do
    mUpdates <- getUpdates token curOffset Nothing Nothing manager
    newOffset <-
        runProduction token manager $
            case mUpdates of
                Right Response{result} ->
                    handleUpdates result
                Left err -> do
                    putLog $ tshow err
                    liftIO $ threadDelay timeout
                    pure curOffset
    bot token newOffset manager

handleMessage
    :: (MonadAppState m, MonadDB m, MonadLog m, MonadTelegram m)
    => Update -> m ()
handleMessage update =
    case mMessage of
        Just Message{text = Just text, from = Just from, chat} -> do
            let User{user_id} = from
                Chat{chat_id} = chat
            case readCommand text of
                Just command ->
                    case command of
                        ShowOld ->
                            showOld chat_id user_id
                        WrongCommand wrongCmd ->
                            putLog (cmdErr wrongCmd)
                Nothing -> addNote user_id text
            saveOffset update_id
        Just msg ->
            putLog $ "unhandled " <> tshow msg
        _ ->
            putLog $ "unhandled " <> tshow update
  where
    cmdErr c = "Wrong bot command: " <> c
    Update{update_id, message = mMessage} = update

handleUpdates
    :: (MonadAppState m, MonadDB m, MonadLog m, MonadTelegram m)
    => [Update] -> m (Maybe Int)
handleUpdates updates = do
    for_ updates handleMessage
    case lastMay updates of
        Just Update{update_id} ->
            pure . Just $ update_id + 1
        Nothing ->
            pure Nothing
