{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot
    ( bot
    , handleUpdates
    ) where

import           Control.Concurrent   (threadDelay)
import           Data.Foldable        (for_)
import           Data.Monoid          ((<>))
import qualified Data.Text            as Text
import           Network.HTTP.Client  (Manager)
import           Safe                 (lastMay)
import           Web.Telegram.API.Bot as Tg (Chat (..), Message (..),
                                             Response (..), Token, Update (..),
                                             User (..), getUpdates)

import BotCommands (BotCmd (..), addNote, readCommand, showOld)
import Const       (timeout, updateIdFile)
import Tools       (putLog, saveOffset)

bot :: Token
    -> Maybe Int -- ^ Offset (update id)
    -> Manager
    -> IO ()
bot token curOffset manager = do
    mUpdates <- getUpdates token curOffset Nothing Nothing manager
    newOffset <- case mUpdates of
        Right Response{result} ->
            handleUpdates token manager result
        Left uError -> do
            putLog (show uError)
            threadDelay timeout
            pure curOffset
    bot token newOffset manager

handleMessage :: Token -> Manager -> Update -> IO ()
handleMessage token manager update =
    case mMessage of
        Just Message{text = Just text, from = Just from, chat} -> do
            let User{user_id} = from
                Chat{chat_id} = chat
            case readCommand text of
                Just command ->
                    case command of
                        ShowOld ->
                            showOld token manager chat_id user_id
                        WrongCommand wrongCmd ->
                            putLog (cmdErr wrongCmd)
                Nothing -> addNote user_id text
            saveOffset updateIdFile update_id
        Just msg ->
            putLog $ "unhandled " <> show msg
        _ ->
            putLog $ "unhandled " <> show update
  where
    cmdErr c = "Wrong bot command: " <> Text.unpack c
    Update{update_id, message = mMessage} = update

handleUpdates :: Token -> Manager -> [Update] -> IO (Maybe Int)
handleUpdates token manager updates = do
    for_ updates $ handleMessage token manager
    case lastMay updates of
        Just Update{update_id} ->
            pure . Just $ update_id + 1
        Nothing ->
            pure Nothing
