{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot
    ( bot
    ) where

import Data.Foldable (for_)
import Data.Monoid ((<>))
import Safe (lastMay)
import Web.Telegram.API.Bot (Chat (..), GetUpdatesRequest (..), Message (..),
                             Response (..), TelegramClient, Update (..),
                             User (..), getUpdatesM, getUpdatesRequest)

import BotCommands (BotCmd (..), addNote, countNotes, readCommand, sendMessage,
                    showNew, showOld)
import Const (updateIdFile)
import Tools (putLog, saveOffset, tshow)

bot :: Maybe Int -- ^ Offset (update id)
    -> TelegramClient ()
bot curOffset = do
    Response{result} <- getUpdatesM updatesRequest
    case lastMay result of
        Just Update{update_id} -> do
            let newOffset = update_id + 1
            for_ result handleMessage
            bot $ Just newOffset
        Nothing -> bot curOffset
  where
    updatesRequest = getUpdatesRequest{updates_offset = curOffset}

handleMessage :: Update -> TelegramClient ()
handleMessage update =
    case mMessage of
        Just Message{text = Just text, from = Just from, chat} -> do
            let User{user_id} = from
                Chat{chat_id} = chat
            case readCommand text of
                Just command ->
                    case command of
                        ShowNew ->
                            showNew (fromIntegral chat_id) user_id
                        ShowOld ->
                            showOld (fromIntegral chat_id) user_id
                        WrongCommand wrongCmd ->
                            putLog $ cmdErr wrongCmd
                Nothing -> do
                    addNote user_id text
                    notes <- countNotes user_id
                    sendMessage
                        (fromIntegral chat_id)
                        ("Добавлено. Всего " <> (tshow notes) <>
                            " заметок.")
            saveOffset updateIdFile update_id
        Just msg ->
            putLog $ "unhandled " <> tshow msg
        _ ->
            putLog $ "unhandled " <> tshow update
  where
    cmdErr c = "Wrong bot command: " <> tshow c
    Update{update_id, message = mMessage} = update
