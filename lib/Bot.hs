{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot
    ( bot
    ) where

import           Data.Foldable (for_)
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           Safe (lastMay)
import           Web.Telegram.API.Bot (Chat (..), GetUpdatesRequest (..),
                                       Message (..), Response (..),
                                       TelegramClient, Update (..), User (..),
                                       getUpdatesM, getUpdatesRequest)

import BotCommands (BotCmd (..), addNote, help, readCommand, showNew, showOld,
                    wrongCommand)
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
            let User{user_id, user_first_name, user_last_name, user_username} =
                    from
            let userText = Text.unwords
                    [ "User{" <> tshow user_id
                    , tshow user_first_name
                    , tshow $ fromMaybe "" user_last_name
                    , tshow (maybe "" ("@" <>) user_username) <> "}"
                    ]
            let Chat{chat_id} = chat
            case readCommand text of
                Just command -> do
                    putLog $ userText <> " requests " <> tshow command
                    case command of
                        Help ->
                            help (fromIntegral chat_id)
                        ShowNew ->
                            showNew (fromIntegral chat_id) user_id
                        ShowOld ->
                            showOld (fromIntegral chat_id) user_id
                        Start ->
                            help (fromIntegral chat_id)
                        WrongCommand{} ->
                            wrongCommand (fromIntegral chat_id)
                Nothing -> do
                    putLog $ userText <> " adds note"
                    addNote (fromIntegral chat_id) user_id text
            saveOffset updateIdFile update_id
        Just msg ->
            putLog $ "unhandled " <> tshow msg
        _ ->
            putLog $ "unhandled " <> tshow update
  where
    Update{update_id, message = mMessage} = update
