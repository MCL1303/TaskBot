{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Foldable           (for_)
import Data.Monoid             ((<>))
import Network.HTTP.Client     (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Safe                    (lastMay)
import Web.Telegram.API.Bot    as Tg (Chat (..), GetUpdatesRequest (..),
                                      Message (..), Response (..),
                                      TelegramClient, Update (..), User (..),
                                      getUpdatesM, getUpdatesRequest, runClient)

import BotCommands (addNote, showOld)
import Tools       (BotCmd (..), loadOffset, loadToken, putLog, putLogT,
                    readCommand, saveOffset)

-- | Path to file which contains current update id
updateIdFile :: String
updateIdFile = "update_id.txt"

handleMessage :: Update -> TelegramClient ()
handleMessage update =
    case mMessage of
        Just Message{text = Just text, from = Just from, chat} -> do
            let User{user_id} = from
                Chat{chat_id} = chat
            case readCommand text of
                Just command ->
                    case command of
                        ShowOld ->
                            showOld (fromIntegral chat_id) user_id
                        WrongCommand wrongCmd ->
                            putLogT (cmdErr wrongCmd)
                Nothing -> addNote user_id text
            saveOffset updateIdFile update_id
        Just msg ->
            putLogT $ "unhandled " <> show msg
        _ ->
            putLogT $ "unhandled " <> show update
  where
    cmdErr c = "Wrong bot command: " <> c
    Update{update_id, message = mMessage} = update

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
main :: IO ()
main = do
    offset  <- loadOffset updateIdFile
    token   <- loadToken tokenFile
    manager <- newManager tlsManagerSettings
    res     <- runClient (bot offset) token manager
    putLog $ show res
  where tokenFile = "token.txt"
