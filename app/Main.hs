{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Foldable           (for_)
import Data.Monoid             ((<>))
import Network.HTTP.Client     (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Safe                    (lastMay)
import Web.Telegram.API.Bot    as Tg (Chat (..), GetUpdatesRequest (..),
                                      Message (..), Response (..), TelegramClient,
                                      Update (..), User (..), getUpdatesM,
                                      runClient)

import BotCommands (addNote, showOld)
import Tools       (BotCmd (..), loadOffset, loadToken, putLog, putLogM, readCommand,
                    saveOffsetM)

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
                            showOld chat_id user_id
                        WrongCommand wrongCmd ->
                            putLogM (cmdErr wrongCmd)
                Nothing -> addNote user_id text
            saveOffsetM updateIdFile update_id
        Just msg ->
            putLogM $ "unhandled " <> show msg
        _ ->
            putLogM $ "unhandled " <> show update
  where
    cmdErr c = "Wrong bot command: " <> c
    Update{update_id, message = mMessage} = update

bot :: Maybe Int -- ^ Offset (update id)
    -> TelegramClient ()
bot curOffset = do
    uResult <- getUpdatesM uRequest
    let Response{result} = uResult
    case lastMay result of
        Just Update{update_id} -> do
            let newOffset = update_id + 1
            for_ result handleMessage
            bot $ Just newOffset
        Nothing -> bot curOffset
  where
    uRequest = GetUpdatesRequest curOffset Nothing Nothing Nothing

main :: IO ()
main = do
    offset   <- loadOffset updateIdFile
    token    <- loadToken tokenFile
    manager  <- newManager tlsManagerSettings
    res <- runClient
        (bot offset)
        token
        manager
    putLog $ show res
  where tokenFile = "token.txt"
