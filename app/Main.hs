{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Concurrent      (threadDelay)
import Data.Foldable           (for_)
import Data.Monoid             ((<>))
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Safe                    (lastMay)
import Web.Telegram.API.Bot    as Tg (Chat(..), getUpdates, Message (..), Response (..), Token (..),
                                      Update (..), User(..))

import BotCommands (addNote, showOld)
import Tools       (BotCmd(..), loadOffset, loadToken, putLog, readCommand, saveOffset)

-- | Path to file which contains current update id
updateIdFile :: String
updateIdFile = "update_id.txt"

-- | Time(ms) which thread sleeps after catching error
timeout :: Int
timeout = 5000

handleMessage :: Token -> Manager -> Update -> IO ()
handleMessage token manager update =
    case mMessage of
        Just Message{text = Just text, from = Just from, chat}-> do
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
    cmdErr c = "Wrong bot command: " <> c
    Update{update_id, message = mMessage} = update

bot :: Token
    -> Maybe Int -- ^ Offset (update id)
    -> Manager
    -> IO ()
bot token curOffset manager = do
    mUpdates <- getUpdates token curOffset Nothing Nothing manager
    case mUpdates of
        Right Response{result} -> do
            for_ result (handleMessage token manager)
            case lastMay result of
                Just Update{update_id} -> do
                    let newOffset = update_id + 1
                    bot token (Just newOffset) manager
                Nothing    -> bot token curOffset manager
        Left uError    -> do
            putLog (show uError)
            threadDelay timeout
            bot token curOffset manager

main :: IO ()
main = do
    offset   <- loadOffset updateIdFile
    token    <- loadToken tokenFile
    manager  <- newManager tlsManagerSettings
    bot token offset manager
  where tokenFile = "token.txt"
