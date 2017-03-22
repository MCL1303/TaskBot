{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Concurrent      (threadDelay)
import Data.Foldable           (for_)
import Data.Monoid             ((<>))
import Database.Persist        (Entity (..), insert_, insertBy)
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Safe                    (lastMay)
import Web.Telegram.API.Bot    as Tg (Message (..), Response (..), Token (..),
                                      Update (..), User(..), getUpdates)

import BotCommands (showNotes)
import DB          (Note (..), User (..), runDB)
import Tools       (loadOffset, loadToken, putLog, readCommand, saveOffset)

-- | Path to file which contains current update id
updateIdFile :: String
updateIdFile = "update_id.txt"

-- | Time(ms) which thread sleeps after catching error
timeout :: Int
timeout = 5000

handleMessage :: Token -> Manager -> Update -> IO ()
handleMessage token manager update =
    case mMessage of
        Just message ->
            case message of
                Message{from = Just user, text = Just text} -> do
                    case readCommand text of
                        Just command ->
                            case command of
                                "show_notes" -> showNotes token manager message
                                _            -> pure ()
                        Nothing -> do
                            let Tg.User{user_id} = user
                            uid <-
                                runDB $
                                    either entityKey id <$>
                                    insertBy DB.User{userTelegramId = fromIntegral user_id}
                            runDB $ insert_ Note{noteText = text, noteOwner = uid}
                    saveOffset updateIdFile update_id
                msg ->
                    putLog $ "unhandled " <> show msg
        Nothing ->
            putLog $ "unhandled " <> show update
  where
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
