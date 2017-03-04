{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           ClassyPrelude           (tshow)
import           Control.Concurrent      (threadDelay)
import           Data.Foldable           (for_)
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Safe                    (lastMay)
import           Tools                   (putLog, readOffset, readToken,
                                          saveOffset)
import           Web.Telegram.API.Bot    (Chat (..), Message (..),
                                          Response (..), Token (..),
                                          Update (..), getUpdates, sendMessage,
                                          sendMessageRequest)

-- | Path to file which contains current update id
updateIdFile :: String
updateIdFile = "update_id.txt"

handleMessage :: Token -> Manager -> Update -> IO ()
handleMessage token manager Update{update_id, message} = do
    case message of
        Just Message{chat = Chat{chat_id}, text} -> do
            case text of
                Just jText -> do
                    res <- sendMessage
                        token
                        (sendMessageRequest (tshow chat_id) jText)
                        manager
                    case res of
                        Left e  ->
                            putLog ("Message request failed. " ++ (show e))
                        Right _ -> do
                            saveOffset updateIdFile update_id
                Nothing -> pure()
        Nothing   -> pure()

bot
    :: Token
    -> Maybe Int -- ^ Offset (update id)
    -> Manager
    -> IO ()
bot token curOffset manager = do
    mUpdates <- getUpdates token curOffset Nothing Nothing manager
    case mUpdates of
        Right Response{result} -> do
            for_ result (handleMessage token manager)
            case (lastMay result) of
                Just Update{update_id} -> do
                    let newOffset = update_id + 1
                    bot token (Just newOffset) manager
                Nothing    -> bot token curOffset manager
        Left uError    -> do
            putLog (show uError)
            threadDelay 5000
            bot token curOffset manager

main :: IO ()
main = do
    offset   <- readOffset updateIdFile
    token    <- readToken tokenFile
    manager  <- newManager tlsManagerSettings
    bot token offset manager
  where tokenFile = "token.txt"
