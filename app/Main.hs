{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Control.Concurrent      (threadDelay)
import           Data.Foldable           (for_)
import           Data.Text               (pack)
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Safe                    (lastMay)
import           Tools                   (putLog, loadOffset, loadToken,
                                          saveOffset)
import           Web.Telegram.API.Bot    (Chat (..), Message (..),
                                          Response (..), Token (..),
                                          Update (..), getUpdates, sendMessage,
                                          sendMessageRequest)

-- | Path to file which contains current update id
updateIdFile :: String
updateIdFile = "update_id.txt"

-- | time which thread sleeps after catching error
timeout :: Int
timeout = 5000

handleMessage :: Token -> Manager -> Update -> IO ()
handleMessage token manager update = do
    let Update{update_id, message} = update
    case message of
        Just Message{chat = Chat{chat_id}, text = (Just text)} -> do
            res <- sendMessage
                token
                (sendMessageRequest (pack $ show chat_id) text)
                manager
            case res of
                Left e  -> do
                    putLog ("Message request failed. " ++ (show e))
                    threadDelay timeout
                    handleMessage token manager update
                Right _ -> do
                    saveOffset updateIdFile update_id
        _ -> pure()

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
            threadDelay timeout
            bot token curOffset manager

main :: IO ()
main = do
    offset   <- loadOffset updateIdFile
    token    <- loadToken tokenFile
    manager  <- newManager tlsManagerSettings
    bot token offset manager
  where tokenFile = "token.txt"
