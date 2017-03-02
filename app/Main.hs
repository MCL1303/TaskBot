{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           ClassyPrelude           (tshow)
import           Data.Foldable           (for_)
import           Data.Maybe              (fromMaybe)
import           Data.Text               (empty, pack, strip)
import           Network.HTTP.Client     (newManager, Manager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Safe                    (lastMay)
import           System.IO               (hPutStrLn, stderr)
import           Tools                   (readParam, readParamString,
                                          writeParam)
import           Web.Telegram.API.Bot    (Chat (..), Message (..),
                                          Response (..), Token (Token),
                                          Update (..),
                                          getUpdates, sendMessage,
                                          sendMessageRequest)

-- | Path to file which contains current update id
updateIdFile :: String
updateIdFile = "update_id.txt"

-- | Puts message in log
putLog :: String -> IO()
putLog = hPutStrLn stderr

handleMessage :: Token -> Update -> Manager -> IO ()
handleMessage token Update{update_id = updateId, message = messageM} manager = do
    case (messageM) of
        Just Message{chat = Chat{chat_id = chatId}, text = mText} -> do
            case mText of
                Just text -> do
                    res <- sendMessage
                        token
                        (sendMessageRequest (tshow chatId) text)
                        manager
                    case res of
                        Left e  ->
                            putLog ("Message request failed. " ++ (show e))
                        Right _ -> do
                            writeParam updateIdFile updateId
                            pure ()
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
        Right Response{result = updates} -> do
            for_ updates (\a -> handleMessage token a manager)
            case (lastMay updates) of
                Just lastM -> do
                    let newOffset = update_id lastM + 1
                    bot token (Just (newOffset)) manager
                Nothing    -> bot token curOffset manager
        Left uError    -> do
            putLog (show uError)
            bot token curOffset manager

main :: IO ()
main = do
    pOffset <- readParam updateIdFile
    tokenFromFile <- readParamString tokenFile
    manager <- newManager tlsManagerSettings
    let offset = case pOffset of
            Right mOffset -> Just mOffset
            Left  _       -> Nothing
    case tokenFromFile of
        Right rawToken -> bot (Token (strip (pack ("bot" ++ rawToken)))) offset manager
        Left  e     -> putLog ("Error opening " ++ tokenFile ++ "file. " ++ (show e))
  where tokenFile = "token.txt"
