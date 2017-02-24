module Main (main) where

import           Data.Foldable           (for_)
import           Data.Maybe              (fromMaybe)
import           Data.Text               (empty, pack, strip)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Safe                    (lastMay)
import           System.IO               (hPutStrLn, stderr)
import           Tools                   (readParam, readParamString,
                                          writeParam)
import           Web.Telegram.API.Bot    (Chat (chat_id), Message (chat, text),
                                          Response (result), Token (Token),
                                          Update (message, update_id),
                                          getUpdates, sendMessage,
                                          sendMessageRequest)

-- | Path to file which contains current update id
updateIdFile :: String
updateIdFile = "update_id.txt"

-- | Puts message in log
putLog :: String -> IO()
putLog = hPutStrLn stderr

handleMessage :: Token -> Update -> IO ()
handleMessage token update = do
    manager <- newManager tlsManagerSettings
    case (message update) of
        Just msg -> do
            res <- sendMessage
                token
                (sendMessageRequest (chatId msg) (messageText msg))
                manager
            case res of
                Left e  -> do
                    putLog "Message request failed"
                    putLog (show e)
                Right _ -> pure ()
        Nothing   -> pure()

  where
    chatId msg = pack(show(chat_id(chat msg)))
    messageText msg = fromMaybe empty (text msg)

bot
    :: Token-- ^ Token
    -> Maybe Int -- ^ Offset (update id)
    -> IO ()
bot token curOffset = do
    manager <- newManager tlsManagerSettings
    mUpdates <- getUpdates token curOffset Nothing Nothing manager
    case mUpdates of
        Right rUpdates -> do
            let updates = result rUpdates
            for_ updates (handleMessage token)
            case (lastMay updates) of
                Just lastM -> do
                    let newOffset = update_id lastM + 1
                    writeParam updateIdFile newOffset
                    bot token (Just (newOffset))
                Nothing    -> bot token curOffset
        Left uError    -> do
            putLog (show uError)
            bot token curOffset

main :: IO ()
main = do
    offset <- readParam updateIdFile
    tokenFromFile <- readParamString tokenFile
    case tokenFromFile of
        Just token -> bot (Token (strip (pack ("bot" ++ token)))) offset
        Nothing    -> putLog ("Error opening " ++ tokenFile ++ "file.")
  where tokenFile = "token.txt"
