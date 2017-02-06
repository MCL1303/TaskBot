module Main (main) where

import           TelegramApi (Chat (chtId), Message (msgChat, msgText),
                              Update (updMessage, updUpdate_id),
                              getLastMessages, sendMessage)
import           Tools       (readParam, writeParam)

-- | Filepath to file, containing current update id
updateIdFile = "update_id.txt"

processUpdates
    :: String -- ^ Token
    -> [Update] -- ^ List of updates
    -> Maybe Int -- ^ Offset(current update id)
    -> IO (Maybe Int) -- ^ New offset
processUpdates token updates offset = do
    case updates of
        []   -> case offset of
            Nothing -> pure offset
            Just a  -> pure (Just a)
        x:xs -> do
            msg <- case updMessage x of
                Just message ->
                    sendMessage
                        token
                        (getMessageText message)
                        (chtId (msgChat message))
            writeParam updateIdFile (updUpdate_id x)
            processUpdates token xs (Just (updUpdate_id x + 1))
  where
    getMessageText a = case msgText a of
        Nothing      -> ""
        Just message -> message

bot
    :: String -- ^ Token
    -> Maybe Int -- ^ Offset(Update id)
    -> IO ()
bot token curOffset = do
    mUpdates <- getLastMessages token curOffset
    case mUpdates of
        Nothing -> do
            bot token curOffset
        Just updates  -> do
            newOffset <- processUpdates token updates curOffset
            bot token newOffset

main :: IO ()
main = do
    offset <- readParam updateIdFile
    token <- getLine
    bot token offset
