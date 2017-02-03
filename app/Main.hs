module Main (main) where

import TelegramApi
    (
        sendMessage,
        getLastMessages,
        Message(msgText,msgChat),
        Update(updMessage,updUpdate_id),
        Chat(chtId)
    )
import Tools (readParam, writeParam)

--Preferences
update_id = "update_id.txt"

forUpdates
    :: String -- ^ Token
    -> [Update] -- ^ List of updates
    -> Maybe Int -- ^ Offset(current update id)
    -> IO (Maybe Int) -- ^ New offset
forUpdates token updates offset = do
    case updates of
        []   -> case offset of
            Nothing -> pure offset
            Just a -> pure (Just a)
        x:xs -> do
            msg <- case updMessage x of
                Just message ->
                    sendMessage
                        token
                        (getMessageText message)
                        (chtId (msgChat message))
            writeParam update_id (updUpdate_id x)
            forUpdates token xs (Just (updUpdate_id x + 1))
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
            newOffset <- forUpdates token updates curOffset
            bot token newOffset

main :: IO ()
main = do
    offset <- readParam update_id
    token <- getLine
    bot token offset
