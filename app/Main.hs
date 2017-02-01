module Main (main) where

import TelegramApi
    (
        sendMessage,
        getLastMessages,
        Message(msgText,msgChat),
        Update(updMessage,updUpdate_id),
        Chat(chtId)
    )

forUpdates :: String -> [Update] -> Maybe Int -> IO (Maybe Int)
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
            forUpdates token xs (Just (updUpdate_id x + 1))
  where
    getMessageText a = case msgText a of
        Nothing      -> ""
        Just message -> message

bot :: String -> Maybe Int -> IO ()
bot token curOffset = do
    mUpdates <- getLastMessages token curOffset
    case mUpdates of
        Nothing -> bot token curOffset
        Just updates  -> do
            newOffset <- forUpdates token updates curOffset
            bot token newOffset

main :: IO ()
main = do
    token <- getLine
    bot token Nothing
