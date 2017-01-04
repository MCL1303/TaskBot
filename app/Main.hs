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
                forUpdates token xs (Just ((updUpdate_id x) + 1))
  where getMessageText a = case msgText a of
                               Nothing      -> ""
                               Just message -> message

bot :: String -> Maybe Int -> IO ()
bot token offset = do
    updates <- getLastMessages token offset
    print updates
    case updates of
        Nothing -> bot token (offset)
        Just d  -> do
            off <- forUpdates token d offset
            bot token off

main :: IO ()
main = do
    token <- getLine
    bot token Nothing
