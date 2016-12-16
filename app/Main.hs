module Main(main) where

import TelegramApi(
                       sendMessage, 
                       getLastMessages,
                       msgText,
                       updUpdate_id,
                       updMessage,
                       msgChat,
                       chtId,
                       Update
                   )


forUpdates :: String -> [Update] -> IO Int
ansOnUpdates token updates = 
    do
        msg <- case (updMessage (head updates)) of
                   Just message -> sendMessage token (getMessageText message) (chtId (msgChat message))
        if null (tail updates) then
            pure (updUpdate_id (head updates))
        else
            ansOnUpdates token (tail updates)
    where getMessageText a = case msgText a of
                              Nothing -> ""
                              Just message -> message

bot :: String -> Maybe Int -> IO ()
bot token offset = do
    updates <- getLastMessages token offset
    print $ updates
    case updates of
        Nothing -> bot token (offset)
        Just d -> 
            if null d then
                bot token (offset)
            else do
                off <- forUpdates token d
                bot token (Just (off + 1))

main :: IO ()
main = do
    token <- getLine
    bot token Nothing
