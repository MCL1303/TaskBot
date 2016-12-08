module Main where

import TelegramApi(sendMessage, getLastMessages, msgText, updUpdate_id, updMessage, msgChat, chtId)

bot :: String -> Int -> IO()
bot token offset = do
    updts <- getLastMessages token (Just offset)
    case updts of
        Nothing -> print "oops"
        Just d -> do
            case (null d) of
                True -> do
                    bot token (offset)
                False -> do
                    msg <- sendMessage token (msgText (updMessage (head d))) (show(chtId (msgChat (updMessage (head d)))))
                    --print $ (show msg)
                    bot token (offset + 1)
main :: IO ()
main = do
    token <- getLine
    messageText <- getLine
    updts <- getLastMessages token Nothing
    case updts of
        Nothing -> print "oops"
        Just d -> do
            msg <- sendMessage token (msgText (updMessage (head d))) (show(chtId (msgChat (updMessage (head d)))))
            print $ (show msg)
            bot token (updUpdate_id (head d))