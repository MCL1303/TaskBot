module Main where

import TelegramApi(sendMessage)

main :: IO ()
main = do
    token <- getLine
    messageText <- getLine
    mes <- sendMessage token messageText
    print $ mes
