module Main where

import TelegramApi

main :: IO ()
main = do
    str <- getLine
    mes <- sendMessage "" str
    print $ mes
    where token = "token"