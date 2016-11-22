module Main where

import TelegramApi

main :: IO ()
main = do
 str <- getLine
 mes <- sendMessage "295506649:AAG9_Ahtmi9P5OUDbQacwhq6RZ4pJVC3ADg" str
 print $ mes
