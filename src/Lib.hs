module TelegramApi
    ( sendMessage
    ) where
import Network.HTTP

sendMessage :: String -> String -> Int
sendMessage a b = postRequest "https://api.telegram.org/bot"++a++"/sendMessage?text="++b++"&chat_id=193856114"

someFunc :: IO ()
someFunc = 	putStrLn "someFunc"