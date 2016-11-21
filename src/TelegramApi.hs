module TelegramApi
    ( someFunc
    ) where
import Network.HTTP

someFunc :: String -> String -> IO String
someFunc a b = simpleHTTP(getRequest ("https://api.telegram.org/bot" ++ a ++"/sendMessage?text=" ++ b ++ "&chat_id=193856114")) >>= getResponseBody