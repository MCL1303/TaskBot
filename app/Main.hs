module Main where

import Data.Char
import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

upperCase :: String -> String
upperCase str = map toUpper str

main :: IO ()
main = do 
 manager <- newManager tlsManagerSettings
 request <- parseRequest ("https://api.telegram.org/"++ token ++"/sendMessage?text=privet&chat_id=193856114")
 response <- httpLbs request manager
 putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
 print $ responseBody response
 where token = "token"