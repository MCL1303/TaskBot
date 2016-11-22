module TelegramApi
    ( sendMessage
    ) where
import Network.HTTP.Client       (httpLbs, responseStatus, responseBody, newManager, parseRequest)
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy      (ByteString)

--Token -> Message
sendMessage :: String -> String -> IO ByteString
sendMessage token message = do 
 manager <- newManager tlsManagerSettings
 request <- parseRequest ("https://api.telegram.org/bot"++ token ++"/sendMessage?text="++message++"&chat_id=193856114")
 response <- httpLbs request manager
 putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
 pure(responseBody response)
 