{-# LANGUAGE OverloadedStrings #-}
module TelegramApi
    ( 
        sendMessage,
        getHttpRequest,
    ) where

import Network.HTTP.Client                    (httpLbs, responseStatus, responseBody, newManager, parseRequest)
import Network.HTTP.Client.TLS                (tlsManagerSettings)
import Network.HTTP.Types.Status              (statusCode)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Text.Encoding                     (encodeUtf8)
import Network.HTTP.Conduit                   (setQueryString)
import Data.ByteString                        (ByteString)
import Data.Text                              (pack)

getHttpRequest :: String -> [(ByteString, Maybe ByteString)] -> IO Lazy.ByteString
getHttpRequest url args = do 
    manager <- newManager tlsManagerSettings
    request <- parseRequest url 
    response <- httpLbs (setQueryString args request) manager
    putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
    pure $ responseBody response

sendMessage :: String -> String -> IO Lazy.ByteString
sendMessage token message = getHttpRequest ("https://api.telegram.org/bot" ++ token ++ "/sendMessage") [("text", Just $ encodeUtf8 (pack message)),("chat_id",Just $ encodeUtf8 "193856114")]
