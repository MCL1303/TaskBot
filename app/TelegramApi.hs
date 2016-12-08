{-# LANGUAGE TemplateHaskell #-}
module TelegramApi
    ( 
        sendMessage,
        getHttpRequest,
        getLastMessages,
        updMessage,
        msgText,
        msgChat,
        updUpdate_id,
        chtId
    ) where

import Network.HTTP.Client       (httpLbs, responseStatus, responseBody, newManager, parseRequest)
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy      (ByteString)
import Data.Aeson.TH             
import Data.Char                 (toLower)
import Data.Aeson                (encode, decode, eitherDecode)
import Tools                     (drvJS)

data Result = 
    Result
        {
            resResult :: Maybe [Update]
        }deriving Show

data Update = 
    Update
        {
            updMessage :: Message,
            updUpdate_id :: Int
        }deriving Show

data Message = 
    Message
        {
            msgText :: String,
            msgChat :: Chat
        }deriving Show

data Chat = 
    Chat
        {
            chtId :: Int
        }deriving Show


fmap concat $ mapM drvJS [''Chat, ''Message, ''Update, ''Result]

getLastMessages :: String -> Maybe Int -> IO (Maybe [Update]) 
getLastMessages token offset = do
    case offset of
        Nothing -> do
            upds <- getHttpRequest("https://api.telegram.org/bot" ++ token ++ "/getUpdates")
            print $ upds
            case (eitherDecode upds :: Either String Result) of
                Left jsonError -> do
                    print $ jsonError
                    pure (Nothing :: Maybe [Update])
                Right d -> do
                    print $ (encode Result{resResult = Just []})
                    pure (resResult d)
        Just s -> do
            print $ "https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ (show s)
            upds <- getHttpRequest("https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ (show s))
            --print $ upds
            case (eitherDecode upds :: Either String Result) of
                Left jsonError -> do
                    print $ jsonError
                    pure (Nothing :: Maybe [Update])
                Right d -> do
                    --print $ show s
                    pure (resResult d)

getHttpRequest :: String -> IO ByteString
getHttpRequest url = do 
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    --putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
    pure $ responseBody response

sendMessage :: String -> String -> String -> IO ByteString
sendMessage token message user = getHttpRequest ("https://api.telegram.org/bot" ++ token ++ "/sendMessage?text=" ++ message ++ "&chat_id=" ++ user)
