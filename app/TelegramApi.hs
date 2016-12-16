{-# LANGUAGE TemplateHaskell #-}

module TelegramApi
    (
        sendMessage,
        getLastMessages,
        Message(msgText,msgChat),
        Update(updMessage,updUpdate_id),
        chtId
    ) where

import Network.HTTP.Client       (httpLbs, responseStatus, responseBody, newManager, parseRequest)
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.ByteString.Lazy      (ByteString)
import Data.Char                 (toLower)
import Data.Aeson                (encode, decode, eitherDecode)
import Tools                     (drvJS)

data Result = Result
        { resResult :: Maybe [Update]
        } deriving Show

data Update = Update
        { updMessage :: Maybe Message
        , updUpdate_id :: Int
        } deriving Show

data Message = Message
        { msgText :: Maybe String
        , msgChat :: Chat
        } deriving Show

data Chat = Chat
        { chtId :: Int
        } deriving Show

concat <$> mapM drvJS [''Chat, ''Message, ''Update, ''Result]

getLastMessages :: String -> Maybe Int -> IO (Maybe [Update])
getLastMessages token offset = do
    updates <- case offset of
                      Nothing -> getHttpRequest("https://api.telegram.org/bot" ++ token ++ "/getUpdates")
                      Just s -> getHttpRequest("https://api.telegram.org/bot" ++ token ++ "/getUpdates?offset=" ++ (show s))
    case (eitherDecode updates :: Either String Result) of
        Left jsonError -> do
            print $ jsonError
            pure (Nothing :: Maybe [Update])
        Right d -> do
            pure (resResult d)

getHttpRequest :: String -> IO ByteString
getHttpRequest url = do 
    manager <- newManager tlsManagerSettings
    request <- parseRequest url
    response <- httpLbs request manager
    pure $ responseBody response

sendMessage :: String -> String -> Int -> IO ByteString
sendMessage token message user = 
    getHttpRequest ("https://api.telegram.org/bot" ++ 
                    token ++ 
                    "/sendMessage?text=" ++ 
                    message ++ 
                    "&chat_id=" 
                    ++ show(user))
