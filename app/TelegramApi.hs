{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TelegramApi
    (
    -- * TelegramApi's methods
      sendMessage
    , getLastMessages
    -- * TelegramApi's types
    , Message(msgText,msgChat)
    , Update(updMessage,updUpdate_id)
    , Chat(chtId)
    ) where

import Data.Aeson                (encode, decode, eitherDecode)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.ByteString           (ByteString)
import Data.Text                 (pack)
import Data.Text.Encoding        (encodeUtf8)
import Data.Char                 (toLower)
import Network.HTTP.Client       (httpLbs
                                 , responseStatus
                                 , responseBody
                                 , newManager
                                 , parseRequest
                                 )
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import Network.HTTP.Conduit      (setQueryString)
import Network.HTTP.Types.Status (statusCode)

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
    updates <-
        case offset of
            Nothing -> getHttpRequest (callMethod token "getUpdates") []
            Just s  -> getHttpRequest (callMethod token ("getUpdates"))
                           [("offset", Just $ encodeUtf8 (pack (show s)))]
    case eitherDecode updates of
        Left jsonError -> do
            print jsonError
            pure Nothing
        Right d -> do
            pure (resResult d)

getHttpRequest
    :: String
    -> [(ByteString, Maybe ByteString)]
    -> IO Lazy.ByteString
getHttpRequest url args = do
    manager  <- newManager tlsManagerSettings
    request  <- parseRequest url
    response <- httpLbs (setQueryString args request) manager
    pure $ responseBody response

sendMessage
    :: String -- ^ Token
    -> String -- ^ Message's text
    -> Int    -- ^ Chat_Id where send message
    -> IO Lazy.ByteString -- ^ Response from server
sendMessage token message user =
    getHttpRequest (callMethod token "sendMessage") params
  where params = [ ("text", Just $ encodeUtf8 (pack message))
                 , ("chat_id", Just $ encodeUtf8 (pack (show user)))
                 ]

callMethod :: String -> String -> String
callMethod token method = api_url ++ token ++ "/" ++ method
  where api_url = "https://api.telegram.org/bot"
