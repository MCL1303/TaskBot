module Main (main) where

import           Control.Exception (IOException, catch)
import           System.Exit       (exitFailure)
import           System.IO         (hPutStr, readFile, stderr)
import           System.IO.Error   (isDoesNotExistError)

import TelegramApi
    (
        sendMessage,
        getLastMessages,
        Message(msgText,msgChat),
        Update(updMessage,updUpdate_id),
        Chat(chtId)
    )

putLog a = hPutStr stderr (a ++ "\n")

forUpdates :: String -> [Update] -> Maybe Int -> IO (Maybe Int)
forUpdates token updates offset = do
    case updates of
        []   -> case offset of
            Nothing -> pure offset
            Just a -> pure (Just a)
        x:xs -> do
            msg <- case updMessage x of
                Just message ->
                    sendMessage
                        token
                        (getMessageText message)
                        (chtId (msgChat message))
            forUpdates token xs (Just (updUpdate_id x + 1))
  where
    getMessageText a = case msgText a of
        Nothing      -> ""
        Just message -> message

loadToken :: String -> IO String
loadToken fileName = do
    catch (readFile fileName) handler
  where
      handler e
          |isDoesNotExistError e = do
              putLog "Couldn't find token file."
              exitFailure
          |otherwise = do
              putLog (show e)
              exitFailure

bot :: String -> Maybe Int -> IO ()
bot token curOffset = do
    mUpdates <- getLastMessages token curOffset
    case mUpdates of
        Nothing -> bot token curOffset
        Just updates  -> do
            newOffset <- forUpdates token updates curOffset
            bot token newOffset

main :: IO ()
main = do
    token <- loadToken tokenFile
    bot token Nothing
  where tokenFile = "token.txt"