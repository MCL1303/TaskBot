module Main (main) where

import           Control.Exception (IOException, catch)
import           System.Exit       (exitFailure)
import           System.IO         (hPutStrLn, readFile, stderr)
import           System.IO.Error   (isDoesNotExistError)

import           TelegramApi (Chat (chtId), Message (msgChat, msgText),
                              Update (updMessage, updUpdate_id),
                              getLastMessages, sendMessage)

putLog a = hPutStrLn stderr a

processUpdates
    :: String -- ^ Token
    -> [Update] -- ^ List of updates
    -> Maybe Int -- ^ Offset (current update id)
    -> IO (Maybe Int) -- ^ New offset
processUpdates token updates offset = do
    case updates of
        []   -> pure offset
        x:xs -> do
            msg <- case updMessage x of
                Just message ->
                    sendMessage
                        token
                        (fromMaybe message)
                        (chtId (msgChat message))
            --writeParam updateIdFile (updUpdate_id x)
            processUpdates token xs (Just (updUpdate_id x + 1))

bot
    :: String -- ^ Token
    -> Maybe Int -- ^ Offset(Update id)
    -> IO ()
bot token curOffset = do
    mUpdates <- getLastMessages token curOffset
    case mUpdates of
        Just updates  ->
            newOffset <- processUpdates token updates curOffset
    bot token newOffset

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
    handler e = do
        if (isDoesNotExistError e) then
            putLog "Couldn't find token file."
        else
            putLog (show e)
        exitFailure

main :: IO ()
main = do
    token <- loadToken tokenFile
    bot token Nothing
  where tokenFile = "token.txt"
