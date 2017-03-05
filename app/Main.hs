{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import           Control.Concurrent      (threadDelay)
import           Data.Foldable           (for_)
import           Data.Text               (pack)
import           Database.Persist        (Entity (..), SelectOpt (LimitTo),
                                          insert_, selectList, upsert, (==.))
import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Safe                    (lastMay)
import           Web.Telegram.API.Bot    as Tg (Chat (..), Message (..),
                                                Response (..), Token (..),
                                                Update (..), User (..),
                                                getUpdates, sendMessage,
                                                sendMessageRequest)

import           DB                      (EntityField (NoteOwner), Note (..),
                                          User (..), runDB)
import           Tools                   (loadOffset, loadToken, putLog,
                                          saveOffset, untilRight)

-- | Path to file which contains current update id
updateIdFile :: String
updateIdFile = "update_id.txt"

-- | time which thread sleeps after catching error
timeout :: Int
timeout = 5000

handleMessage :: Token -> Manager -> Update -> IO ()
handleMessage token manager update =
    case message of
        Just Message{chat, text = Just text, from = Just user} -> do
            let Chat{chat_id} = chat
                Tg.User{user_id} = user
            uid <- runDB $ do
                Entity uid _ <-
                    upsert DB.User{userTelegramId = fromIntegral user_id} []
                insert_ Note{noteText = text, noteOwner = uid}
                pure uid
            notes <-
                fmap (map entityVal) . runDB $
                    selectList [NoteOwner ==. uid] [LimitTo 3]
            for_ notes $ \Note{noteText} ->
                untilRight
                    (sendMessage
                        token
                        (sendMessageRequest (pack $ show chat_id) noteText)
                        manager)
                    (\e -> do
                        putLog $ "Message request failed. " ++ show e
                        threadDelay timeout)
            saveOffset updateIdFile update_id
        _ -> pure ()
  where Update{update_id, message} = update

bot
    :: Token
    -> Maybe Int -- ^ Offset (update id)
    -> Manager
    -> IO ()
bot token curOffset manager = do
    mUpdates <- getUpdates token curOffset Nothing Nothing manager
    case mUpdates of
        Right Response{result} -> do
            for_ result (handleMessage token manager)
            case lastMay result of
                Just Update{update_id} -> do
                    let newOffset = update_id + 1
                    bot token (Just newOffset) manager
                Nothing    -> bot token curOffset manager
        Left uError    -> do
            putLog (show uError)
            threadDelay timeout
            bot token curOffset manager

main :: IO ()
main = do
    offset   <- loadOffset updateIdFile
    token    <- loadToken tokenFile
    manager  <- newManager tlsManagerSettings
    bot token offset manager
  where tokenFile = "token.txt"
