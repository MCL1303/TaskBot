{-# LANGUAGE NamedFieldPuns #-}

module BotCommands
(
    showOld,
    addNote
)
where

import Control.Concurrent     (threadDelay)
import Data.Foldable          (for_)
import Data.Monoid            ((<>))
import Data.Text              (Text, pack)
import Database.Persist.Extra (Entity (..), SelectOpt (Desc, LimitTo),
                               getKeyByValue, insertBy, insert_, selectValList,
                               (==.))
import Network.HTTP.Client    (Manager)
import Web.Telegram.API.Bot   as Tg (Token (..), sendMessage,
                                     sendMessageRequest)

import DB    (EntityField (NoteId, NoteOwner), Note (..), User (..), runDB)
import Tools (putLog, untilRight)

-- | Time(ms) which thread sleeps after catching error
timeout :: Int
timeout = 5000

sendMessageB :: Token -> Manager -> Int -> Text -> IO()
sendMessageB token manager chat_id mesText = do
    _ <- untilRight
        (sendMessage
            token
            (sendMessageRequest (pack $ show chat_id) mesText)
            manager)
        (\e -> do
            putLog $ "sendMessageB failed. " <> show e
            threadDelay timeout)
    pure ()

showOld :: Token
        -> Manager
        -> Int -- ^ ChatId for sending notes
        -> Int -- ^ UserId - who wants to show
        -> IO ()
showOld token manager chatId userId = do
    mUid <- runDB $ getKeyByValue DB.User{userTelegramId = fromIntegral userId}
    case mUid of
        Just uid -> do
            notes <- runDB $
                selectValList [NoteOwner ==. uid] [LimitTo 3, Desc NoteId]
            for_ notes $ \Note{noteText} ->
                sendMessageB token manager chatId noteText
        Nothing ->
            sendMessageB token manager chatId (pack "Записей нет.")

addNote :: Int -- ^ UserId - who wants to insert
        -> Text -- ^ Inserting note
        -> IO ()
addNote userId note = do
    uid <-
        runDB $
            either entityKey id <$>
            insertBy DB.User{userTelegramId = fromIntegral userId}
    runDB $ insert_ Note{noteText = note, noteOwner = uid}
