{-# LANGUAGE NamedFieldPuns #-}

module BotCommands
(
    showOld,
    addNote
)
where

import Control.Concurrent   (threadDelay)
import Data.Foldable        (for_)
import Data.Monoid          ((<>))
import Data.Text            (Text, pack)
import Database.Persist     (Entity (..), getByValue, SelectOpt (LimitTo, Desc),
                             selectList, (==.), insert_, insertBy)
import Network.HTTP.Client  (Manager)
import Web.Telegram.API.Bot as Tg (Token (..), sendMessage, sendMessageRequest)

import DB    (EntityField (NoteOwner, NoteId), Note (..), User (..), runDB)
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
        -> Int
        -> Int
        -> IO()
showOld token manager chat_id user_id = do
    mUidEntity <-
        runDB $
            getByValue DB.User{userTelegramId = fromIntegral user_id}
    case mUidEntity of
        Just rec -> do
            let uid = entityKey rec
            notes <-
                fmap (fmap entityVal) . runDB $
                    selectList
                        [NoteOwner ==. uid]
                        [LimitTo 3, Desc NoteId]
            for_ notes $ \Note{noteText} ->
                sendMessageB
                    token
                    manager
                    chat_id
                    noteText
        Nothing   ->
            sendMessageB
                token
                manager
                chat_id
                (pack "Увы, но записей нет.")

addNote :: Int -> Text -> IO()
addNote user_id note = do
    uid <-
        runDB $
            either entityKey id <$>
            insertBy DB.User{userTelegramId = fromIntegral user_id}
    runDB $ insert_ Note{noteText = note, noteOwner = uid}
