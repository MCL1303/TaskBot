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
import Web.Telegram.API.Bot as Tg (Chat (..), Message (..), Token (..),
                                   User (..), sendMessage, sendMessageRequest)

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

showOld :: Token -> Manager -> Message -> IO()
showOld token manager message =
    case message of
        Message{chat, from = Just user} -> do
            let Chat{chat_id} = chat
                Tg.User{user_id} = user
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
        _ ->
            putLog ("Edentifying user error. " <> show message)

addNote :: Message -> IO()
addNote Message{from = Just user, text = Just text} = do
    let Tg.User{user_id} = user
    uid <-
        runDB $
            either entityKey id <$>
            insertBy DB.User{userTelegramId = fromIntegral user_id}
    runDB $ insert_ Note{noteText = text, noteOwner = uid}
addNote message = do
    putLog ("Edentifying user error. " <> show message)
