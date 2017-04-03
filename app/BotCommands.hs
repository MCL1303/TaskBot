{-# LANGUAGE NamedFieldPuns #-}

module BotCommands
(
    showOld,
    addNote
)
where

import Data.Foldable          (for_)
import Data.Text              (Text, pack)
import Database.Persist.Extra (Entity (..), SelectOpt (Desc, LimitTo),
                               getKeyByValue, insertBy, insert_, selectValList,
                               (==.))
import Web.Telegram.API.Bot   as Tg (ChatId(..), TelegramClient, sendMessageM,
                                     sendMessageRequest)

import DB    (EntityField (NoteId, NoteOwner), Note (..), User (..), runDBM)

sendMessageB :: Int -> Text -> TelegramClient ()
sendMessageB chatId mesText = do
    _ <- sendMessageM (sendMessageRequest (ChatId $ fromIntegral chatId) mesText)
    pure ()

showOld :: Int -- ^ ChatId for sending notes
        -> Int -- ^ UserId - who wants to show
        -> TelegramClient ()
showOld chatId userId = do
    mUid <- runDBM $ getKeyByValue DB.User{userTelegramId = fromIntegral userId}
    case mUid of
        Just uid -> do
            notes <- runDBM $
                selectValList [NoteOwner ==. uid] [LimitTo 3, Desc NoteId]
            for_ notes $ \Note{noteText} ->
                sendMessageB chatId noteText
        Nothing ->
            sendMessageB chatId (pack "Записей нет.")

addNote :: Int -- ^ UserId - who wants to insert
        -> Text -- ^ Inserting note
        -> TelegramClient ()
addNote userId note = do
    uid <-
        runDBM $
            either entityKey id <$>
            insertBy DB.User{userTelegramId = fromIntegral userId}
    runDBM $ insert_ Note{noteText = note, noteOwner = uid}
