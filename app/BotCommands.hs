{-# LANGUAGE NamedFieldPuns #-}

module BotCommands
(
    showOld,
    addNote
)
where

import Control.Monad          (void)
import Data.Foldable          (for_)
import Data.Text              (Text, pack)
import Database.Persist.Extra (Entity (..), SelectOpt (Desc, LimitTo),
                               getKeyByValue, insertBy, insert_, selectValList,
                               (==.))
import Web.Telegram.API.Bot   as Tg (ChatId (..), TelegramClient, sendMessageM,
                                     sendMessageRequest)

import DB (EntityField (NoteId, NoteOwner), Note (..), User (..), runDB)

sendMessageB :: Integer -> Text -> TelegramClient ()
sendMessageB chatId mesText = do
    void $ sendMessageM messageRequest
  where
    messageRequest = sendMessageRequest (ChatId chatId) mesText

showOld :: Integer -- ^ ChatId for sending notes
        -> Int -- ^ UserId - who wants to show
        -> TelegramClient ()
showOld chatId userId = do
    mUid <- runDB $ getKeyByValue DB.User{userTelegramId = fromIntegral userId}
    case mUid of
        Just uid -> do
            notes <- runDB $
                selectValList [NoteOwner ==. uid] [LimitTo 3, Desc NoteId]
            for_ notes $ \Note{noteText} ->
                sendMessageB chatId noteText
        Nothing ->
            sendMessageB chatId (pack "Записей нет.")

addNote :: Int -- ^ UserId - who wants to insert
        -> Text -- ^ Note to insert
        -> TelegramClient ()
addNote userId note = do
    uid <-
        runDB $
            either entityKey id <$>
            insertBy DB.User{userTelegramId = fromIntegral userId}
    runDB $ insert_ Note{noteText = note, noteOwner = uid}
