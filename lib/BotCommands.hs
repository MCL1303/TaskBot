{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BotCommands
    ( BotCmd (..)
    , addNote
    , help
    , readCommand
    , showNew
    , showOld
    , wrongCommand
    ) where

import           Control.Monad (void)
import           Data.Char (isSpace)
import           Data.Foldable (for_)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Database.Persist.Extra (Entity (..), SelectOpt (Desc, LimitTo),
                                         getKeyByValue, insertBy, insert_,
                                         selectValList, (==.))
import           Web.Telegram.API.Bot (ChatId (..), TelegramClient,
                                       sendMessageM, sendMessageRequest)

import DB (EntityField (NoteId, NoteOwner), Note (..), User (..), runDB)

data BotCmd = Help | ShowNew | ShowOld | Start | WrongCommand Text
    deriving Show

sendMessage :: Integer
            -> Text
            -> TelegramClient ()
sendMessage chatId message = void . sendMessageM $
    sendMessageRequest (ChatId chatId) message

showNew :: Integer -- ^ ChatId for sending notes
        -> Int -- ^ UserId - who wants to show
        -> TelegramClient ()
showNew chatId userId = do
    mUid <- runDB $ getKeyByValue DB.User{userTelegramId = fromIntegral userId}
    case mUid of
        Just uid -> do
            notes <- runDB $
                selectValList [NoteOwner ==. uid] [LimitTo 3, Desc NoteId]
            for_ notes $ \Note{noteText} ->
                sendMessage chatId noteText
        Nothing ->
            sendMessage chatId "Записей нет."

showOld :: Integer -- ^ ChatId for sending notes
        -> Int -- ^ UserId - who wants to show
        -> TelegramClient ()
showOld chatId userId = do
    mUid <- runDB $ getKeyByValue DB.User{userTelegramId = fromIntegral userId}
    case mUid of
        Just uid -> do
            notes <- runDB $
                selectValList [NoteOwner ==. uid] [LimitTo 3]
            for_ notes $ \Note{noteText} ->
                sendMessage chatId noteText
        Nothing ->
            sendMessage chatId "Записей нет."

addNote :: Integer
        -> Int -- ^ UserId - who wants to insert
        -> Text -- ^ Note to insert
        -> TelegramClient ()
addNote chatId userId note = do
    uid <-
        runDB $
            either entityKey id <$>
            insertBy DB.User{userTelegramId = fromIntegral userId}
    runDB $ insert_ Note{noteText = note, noteOwner = uid}
    sendMessage chatId "Запомнил."

readCommand :: Text -> Maybe BotCmd
readCommand messageText =
    case Text.uncons slashCommand of
        Just ('/', tTail) ->
            case Text.strip tTail of
                "help"      -> Just Help
                "show_new"  -> Just ShowNew
                "show_old"  -> Just ShowOld
                "start"     -> Just Start
                wrongCmd    -> Just $ WrongCommand wrongCmd
        _ -> Nothing
  where slashCommand = Text.takeWhile (not . isSpace) messageText

wrongCommand :: Integer -> TelegramClient ()
wrongCommand chatId = sendMessage chatId "Не знаю такую команду. /help"

help :: Integer -> TelegramClient ()
help chatId = sendMessage chatId $ Text.unlines
    [ "Я бот для хранения заметок."
    , "Доступные команды:"
    , "/show_old - Показать первые записи"
    , "/show_new - Показать последние записи"
    ]
