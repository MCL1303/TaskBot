{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BotCommands
    ( BotCmd(..)
    , addNote
    , readCommand
    , showOld
    ) where

import           Control.Concurrent     (threadDelay)
import           Data.Char              (isSpace)
import           Data.Foldable          (for_)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Database.Persist.Extra (Entity (..), SelectOpt (Desc, LimitTo),
                                         getKeyByValue, insertBy, insert_,
                                         selectValList, (==.))
import           Network.HTTP.Client    (Manager)
import           Web.Telegram.API.Bot   as Tg (Token (..), sendMessage,
                                               sendMessageRequest)

import Const (timeout)
import DB    (EntityField (NoteId, NoteOwner), Note (..), User (..), runDB)
import Tools (putLog, untilRight)

data BotCmd = ShowOld | WrongCommand Text

sendMessageB :: Token -> Manager -> Int -> Text -> IO()
sendMessageB token manager chat_id mesText = do
    _ <- untilRight
        (sendMessage
            token
            (sendMessageRequest (tshow chat_id) mesText)
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
                selectValList [NoteOwner ==. uid] [LimitTo 3]
            for_ notes $ \Note{noteText} ->
                sendMessageB token manager chatId noteText
        Nothing ->
            sendMessageB token manager chatId "Записей нет."

addNote :: Int -- ^ UserId - who wants to insert
        -> Text -- ^ Inserting note
        -> IO ()
addNote userId note = do
    uid <-
        runDB $
            either entityKey id <$>
            insertBy DB.User{userTelegramId = fromIntegral userId}
    runDB $ insert_ Note{noteText = note, noteOwner = uid}

readCommand :: Text -> Maybe BotCmd
readCommand messageText =
    case Text.uncons slashCommand of
        Just ('/', tTail) ->
            case tTail of
                "show_old"    -> Just ShowOld
                wrongCmd -> Just (WrongCommand wrongCmd)
        _ -> Nothing
  where slashCommand = Text.takeWhile (not . isSpace) messageText

tshow :: Show a => a -> Text
tshow = Text.pack . show
