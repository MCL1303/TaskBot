{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BotCommands
    ( BotCmd(..)
    , addNote
    , readCommand
    , showOld
    ) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Char              (isSpace)
import           Data.Foldable          (for_)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Database.Persist.Extra (Entity (..), SelectOpt (Desc, LimitTo),
                                         getKeyByValue, insertBy, insert_,
                                         selectValList, (==.))
import           Web.Telegram.API.Bot   (sendMessageRequest)

import Classes (MonadDB, MonadLog, MonadTelegram, putLog, runDB, sendMessage)
import Const   (timeout)
import DB      (EntityField (NoteId, NoteOwner), Note (..), User (..))
import Tools   (tshow, untilRight)

data BotCmd = ShowOld | WrongCommand Text

sendMessageB :: (MonadIO m, MonadLog m, MonadTelegram m) => Int -> Text -> m ()
sendMessageB chat_id mesText = do
    _ <- untilRight
        (sendMessage $ sendMessageRequest (tshow chat_id) mesText)
        (\e -> do
            putLog $ "sendMessageB failed. " <> tshow e
            liftIO $ threadDelay timeout)
    pure ()

showOld
    :: (MonadDB m, MonadLog m, MonadTelegram m)
    => Int -- ^ ChatId for sending notes
    -> Int -- ^ UserId - who wants to show
    -> m ()
showOld chatId userId = do
    mUid <- runDB $ getKeyByValue DB.User{userTelegramId = fromIntegral userId}
    case mUid of
        Just uid -> do
            notes <- runDB $
                selectValList [NoteOwner ==. uid] [LimitTo 3, Desc NoteId]
            for_ notes $ \Note{noteText} ->
                sendMessageB chatId noteText
        Nothing ->
            sendMessageB chatId "Записей нет."

addNote
    :: MonadDB m
    => Int -- ^ UserId - who wants to insert
    -> Text -- ^ Inserting note
    -> m ()
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
