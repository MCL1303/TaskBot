{-# LANGUAGE NamedFieldPuns #-}

module BotCommands
(
    showNotes
)
where

import Control.Concurrent   (threadDelay)
import Data.Foldable        (for_)
import Data.Monoid          ((<>))
import Data.Text            (pack)
import Database.Persist     (Entity (..), getByValue, SelectOpt (LimitTo),
                             selectList, (==.))
import Network.HTTP.Client  (Manager)
import Web.Telegram.API.Bot as Tg (Chat (..), Message (..), Token (..),
                                   User (..), sendMessage, sendMessageRequest)

import DB    (EntityField (NoteOwner), Note (..), User (..), runDB)
import Tools (putLog, untilRight)

-- | Time(ms) which thread sleeps after catching error
timeout :: Int
timeout = 5000

showNotes :: Token -> Manager -> Message -> IO()
showNotes token manager message =
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
                            selectList [NoteOwner ==. uid] [LimitTo 3]
                    for_ notes $ \Note{noteText} ->
                        untilRight
                            (sendMessage
                                token
                                (sendMessageRequest (pack $ show chat_id) noteText)
                                manager)
                            (\e -> do
                                putLog $ "Message request failed. " <> show e
                                threadDelay timeout)
                Nothing   -> do
                    _ <- untilRight
                        (sendMessage
                            token
                            (sendMessageRequest
                                (pack $ show chat_id)
                                (pack "You don't have any notes."))
                            manager)
                        (\e -> do
                            putLog $ "Message request failed. " <> show e
                            threadDelay timeout)
                    pure()
        msg -> putLog $ show msg
