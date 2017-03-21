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
import Database.Persist     (Entity (..), SelectOpt (LimitTo), insertBy,
                             selectList, (==.))
import Network.HTTP.Client  (Manager)
import Web.Telegram.API.Bot as Tg (Chat (..), Message (..), Token (..),
                                   User (..), sendMessage, sendMessageRequest)

import DB    (EntityField (NoteOwner), Note (..), User (..), runDB)
import Tools (putLog, untilRight)

-- | time which thread sleeps after catching error
timeout :: Int
timeout = 5000

showNotes :: Token -> Manager -> Message -> IO()
showNotes token manager message = do
    case message of
        Message{chat, from = Just user} -> do
            let Chat{chat_id} = chat
                Tg.User{user_id} = user
            uid <-
                runDB $
                    either entityKey id <$>
                    insertBy DB.User{userTelegramId = fromIntegral user_id}
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
        _ -> pure()
