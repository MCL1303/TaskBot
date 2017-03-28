{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Concurrent      (threadDelay)
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot    as Tg (Response (..), Token, getUpdates)

import Const    (timeout, tokenFile, updateIdFile)
import Handlers (handleUpdates)
import Tools    (loadOffset, loadToken, putLog)

bot :: Token
    -> Maybe Int -- ^ Offset (update id)
    -> Manager
    -> IO ()
bot token curOffset manager = do
    mUpdates <- getUpdates token curOffset Nothing Nothing manager
    newOffset <- case mUpdates of
        Right Response{result} ->
            handleUpdates token manager result
        Left uError -> do
            putLog (show uError)
            threadDelay timeout
            pure curOffset
    bot token newOffset manager

main :: IO ()
main = do
    offset   <- loadOffset updateIdFile
    token    <- loadToken tokenFile
    manager  <- newManager tlsManagerSettings
    bot token offset manager
