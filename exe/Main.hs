{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot (runClient)

import Bot (bot)
import Const (tokenFile, updateIdFile)
import Tools (loadOffset, loadToken, putLog, tshow)

main :: IO ()
main = do
    offset  <- loadOffset updateIdFile
    token   <- loadToken tokenFile
    manager <- newManager tlsManagerSettings
    res     <- runClient (bot offset) token manager
    putLog $ tshow res
