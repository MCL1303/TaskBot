{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Bot (bot)
import Const (tokenFile, updateIdFile)
import Tools (loadOffset, loadToken)

main :: IO ()
main = do
    offset   <- loadOffset updateIdFile
    token    <- loadToken tokenFile
    manager  <- newManager tlsManagerSettings
    bot token offset manager
