{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Exception (IOException, catch)
import           System.Exit       (exitFailure)
import           System.IO         (hPutStr, readFile, stderr)
import           System.IO.Error   (isDoesNotExistError)

import           TelegramApi       (sendMessage)

putLog a = hPutStr stderr (a ++ "\n")

loadToken :: String -> IO String
loadToken fileName = do
    catch (readFile fileName) handler
  where
    handler e =
        if (isDoesNotExistError e) then do
            putLog "Couldn't find token file."
            exitFailure
        else do
            putLog (show e)
            exitFailure

main :: IO ()
main = do
    str <- loadToken filePath
    print str
  where
  	filePath = "token.txt"
