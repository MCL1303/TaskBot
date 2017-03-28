module Const where

-- | Time (ms) which thread sleeps after catching error
timeout :: Int
timeout = 5000

tokenFile :: FilePath
tokenFile = "token.txt"

-- | Path to file which contains current update id
updateIdFile :: FilePath
updateIdFile = "update_id.txt"
