{-# LANGUAGE TypeFamilies #-}

module Database.Persist.Extra
    ( module Persist
    , getKeyByValue
    , selectValList
    ) where

import Database.Persist as Persist

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader   (ReaderT)

getKeyByValue
    ::  ( MonadIO m
        , PersistUniqueRead backend
        , PersistRecordBackend record backend
        )
    =>  record -> ReaderT backend m (Maybe (Key record))
getKeyByValue value = fmap entityKey <$> getByValue value

selectValList
    ::  ( MonadIO m
        , PersistQueryRead backend
        , PersistRecordBackend record backend
        )
    =>  [Filter record] -> [SelectOpt record] -> ReaderT backend m [record]
selectValList filters options = map entityVal <$> selectList filters options
