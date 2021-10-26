{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}


module Tokenomia.Vesting.Repository
    ( register
    , getAll

    ) where

import           Data.Aeson
 
import           Control.Monad.Reader

import           System.Directory


import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Vesting.Contract

import           Tokenomia.Adapter.Cardano.CLI.Folder (getFolderPath,Folder (..))

register :: ( MonadIO m , MonadReader Environment m) => VestingParams -> m ()
register vestingParams = do
    indexFolder <- getFolderPath Validators
    let filePath = indexFolder <> "vesting.index"
    liftIO (doesFileExist filePath)
        >>= \case
             False -> liftIO $ encodeFile filePath [vestingParams]
             True ->  liftIO $ decodeFileStrict @[VestingParams] filePath
                        >>= \case
                            Nothing -> error "Vesting index is badly formed"
                            Just params -> liftIO $ encodeFile filePath (vestingParams:params)


getAll :: ( MonadIO m , MonadReader Environment m) => m [VestingParams]
getAll = do
    indexFolder <- getFolderPath Validators
    let filePath = indexFolder <> "vesting.index"
    liftIO (doesFileExist filePath)
        >>= \case
             False -> return []
             True ->  liftIO $ decodeFileStrict @[VestingParams] filePath
                >>= \case
                    Nothing -> error "Vesting index is badly formed"
                    Just params -> return params

