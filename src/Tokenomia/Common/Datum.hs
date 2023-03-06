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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tokenomia.Common.Datum
  ( getDataHash
  , registerDatum
  ) where

import           Data.Aeson as Json ( encode )
import           Data.String
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.ByteString.Lazy.UTF8 as BLU ( toString )
import           Data.Coerce

import qualified PlutusTx
import           PlutusTx.IsData.Class ( ToData )

import           Control.Monad.Reader

import           Shh.Internal


import           Cardano.Api.Shelley ( fromPlutusData )
import           Cardano.Api hiding (Testnet,Mainnet,Address,Hash)

import           Tokenomia.Common.Environment
import           Tokenomia.Common.Folder (getFolderPath,Folder (..))
import           Tokenomia.Common.Hash
import           System.Random

load SearchPath ["cat","cardano-cli", "echo" ]

dataToJSONString :: ToData a => a -> String
dataToJSONString =
  BLU.toString
    . Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData

getDataHash :: (MonadIO m, MonadReader Environment m, ToData a) => a -> m Hash
getDataHash a = do
    tmpFolder <- getFolderPath TMP
    randomInt <- liftIO ( abs <$> randomIO :: IO Integer)
    let filePath = tmpFolder <> "datum-" <> show randomInt <> ".json"
    liftIO $ echo  (dataToJSONString a) &> (Truncate . fromString) filePath
    liftIO $ Hash . init . C.unpack  <$>  (cardano_cli "transaction" "hash-script-data" "--script-data-file" filePath |> capture)

registerDatum
  :: ( MonadIO m
     , MonadReader Environment m
     , ToData a) => a -> m FilePath
registerDatum a = do
    datumFolder <- getFolderPath Datum
    datumHash <- getDataHash a
    let filePath = datumFolder <> coerce datumHash <> ".datum"
    liftIO $ echo  (dataToJSONString a) &> (Truncate . fromString) filePath
    return filePath
