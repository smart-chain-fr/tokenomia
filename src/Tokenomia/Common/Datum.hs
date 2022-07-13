{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Common.Datum (
  getDataHash,
  registerDatum,
) where

import Data.Aeson as Json (encode)
import Data.ByteString.Lazy.Char8 qualified as C
import Data.ByteString.Lazy.UTF8 as BLU (toString)
import Data.Coerce
import Data.String

import PlutusTx qualified
import PlutusTx.IsData.Class (ToData)

import Control.Monad.Reader

import Shh.Internal

import Cardano.Api hiding (Address, Hash, Mainnet, Testnet)
import Cardano.Api.Shelley (fromPlutusData)

import System.Random
import Tokenomia.Common.Environment
import Tokenomia.Common.Folder (Folder (..), getFolderPath)
import Tokenomia.Common.Hash

load SearchPath ["cat", "cardano-cli", "echo"]

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
  randomInt <- liftIO (abs <$> randomIO :: IO Integer)
  let filePath = tmpFolder <> "datum-" <> show randomInt <> ".json"
  liftIO $ echo (dataToJSONString a) &> (Truncate . fromString) filePath
  liftIO $ Hash . init . C.unpack <$> (cardano_cli "transaction" "hash-script-data" "--script-data-file" filePath |> capture)

registerDatum ::
  ( MonadIO m
  , MonadReader Environment m
  , ToData a
  ) =>
  a ->
  m FilePath
registerDatum a = do
  datumFolder <- getFolderPath Datum
  datumHash <- getDataHash a
  let filePath = datumFolder <> coerce datumHash <> ".datum"
  liftIO $ echo (dataToJSONString a) &> (Truncate . fromString) filePath
  return filePath
