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


module Tokenomia.Adapter.Cardano.CLI.Folder
    ( getFolderPath
    , Folder (..)
    ) where


import           Data.String
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB
 
import           Control.Monad.Reader


import           System.Random

import           System.Environment (getEnv)
import           Shh.Internal

import           Codec.Serialise ( serialise )

import qualified Cardano.Api.Shelley  as Shelley



import qualified Plutus.V1.Ledger.Scripts as Script
import           PlutusTx.IsData.Class ( ToData )

import           Tokenomia.Adapter.Cardano.CLI.Environment


import           Tokenomia.Adapter.Cardano.CLI.Data (dataToJSONString)
import           Tokenomia.Common.Shell.Console (printLn)



{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["mkdir","cardano-cli" ]




data Folder = Transactions | Keys | Parameters | MonetaryPolicies | Validators | TMP

getFolderPath :: (MonadIO m, MonadReader Environment m) => Folder -> m FilePath
getFolderPath folder
    =  getFolderPath'
            $ case folder of
                Transactions -> "transactions"
                Keys ->  "keys"
                Parameters -> "parameters"
                MonetaryPolicies -> "monetary-policies"
                Validators -> "validators"
                TMP -> "tmp"


getFolderPath' :: (MonadIO m, MonadReader Environment m) => String -> m FilePath
getFolderPath' s = do
    a <- ( <> "/"<> s<>"/") <$> getRootCLIFolder
    liftIO $ mkdir "-p" a
    return a

getRootCLIFolder :: (MonadIO m, MonadReader Environment m) => m FilePath
getRootCLIFolder = do
    environmentFolder <- asks (\case
                                Testnet {} -> "testnet"
                                Mainnet {} -> "mainnet")
    a <- ( <> "/.tokenomia-cli/" <> environmentFolder) <$> (liftIO . getEnv) "HOME"
    liftIO $ mkdir "-p" a
    return a

toPlutusScriptV1 :: Script.Script -> Shelley.PlutusScript Shelley.PlutusScriptV1
toPlutusScriptV1
  = Shelley.PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise

getDataHash :: (MonadIO m, MonadReader Environment m, ToData a) => a -> m String
getDataHash a = do
    filePath <- persistDataInTMP a
    liftIO $ init . C.unpack  <$>  (cardano_cli "transaction" "hash-script-data" "--script-data-file" filePath |> capture)

persistDataInTMP :: (MonadIO m, MonadReader Environment m, ToData a) => a -> m FilePath
persistDataInTMP a = do
    tmpFolder <- getFolderPath TMP
    randomInt <- liftIO ( abs <$> randomIO :: IO Integer)
    let filePath = tmpFolder <> show randomInt <> ".txt"
    liftIO $ printLn (dataToJSONString a) &> (Truncate . fromString) filePath
    return filePath
