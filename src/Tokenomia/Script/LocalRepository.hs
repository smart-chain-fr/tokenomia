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


module Tokenomia.Script.LocalRepository
    ( registerMintingScriptFile
    , registerValidatorScriptFile
    , getScriptLocation
    , ScriptLocation (..)
    , getMonetaryPolicyPath
    , getDataHash
    , persistDataInTMP
    ) where

import           Data.String
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB
 

import           Control.Monad.Reader

import           System.Random
import           System.Directory

import           Shh.Internal

import           Codec.Serialise ( serialise )

import           Cardano.Api hiding (Testnet,Mainnet,Address,Hash)
import qualified Cardano.Api.Shelley  as Shelley


import           Ledger hiding (Address)

import qualified Plutus.V1.Ledger.Scripts as Script
import           PlutusTx.IsData.Class ( ToData )

import           Tokenomia.Common.Environment


import           Tokenomia.Common.Data (dataToJSONString)
import           Tokenomia.Common.Folder (getFolderPath,Folder (..))

import           Tokenomia.Common.Address
import           Tokenomia.Common.Hash

{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["cat","cardano-cli", "echo" ]

getMonetaryPolicyPath
    :: ( MonadIO m, MonadReader Environment m )
    => CurrencySymbol
    -> m (Maybe FilePath)
getMonetaryPolicyPath  currencySymbol = do
    scFolder <- getFolderPath MonetaryPolicies
    let monetaryFilePath = scFolder <> show currencySymbol <> ".plutus"
    liftIO (doesFileExist monetaryFilePath)
        >>= \case
             True ->  (return . Just) monetaryFilePath
             False -> return Nothing

registerMintingScriptFile
    :: ( MonadIO m, MonadReader Environment m )
    => MintingPolicy
    -> m FilePath
registerMintingScriptFile mp = do
    scFolder <- getFolderPath MonetaryPolicies
    let filePath =  scFolder <> show (scriptCurrencySymbol mp) <> ".plutus"
    liftIO $ writeFileTextEnvelope filePath Nothing ((toPlutusScriptV1 . unMintingPolicyScript) mp)
        >>= (\case
            Left err -> error $ displayError err
            Right () -> return filePath)


data ScriptLocation = ScriptLocation {onChain :: Address , offChain::FilePath} deriving (Eq,Show)

getScriptLocation :: ( MonadIO m , MonadReader Environment m )
    => Validator
    -> m ScriptLocation
getScriptLocation validator = do
    networkOption <- asks (\case 
                        Mainnet {} -> asArg ["--mainnet"]
                        Testnet {magicNumber} ->  asArg ["--testnet-magic", show magicNumber])
    scFolder <- getFolderPath Validators
    let validatorPath =  scFolder <> show (validatorHash validator) <> ".plutus"
    scriptAddr <- liftIO $ Address . C.unpack  <$> (cardano_cli "address" "build" "--payment-script-file" validatorPath networkOption |> capture)
    return ScriptLocation {onChain = scriptAddr, offChain = validatorPath}

registerValidatorScriptFile
    :: ( MonadIO m , MonadReader Environment m )
    => Validator
    -> m ScriptLocation
registerValidatorScriptFile validator  = do
    scFolder <- getFolderPath Validators
    let validatorPath =  scFolder <> show (validatorHash validator) <> ".plutus"
    liftIO (writeFileTextEnvelope validatorPath Nothing ((toPlutusScriptV1 . unValidatorScript) validator))
        >>= \case
            Left err -> error $ displayError err
            Right () -> getScriptLocation validator


toPlutusScriptV1 :: Script.Script -> Shelley.PlutusScript Shelley.PlutusScriptV1
toPlutusScriptV1
  = Shelley.PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise

getDataHash :: (MonadIO m, MonadReader Environment m, ToData a) => a -> m Hash
getDataHash a = do
    filePath <- persistDataInTMP a
    liftIO $ Hash . init . C.unpack  <$>  (cardano_cli "transaction" "hash-script-data" "--script-data-file" filePath |> capture)

persistDataInTMP :: (MonadIO m, MonadReader Environment m, ToData a) => a -> m FilePath
persistDataInTMP a = do
    tmpFolder <- getFolderPath TMP
    randomInt <- liftIO ( abs <$> randomIO :: IO Integer)
    let filePath = tmpFolder <> show randomInt <> ".txt"
    liftIO $ echo  (dataToJSONString a) &> (Truncate . fromString) filePath
    return filePath
