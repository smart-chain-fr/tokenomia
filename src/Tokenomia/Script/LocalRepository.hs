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
    ) where

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LB
 

import           Control.Monad.Reader

import           System.Directory

import           Shh.Internal

import           Codec.Serialise ( serialise )

import           Cardano.Api hiding (Testnet,Mainnet,Address,Hash)
import qualified Cardano.Api.Shelley  as Shelley


import           Ledger hiding (Address, scriptCurrencySymbol, validatorHash)

import qualified Plutus.V1.Ledger.Scripts as Script
import           Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol, validatorHash)

import           Tokenomia.Common.Environment
import           Tokenomia.Common.Folder (getFolderPath,Folder (..))
import           Tokenomia.Common.Address

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
