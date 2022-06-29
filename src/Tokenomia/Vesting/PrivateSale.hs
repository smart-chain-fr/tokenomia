{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Tokenomia.Vesting.PrivateSale (verifyPrivateSale) where

import Ledger qualified
import Ledger.Value qualified as Value
import Prelude

import Control.Monad
import Control.Monad.Reader
import GHC.Generics (Generic)

import Data.Hex (unhex)
import Data.String (fromString)

import Debug.Trace (traceShowId)

import Control.Lens
import Control.Monad.Except
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Bifunctor (first)
import Data.Foldable (find)
import Data.Text (Text, pack, unpack)
import Money qualified as M
import System.IO (FilePath, getLine, putStrLn)

import Blockfrost.Client qualified as B
import Blockfrost.Lens qualified as B

import Tokenomia.Common.Blockfrost (projectFromEnv'')
import Tokenomia.Common.Environment
import Tokenomia.Common.Error

data PrivateSale = PrivateSale
  { -- | Treasury address
    _psAddress :: B.Address
  , _psStart :: Ledger.POSIXTime
  , _psTranches :: [Tranche]
  , -- | Vesting token
    _psAssetClass :: Value.AssetClass
  , _psInvestors :: [PrivateInvestor]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data Tranche = Tranche
  { _tranchePercentage :: Integer
  , _trancheDuration :: Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data PrivateInvestor = PrivateInvestor
  { _piAddress :: B.Address
  , -- | Amount of vesting tokens to lock (note, this is likely NOT equal
    -- to the total amounts of investments, as different tokens)
    _piAllocation :: Integer
  , _piInvestments :: [Investment]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data Investment = Investment
  { -- | Tx that sends below asset class to payment address
    _invTx :: B.TxHash
  , _invAssetClass :: Value.AssetClass
  , -- | Amount of above asset class expected to be sent to payment address
    _invAmount :: Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

makeLenses ''PrivateSale
makeLenses ''Tranche
makeLenses ''PrivateInvestor
makeLenses ''Investment

verifyPrivateSale ::
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  m ()
verifyPrivateSale = do
  liftIO . putStrLn $ "Please enter a filepath with JSON data"
  jsonFilePath <- liftIO getLine
  verifyPrivateSale' jsonFilePath

verifyPrivateSale' ::
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  FilePath ->
  m ()
verifyPrivateSale' jsonFilePath = do
  ps <- jsonToPrivateSale jsonFilePath
  treasAddrTxs <- getTreasAddrTxs ps -- :: [B.AddressTransaction] <- m [B.AddressTransaction]
  let invTxhs = getTxhsByPrivateSale ps
      txhs = verifyTxHashList invTxhs treasAddrTxs
  if length txhs == length invTxhs
    then do
      verifyTxs ps txhs
    else throwError . BlockFrostError . B.BlockfrostError $ "Missing Transactions"

jsonToPrivateSale ::
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  FilePath ->
  m PrivateSale
jsonToPrivateSale jsonFilePath = do
  eitherErrPs <- liftIO . eitherDecodeFileStrict $ jsonFilePath
  liftEither (first (BlockFrostError . B.BlockfrostError . pack) eitherErrPs)

getTreasAddrTxs ::
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  PrivateSale ->
  m [B.AddressTransaction]
getTreasAddrTxs ps = do
  prj <- projectFromEnv''
  eitherErrAddrTxs <- liftIO $ B.runBlockfrost prj (B.getAddressTransactions (ps ^. psAddress))
  liftEither $ first BlockFrostError eitherErrAddrTxs

getTxhsByPrivateSale :: PrivateSale -> [B.TxHash]
getTxhsByPrivateSale ps = (^. invTx) <$> getPsInvestments ps

verifyTxs ::
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  PrivateSale ->
  [B.TxHash] ->
  m ()
verifyTxs ps =
  mapM_ $ verifyTx invs
  where
    invs :: [Investment]
    invs = getPsInvestments ps

    verifyTx ::
      ( MonadIO m
      , MonadError TokenomiaError m
      , MonadReader Environment m
      ) =>
      [Investment] ->
      B.TxHash ->
      m ()
    verifyTx invs txh = do
      bfTxUtxos <- getTxUtxosByTxHash txh -- :: B.TransactionUtxos
      inv <- liftEither $ getInvByTxHash (traceShowId txh) invs -- :: Investment
      let bfUtxoOutputs = bfTxUtxos ^. B.outputs -- :: [B.UtxoOutput]
          treasAddrOutputs = filter (\output -> (output ^. B.address) == (ps ^. psAddress)) bfUtxoOutputs -- :: [B.UtxoOutput]
          bfAmts = concat ((^. B.amount) <$> treasAddrOutputs) -- :: [B.Amount]
          bfValues = amountToAssetValue <$> bfAmts -- :: [(Value.AssetClass, Integer)]
          confirmedVals = confirmValues inv (traceShowId bfValues) -- :: Integer
          invAmount' = inv ^. invAmount -- :: Integer
          invAssetClass' = inv ^. invAssetClass -- :: Value.AssetClass
       in unless (invAmount' == confirmedVals && invAssetClass' == inv ^. invAssetClass) $
            throwError . BlockFrostError . B.BlockfrostError $ "Values don't match"

confirmValues :: Investment -> [(Value.AssetClass, Integer)] -> Integer
confirmValues inv = foldr (\val z -> if fst val == inv ^. invAssetClass then snd val + z else z) 0

getTxUtxosByTxHash ::
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  B.TxHash ->
  m B.TransactionUtxos
getTxUtxosByTxHash txh = do
  prj <- projectFromEnv''
  eitherErrTxUtxos <- liftIO $ B.runBlockfrost prj (B.getTxUtxos txh)
  liftEither $ first BlockFrostError eitherErrTxUtxos

getInvByTxHash :: B.TxHash -> [Investment] -> Either TokenomiaError Investment
getInvByTxHash txh invs =
  maybe
    ( Left . BlockFrostError . B.BlockfrostError $
        "Investment list doesn't contain matching TxHash"
    )
    Right
    (find (\inv -> (inv ^. invTx) == txh) invs)

getPsInvestments ::
  PrivateSale ->
  [Investment]
getPsInvestments ps = concat ((^. piInvestments) <$> investors)
  where
    investors :: [PrivateInvestor]
    investors = ps ^. psInvestors

verifyTxHashList :: [B.TxHash] -> [B.AddressTransaction] -> [B.TxHash]
verifyTxHashList txhs addrTxs =
  filter (`elem` txhs) ((^. B.txHash) <$> addrTxs)

{-
  Below, currSymPart is the first part of a 2-part value returned by M.someDiscreteCurrency,
  and it's 56 characters long. The String returned by M.someDiscreteCurrency has the
  form "CurrencysymbolName" :: Text, where we have to manually extract the currency symbol
  embedded in the first half using take, and extract the token name in the second half
  using drop.
-}

amountToAssetValue :: B.Amount -> (Value.AssetClass, Integer)
amountToAssetValue (B.AdaAmount ll) = (Value.assetClass "" "", M.someDiscreteAmount $ M.toSomeDiscrete ll)
amountToAssetValue (B.AssetAmount sd) = (Value.assetClass (fromString currSym) (fromString name), M.someDiscreteAmount (traceShowId sd))
  where
    encodedName :: String
    encodedName = unpack $ M.someDiscreteCurrency sd

    currSym :: String
    currSym = take currSymPart encodedName

    currSymPart :: Int
    currSymPart = 56

    name :: String
    name = either error id $ unhex $ drop currSymPart encodedName
