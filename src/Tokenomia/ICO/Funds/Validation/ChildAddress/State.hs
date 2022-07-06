{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Tokenomia.ICO.Funds.Validation.ChildAddress.State (
  fetchActiveAddresses,
  fetchAllWhiteListedFunds,
  fetchWhiteListedFunds,
  AddressVolumes (..),
  WhiteListedInvestorState (..),
  ReceivedFunds (..),
  NativeTokens,
  Funds,
) where

import Prelude hiding (print, round)

import Data.Set.Ordered as OS (OSet, filter, fromList)
import Plutus.V1.Ledger.Ada (Ada (..))

import Data.Text qualified as T
import Ledger.Ada as Ada (lovelaceOf)

import Blockfrost.Types.Shared.Amount (Amount (..))
import Data.Foldable (Foldable (fold))
import Ledger (Slot (..), TxOutRef (..))

import Control.Monad.Reader

import Blockfrost.Client qualified as B

import Data.List.NonEmpty hiding (fromList)
import Data.String (IsString (fromString))

import Control.Monad.Except
import Data.Coerce
import Tokenomia.Common.Address (Address (..))
import Tokenomia.Common.Blockfrost qualified as B
import Tokenomia.Common.Environment
import Tokenomia.Common.Error
import Tokenomia.Common.PageNumber
import Tokenomia.ICO.Funds.Validation.ChildAddress.Types
import Tokenomia.ICO.Round.Settings
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.Type

fetchActiveAddresses ::
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  RoundAddresses ->
  PageNumber ->
  Wallet ->
  m (Maybe (NonEmpty Address))
fetchActiveAddresses _ pageNumber Wallet {stakeAddress = Address stakeAddress} =
  do
    let paged = B.Paged {countPerPage = 100, pageNumber = coerce pageNumber}
    prj <- B.projectFromEnv''
    liftIO $
      B.runBlockfrost prj $ do
        addresses :: [Address] <-
          (fmap . fmap)
            (Address . T.unpack . coerce)
            (B.getAccountAssociatedAddresses' (fromString stakeAddress) paged B.asc)
        (return . nonEmpty . Prelude.filter notInvalidAddress) addresses
    >>= ( \case
            Left e -> throwError $ BlockFrostError e
            Right res -> return res
        )

fetchAllWhiteListedFunds ::
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  RoundSettings ->
  NonEmpty WhiteListedInvestorRef ->
  m (NonEmpty WhiteListedInvestorState)
fetchAllWhiteListedFunds settings whiteListedInvestorRefs =
  do
    prj <- B.projectFromEnv''
    liftIO $
      B.runBlockfrost prj $ do
        fetchAllWhiteListedFunds' whiteListedInvestorRefs
    >>= ( \case
            Left e -> throwError $ BlockFrostError e
            Right xs -> return $ (\(a, b, c) -> mkWhiteListedInvestorState settings a b c) <$> xs
        )

mkWhiteListedInvestorState ::
  RoundSettings ->
  WhiteListedInvestorRef ->
  AddressVolumes ->
  OSet ReceivedFunds ->
  WhiteListedInvestorState
mkWhiteListedInvestorState RoundSettings {syncSlot = Just syncSlot} investorRef volumes allReceivedFunds =
  WhiteListedInvestorState {allReceivedFunds = OS.filter (\ReceivedFunds {..} -> receivedAt < syncSlot) allReceivedFunds, ..}
mkWhiteListedInvestorState RoundSettings {syncSlot = Nothing} investorRef volumes allReceivedFunds =
  WhiteListedInvestorState {..}

fetchAllWhiteListedFunds' ::
  NonEmpty WhiteListedInvestorRef ->
  B.BlockfrostClient (NonEmpty (WhiteListedInvestorRef, AddressVolumes, OSet ReceivedFunds))
fetchAllWhiteListedFunds' whiteListedInvestorRefs = do
  mapM fetchWhiteListedFunds' whiteListedInvestorRefs

fetchWhiteListedFunds ::
  ( MonadIO m
  , MonadError TokenomiaError m
  , MonadReader Environment m
  ) =>
  RoundSettings ->
  WhiteListedInvestorRef ->
  m WhiteListedInvestorState
fetchWhiteListedFunds settings whiteListedInvestorRef =
  do
    prj <- B.projectFromEnv''
    liftIO $
      B.runBlockfrost prj $ do
        fetchWhiteListedFunds' whiteListedInvestorRef
    >>= ( \case
            Left e -> throwError $ BlockFrostError e
            Right (a, b, c) -> return $ mkWhiteListedInvestorState settings a b c
        )

fetchWhiteListedFunds' ::
  WhiteListedInvestorRef ->
  B.BlockfrostClient (WhiteListedInvestorRef, AddressVolumes, OSet ReceivedFunds)
fetchWhiteListedFunds' w@WhiteListedInvestorRef {indexedAddress = IndexedAddress {..}} = do
  volumes <- fetchAddressVolumes address
  allReceivedFunds <- fetchAllReceivedFunds address
  pure (w, volumes, allReceivedFunds)

fetchAllReceivedFunds :: Address -> B.BlockfrostClient (OSet ReceivedFunds)
fetchAllReceivedFunds addr = fromList <$> (fmap .fmap) (\(a, b, c) -> mkReceivedFunds a b c) (fetchUTxOsDetails addr)

fetchUTxOsDetails :: Address -> B.BlockfrostClient [(B.AddressUtxo, B.Transaction, B.TransactionUtxos)]
fetchUTxOsDetails (Address addr) = do
  utxos <- B.getAddressUtxos (fromString addr)
  mapM
    ( \x@B.AddressUtxo {_addressUtxoTxHash = hash} -> do
        transaction <- B.getTx hash
        transactionUtxos <- B.getTxUtxos hash
        return (x, transaction, transactionUtxos)
    )
    utxos

mkReceivedFunds :: B.AddressUtxo -> B.Transaction -> B.TransactionUtxos -> ReceivedFunds
mkReceivedFunds
  B.AddressUtxo {..}
  B.Transaction {..}
  _ =
    ReceivedFunds
      { txOutRef =
          TxOutRef
            { txOutRefId = fromString . T.unpack . coerce $_addressUtxoTxHash
            , txOutRefIdx = _addressUtxoOutputIndex
            }
      , receivedAt = (Slot . coerce) _transactionSlot
      , funds = splitNativeTokensAndAdas _addressUtxoAmount
      }

splitNativeTokensAndAdas :: [Amount] -> Either NativeTokens Ada
splitNativeTokensAndAdas [AdaAmount x] = Right $ Ada.lovelaceOf (fromIntegral x)
splitNativeTokensAndAdas x = Left x

fetchAddressVolumes :: Address -> B.BlockfrostClient AddressVolumes
fetchAddressVolumes (Address addr) = do
  B.AddressDetails {..} <- B.getAddressDetails (fromString addr)
  return
    AddressVolumes
      { received = fold (filterOnlyLovelaces <$> _addressDetailsReceivedSum)
      , sent = fold (filterOnlyLovelaces <$> _addressDetailsSentSum)
      }

filterOnlyLovelaces :: Amount -> Ada
filterOnlyLovelaces (AdaAmount x) = Ada.lovelaceOf (fromIntegral x)
filterOnlyLovelaces (AssetAmount _) = Ada.lovelaceOf 0
