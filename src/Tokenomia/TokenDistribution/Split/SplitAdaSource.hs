{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.TokenDistribution.Split.SplitAdaSource (
  splitAdaSource,
) where

import Prelude hiding (head, repeat, zipWith3)

import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadIO, MonadReader)

import Data.List.NonEmpty (NonEmpty ((:|)), fromList, repeat)

import Ledger.Ada (Ada, lovelaceValueOf, toValue)
import Ledger.Value (Value)

import Tokenomia.Common.AssetClass (adaAssetClass)
import Tokenomia.Common.Environment (Environment)
import Tokenomia.Common.Error (TokenomiaError)
import Tokenomia.Wallet.WalletUTxO (WalletUTxO)

import Tokenomia.Common.Transacting (
  Metadata (..),
  TxBalance (..),
  TxBuild (..),
  TxInFromWallet (..),
  TxOut (..),
  buildAndSubmit,
 )

import Tokenomia.Common.Data.List.NonEmpty (singleton, zipWith3)

import Tokenomia.TokenDistribution.CLI.Parameters (Parameters (..))
import Tokenomia.TokenDistribution.Distribution (Distribution (..), countRecipients)

import Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository (
  fetchAddressesByWalletWithIndexInRange,
 )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef (
  defaultCollateralAddressRef,
  defaultFeeAddressRef,
 )

splitAdaSource ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  WalletUTxO ->
  Ada ->
  Parameters ->
  NonEmpty Distribution ->
  m ()
splitAdaSource source fees parameters distributions = do
  splitAdaSourceTxBuild source fees parameters distributions
    >>= buildAndSubmit
      (Unbalanced $ defaultFeeAddressRef $ adaWallet parameters)
      (Just $ defaultCollateralAddressRef $ collateralWallet parameters)

splitAdaSourceTxBuild ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  WalletUTxO ->
  Ada ->
  Parameters ->
  NonEmpty Distribution ->
  m TxBuild
splitAdaSourceTxBuild source fees parameters distributions = do
  outputs <- splitAdaSourceOutputs fees parameters distributions
  return
    TxBuild
      { inputsFromScript = Nothing
      , inputsFromWallet = singleton $ FromWallet source
      , outputs = outputs
      , validitySlotRangeMaybe = Nothing
      , metadataMaybe = Metadata <$> metadataFilePath parameters
      , tokenSupplyChangesMaybe = Nothing
      }

-- Let ε be the value corresponding to the minimum ada needed to create an UTxO.
--
-- When sending token to many addresses for a given batch,
-- we need to cover multiple ε to create output UTxOs
-- containing the token for each recipients.
--
-- Considering the single input token UTxO already contains exactly one ε,
-- sending to n recipients requires an additional (n - 1) ε.
--
-- The split of ada source needs to cover those extra ε besides fees,
-- unless the batch contains a single recipient
-- or the assets to distribute are ada.
--
-- Assuming the fees are less than ε, fees alone are not sufficient
-- to create the output UTxOs for the split. In the last two cases,
-- adding an additional ε is sufficient. This value must be retrived later on a
-- change address, as a by-product of the parallelization process
-- that requires splitting input sources.

splitAdaSourceOutputs ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  Ada ->
  Parameters ->
  NonEmpty Distribution ->
  m (NonEmpty TxOut)
splitAdaSourceOutputs fees Parameters {..} distributions = do
  let range = [1 .. (length distributions)]
  addresses <- fromList <$> fetchAddressesByWalletWithIndexInRange range adaWallet

  return $ zipWith3 ToWallet addresses values (repeat Nothing)
  where
    ε :: Value
    ε = nε 1

    nε :: Integer -> Value
    nε n = lovelaceValueOf (n * minLovelacesPerUtxo)

    distributeAda :: Bool
    distributeAda = case distributions of
      Distribution {..} :| _ -> assetClass == adaAssetClass

    value :: Integer -> Value
    value n
      | n <= 1 || distributeAda = toValue fees <> ε
      | otherwise = toValue fees <> nε (n - 1)

    values :: NonEmpty Value
    values =
      let ns = countRecipients <$> distributions
       in value <$> ns
