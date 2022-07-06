{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tokenomia.ICO.Funds.Validation.CardanoCLI.Transact (
  transact,
  transactWithoutConfirmation,
  buildTx,
) where

import Control.Monad.Except
import Control.Monad.Reader hiding (ask)
import Data.List.NonEmpty
import Data.Set.NonEmpty
import Tokenomia.Common.Environment
import Tokenomia.Common.Transacting
import Prelude hiding (print, round)

import Tokenomia.Common.Error
import Tokenomia.ICO.Funds.Validation.CardanoCLI.Command
import Tokenomia.ICO.Round.Settings

import Ledger.Ada as Ada
import Tokenomia.ICO.Funds.Validation.CardanoCLI.Plan (Plan (..), getTxBalance)

buildTx ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundAddresses ->
  Plan Command ->
  m BuiltTx
buildTx roundAddresses plan = do
  build
    (getTxBalance roundAddresses plan)
    (Just $ getCollateral roundAddresses)
    TxBuild
      { inputsFromWallet = txInputs (commands plan)
      , outputs = txOutputs (commands plan)
      , validitySlotRangeMaybe = Nothing
      , tokenSupplyChangesMaybe = Nothing
      , inputsFromScript = Nothing
      , metadataMaybe = Nothing
      }

transactWithoutConfirmation ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundAddresses ->
  Plan Command ->
  m BuiltTx
transactWithoutConfirmation a b = buildTx a b >>= submitWithoutWaitingConfimation

transact ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundAddresses ->
  Plan Command ->
  m ()
transact a b = buildTx a b >>= submitAndWait

txInputs :: NESet Command -> NonEmpty TxInFromWallet
txInputs xs = FromWallet . source <$> toAscList xs

txOutputs :: NESet Command -> NonEmpty TxOut
txOutputs xs = txCommandOutputs =<< toAscList xs

txCommandOutputs :: Command -> NonEmpty TxOut
txCommandOutputs = \case
  SendOnExchangeAddressAndPartiallyRefund {..} ->
    ToWallet
      { address = exchangeAddress
      , value = Ada.toValue adasToSendOnExchange
      , datumMaybe = Just datum
      }
      :| [ ToWallet
            { address = refundAddress
            , value = Ada.toValue adasToRefund
            , datumMaybe = Nothing
            }
         ]
  SendOnExchangeAddressAndPartiallyMoveToNextRound {..} ->
    ToWallet
      { address = exchangeAddress
      , value = Ada.toValue adasToSendOnExchange
      , datumMaybe = Just datum
      }
      :| [ ToWallet
            { address = nextRoundExchangeAddress
            , value = Ada.toValue adasToMove
            , datumMaybe = Nothing
            }
         ]
  Refund {..} ->
    ToWallet
      { address = refundAddress
      , value = Ada.toValue adasToRefund
      , datumMaybe = Nothing
      }
      :| []
  MoveToNextRound {..} ->
    ToWallet
      { address = nextRoundExchangeAddress
      , value = Ada.toValue adasToMove
      , datumMaybe = Just datum
      }
      :| []
  SendOnExchangeAddress {..} ->
    ToWallet
      { address = exchangeAddress
      , value = Ada.toValue adasToSendOnExchange
      , datumMaybe = Just datum
      }
      :| []
