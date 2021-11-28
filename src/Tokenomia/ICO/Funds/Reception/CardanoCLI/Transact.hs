{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Reception.CardanoCLI.Transact
    (buildTx,buildAndSubmitTx) where

import           Prelude hiding (round,print)
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except
import           Data.Set.NonEmpty
import           Data.List.NonEmpty
import           Tokenomia.Common.Environment
import           Tokenomia.Common.Transacting

import           Tokenomia.ICO.Funds.Reception.CardanoCLI.Command
import           Tokenomia.Common.Error
import           Tokenomia.ICO.RoundSettings
import           Tokenomia.Common.Address
import           Ledger.Ada as Ada

buildTx
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => RoundAddresses -> NESet Command  -> m BuiltTx
buildTx roundAddresses commands =
    build
      (getCollateral roundAddresses)
      (getFees roundAddresses)
      TxBuild
        { inputsFromWallet  = txInputs commands
        , outputs = txOutputs (getExchangeAddress roundAddresses) commands
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , inputsFromScript  = Nothing
        , metadataMaybe = Nothing}

buildAndSubmitTx
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => RoundAddresses -> NESet Command  -> m ()
buildAndSubmitTx a b =  buildTx a b >>= submitAndWait   

txInputs :: NESet Command -> NonEmpty TxInFromWallet
txInputs xs = FromWallet . source <$> toAscList xs


txOutputs :: Address -> NESet Command -> NonEmpty TxOut
txOutputs exchangeAddress xs = txCommandOutputs exchangeAddress =<< toAscList xs

txCommandOutputs :: Address -> Command -> NonEmpty TxOut
txCommandOutputs exchangeAddress = \case
  TransferAndPartiallyRefund {..} -> 
      ToWallet 
        { address = exchangeAddress
        , value = Ada.toValue adas
        , datumMaybe = Just datum } :| 
      [ToWallet 
        { address = refundAddress
        , value = Ada.toValue adasToBeRefund
        , datumMaybe = Nothing }]
  Refund {..} -> 
      ToWallet 
        { address = refundAddress
        , value = Ada.toValue adasToBeRefund
        , datumMaybe = Nothing } :| []
  Transfer {..} -> 
      ToWallet 
        { address = exchangeAddress
        , value = Ada.toValue adas
        , datumMaybe = Just datum } :| []
