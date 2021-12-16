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
    ( transact
    , buildTx) where

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
import           Tokenomia.ICO.Funds.Reception.CardanoCLI.Plan (Plan(..),getTxBalance)



buildTx
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => RoundAddresses 
    -> Plan Command  
    -> m BuiltTx
buildTx roundAddresses plan = do
    build
      (getTxBalance roundAddresses plan)
      (Just $ getCollateral roundAddresses)
      TxBuild
              { inputsFromWallet  = txInputs (commands plan)
              , outputs = txOutputs (getExchangeAddress roundAddresses) (commands plan)
              , validitySlotRangeMaybe = Nothing
              , tokenSupplyChangesMaybe = Nothing
              , inputsFromScript  = Nothing
              , metadataMaybe = Nothing}
     

transact
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => RoundAddresses 
    -> Plan Command  
    -> m ()
transact a b =  buildTx a b >>= submitAndWait   

txInputs :: NESet Command -> NonEmpty TxInFromWallet
txInputs xs = FromWallet . source <$> toAscList xs


txOutputs :: Address -> NESet Command -> NonEmpty TxOut
txOutputs exchangeAddress xs = txCommandOutputs exchangeAddress =<< toAscList xs

txCommandOutputs :: Address -> Command -> NonEmpty TxOut
txCommandOutputs exchangeAddress = \case
  SendOnExchangeAddressWithPartialRefund {..} -> 
      ToWallet 
        { address = exchangeAddress
        , value = Ada.toValue adasToSendOnExchange
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
  SendOnExchangeAddress {..} -> 
      ToWallet 
        { address = exchangeAddress
        , value = Ada.toValue adasToSendOnExchange
        , datumMaybe = Just datum } :| []
