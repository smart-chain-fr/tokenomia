{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Exchange.Transact
    ( transact
    , buildTx) where

import           Prelude hiding (round,print,(+), (-))
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except
import           Data.Set.NonEmpty
import           Data.List.NonEmpty
import           Tokenomia.Common.Environment
import           Tokenomia.Common.Transacting
import           Tokenomia.ICO.Funds.Exchange.Command
import           Tokenomia.Common.Error
import           Tokenomia.ICO.Round.Settings
import           Tokenomia.Common.Address
import           Ledger.Ada as Ada
import           Ledger.Value
import           Tokenomia.Common.Token
import           PlutusTx.Prelude  (AdditiveSemigroup((+)))

import           Tokenomia.ICO.Funds.Exchange.Plan (Plan (..),getTxBalance,IOOnTokenAddress )
import qualified Tokenomia.ICO.Funds.Exchange.Plan as Plan
import           Tokenomia.Common.Shell.Console (printLn)
import qualified Tokenomia.ICO.Funds.Exchange.Tokens as Plan

buildTx
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => RoundAddresses
    -> Plan Command
    -> m BuiltTx
buildTx roundAddresses plan =
    build
      (getTxBalance roundAddresses plan)
      (Just $ getCollateral roundAddresses)
      TxBuild
        { inputsFromWallet  = appendList 
                                (txInputs  (commands plan))  
                                (txInputFromTokenAddress (ioOnTokenAddress plan)) 
        , outputs = appendList
                      (txOutputs (adaSink roundAddresses) (commands plan))
                      (txOutputToTokenAddress (getTokenAddress roundAddresses) (ioOnTokenAddress plan))
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
transact a b = do 
  printLn "transact.building"
  r <- buildTx a b 
  printLn "transact.built"
  submitAndWait r


txInputFromTokenAddress ::  Maybe IOOnTokenAddress -> [TxInFromWallet]
txInputFromTokenAddress  Nothing = []
txInputFromTokenAddress  (Just Plan.IOOnTokenAddress {source = Plan.ExchangeToken {..}})  = [FromWallet source]


txInputs :: NESet Command -> NonEmpty TxInFromWallet
txInputs xs = FromWallet . source <$> toAscList xs

txOutputToTokenAddress :: Address -> Maybe IOOnTokenAddress -> [TxOut]
txOutputToTokenAddress _ Nothing = []
txOutputToTokenAddress _ (Just Plan.IOOnTokenAddress {remainingTokensMaybe  = Nothing})  = []
txOutputToTokenAddress tokenAddress (Just Plan.IOOnTokenAddress {remainingTokensMaybe = Just Token {assetClass = tokenAssetClass,..}})  
  = [ToWallet -- Putting back the remaining tokens
      { address = tokenAddress
      , value = assetClassValue tokenAssetClass amount + Ada.toValue minimumAdaRequired
      , datumMaybe = Nothing}]

txOutputs :: Address -> NESet Command -> NonEmpty TxOut
txOutputs exchangeAddress xs = txCommandOutputs exchangeAddress =<< toAscList xs

txCommandOutputs :: Address -> Command -> NonEmpty TxOut
txCommandOutputs adaSink = \case
  RefundBecauseTokensSoldOut {..} ->
      ToWallet --refund
        { address = paybackAddress
        , value = Ada.toValue refundAmount
        , datumMaybe = Nothing } :| []
  ExchangeAndPartiallyRefund {tokens = Token {assetClass = tokenAssetClass,..},..} ->
      ToWallet -- Exchange
        { address = paybackAddress
        , value = assetClassValue tokenAssetClass amount + Ada.toValue minimumAdaRequired
        , datumMaybe = Nothing} :|
      [ToWallet --refund
        { address = paybackAddress
        , value = Ada.toValue refundAmount
        , datumMaybe = Nothing } ,
       ToWallet -- Ada Collection
        { address = adaSink
        , value =  Ada.toValue collectedAmount
        , datumMaybe = Nothing}]          
  Exchange {tokens = Token {assetClass = tokenAssetClass,..},..} ->
      ToWallet -- Exchange
        { address = paybackAddress
        , value = assetClassValue tokenAssetClass amount + Ada.toValue minimumAdaRequired 
        , datumMaybe = Nothing} :|
      [ToWallet -- Ada Collection
        { address = adaSink
        , value =  Ada.toValue collectedAmount
        , datumMaybe = Nothing}]


appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (x :| xs) ys = x :| xs <> ys