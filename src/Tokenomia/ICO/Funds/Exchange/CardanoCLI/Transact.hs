{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tokenomia.ICO.Funds.Exchange.CardanoCLI.Transact (
  transact,
  buildTx,
) where

import Control.Monad.Except
import Control.Monad.Reader hiding (ask)
import Data.List.NonEmpty
import Data.Set.NonEmpty
import Ledger.Ada as Ada
import Ledger.Value
import PlutusTx.Prelude (AdditiveSemigroup ((+)))
import Tokenomia.Common.Address
import Tokenomia.Common.Environment
import Tokenomia.Common.Error
import Tokenomia.Common.Token
import Tokenomia.Common.Transacting
import Tokenomia.ICO.Funds.Exchange.CardanoCLI.Command
import Tokenomia.ICO.Round.Settings
import Prelude hiding (print, round, (+), (-))

import Tokenomia.Common.Shell.Console (printLn)
import Tokenomia.ICO.Funds.Exchange.Plan (IOOnTokenAddress, Plan (..), getTxBalance)
import Tokenomia.ICO.Funds.Exchange.Plan qualified as Plan
import Tokenomia.ICO.Funds.Exchange.Tokens qualified as Plan

buildTx ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundAddresses ->
  Plan Command ->
  m BuiltTx
buildTx roundAddresses plan = do
  let tx =
        TxBuild
          { inputsFromWallet =
              appendList
                (txInputs (commands plan))
                (txInputFromTokenAddress (ioOnTokenAddress plan))
          , outputs =
              appendList
                (txOutputs (adaSink roundAddresses) (commands plan))
                (txOutputToTokenAddress (getTokenAddress roundAddresses) (ioOnTokenAddress plan))
          , validitySlotRangeMaybe = Nothing
          , tokenSupplyChangesMaybe = Nothing
          , inputsFromScript = Nothing
          , metadataMaybe = Nothing
          }
  -- printLn $ "Tx > " <> show tx
  build
    (getTxBalance roundAddresses plan)
    (Just $ getCollateral roundAddresses)
    tx

transact ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundAddresses ->
  Plan Command ->
  m ()
transact a b = do
  printLn "transact.building"
  r <- buildTx a b
  printLn "transact.built"
  submitAndWait r

txInputFromTokenAddress :: Maybe IOOnTokenAddress -> [TxInFromWallet]
txInputFromTokenAddress Nothing = []
txInputFromTokenAddress (Just Plan.IOOnTokenAddress {source = Plan.ExchangeToken {..}}) = [FromWallet source]

txInputs :: NESet Command -> NonEmpty TxInFromWallet
txInputs xs = FromWallet . source <$> toAscList xs

txOutputToTokenAddress :: Address -> Maybe IOOnTokenAddress -> [TxOut]
txOutputToTokenAddress _ Nothing = []
txOutputToTokenAddress _ (Just Plan.IOOnTokenAddress {remainingTokensMaybe = Nothing}) = []
txOutputToTokenAddress tokenAddress (Just Plan.IOOnTokenAddress {remainingTokensMaybe = Just Token {assetClass = tokenAssetClass, ..}}) =
  [ ToWallet -- Putting back the remaining tokens
      { address = tokenAddress
      , value = assetClassValue tokenAssetClass amount + Ada.toValue minimumAdaRequired
      , datumMaybe = Nothing
      }
  ]

txOutputs :: Address -> NESet Command -> NonEmpty TxOut
txOutputs exchangeAddress xs = txCommandOutputs exchangeAddress =<< toAscList xs

txCommandOutputs :: Address -> Command -> NonEmpty TxOut
txCommandOutputs adaSink = \case
  RefundBecauseTokensSoldOut {..} ->
    ToWallet --refund
      { address = paybackAddress
      , value = Ada.toValue refundAmount
      , datumMaybe = Nothing
      }
      :| []
  MoveToNextRoundBecauseTokensSoldOut {..} ->
    ToWallet --Move Next Round
      { address = nextRoundExchangeAddress
      , value = Ada.toValue moveAmount
      , datumMaybe = Just datumFile
      }
      :| []
  ExchangeAndPartiallyRefund {tokens = Token {assetClass = tokenAssetClass, ..}, ..} ->
    ToWallet -- Exchange
      { address = paybackAddress
      , value = assetClassValue tokenAssetClass amount + Ada.toValue minimumAdaRequired
      , datumMaybe = Nothing
      }
      :| [ ToWallet --refund
            { address = paybackAddress
            , value = Ada.toValue refundAmount
            , datumMaybe = Nothing
            }
         , ToWallet -- Ada Collection
            { address = adaSink
            , value = Ada.toValue collectedAmount
            , datumMaybe = Nothing
            }
         ]
  ExchangeAndPartiallyMoveToNextRound {tokens = Token {assetClass = tokenAssetClass, ..}, ..} ->
    ToWallet -- Exchange
      { address = paybackAddress
      , value = assetClassValue tokenAssetClass amount + Ada.toValue minimumAdaRequired
      , datumMaybe = Nothing
      }
      :| [ ToWallet --Move Next Round
            { address = nextRoundExchangeAddress
            , value = Ada.toValue moveAmount
            , datumMaybe = Just datumFile
            }
         , ToWallet -- Ada Collection
            { address = adaSink
            , value = Ada.toValue collectedAmount
            , datumMaybe = Nothing
            }
         ]
  Exchange {tokens = Token {assetClass = tokenAssetClass, ..}, ..} ->
    ToWallet -- Exchange
      { address = paybackAddress
      , value = assetClassValue tokenAssetClass amount + Ada.toValue minimumAdaRequired
      , datumMaybe = Nothing
      }
      :| [ ToWallet -- Ada Collection
            { address = adaSink
            , value = Ada.toValue collectedAmount
            , datumMaybe = Nothing
            }
         ]

appendList :: NonEmpty a -> [a] -> NonEmpty a
appendList (x :| xs) ys = x :| xs <> ys
