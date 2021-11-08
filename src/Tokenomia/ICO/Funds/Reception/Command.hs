{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Tokenomia.ICO.Funds.Reception.Command
    ( RejectReason (..)
    , Command (..)
    , ) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada

import           Blockfrost.Pretty.Ada
import           Ledger ( Slot(..) )

import           Tokenomia.Common.TxOutRef
import           Tokenomia.ICO.Funds.Reception.ChildAddress.State


data RejectReason
    = AddressSaturated
    | FundsWithNativeTokens
    | TransactionOutofRoundTimeRange
    | InsufficientFundsReceived
    deriving (Show,Eq)

data Command
    = Reject {receivedFunds :: ReceivedFunds, reason :: RejectReason}
    | AcceptWithPartialRefund {receivedFunds :: ReceivedFunds, refundAmount :: Ada}
    | Accept {receivedFunds :: ReceivedFunds} deriving Eq

instance Ord Command where 
    compare x y = compare (receivedFunds x) (receivedFunds y)

instance Show Command where
    show Accept {receivedFunds = ReceivedFunds {..}} 
        =  show (getSlot receivedAt) 
            <> " - " 
            <>  showTxOutRef txOutRef <> " : Accept"
    show Reject {receivedFunds = ReceivedFunds {..}, ..} 
        =  show (getSlot receivedAt) 
            <> " - " 
            <> showTxOutRef txOutRef 
            <> " : Reject (" <> show reason <> ")"
    show AcceptWithPartialRefund {receivedFunds = ReceivedFunds {..}, ..}
        =  show (getSlot receivedAt) 
            <> " - " 
            <> showTxOutRef txOutRef 
            <> " : Partially Accept ( refund of " <> show (prettyLovelaces $ fromIntegral refundAmount) <> ")"

