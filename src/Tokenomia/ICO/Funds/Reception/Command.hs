{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Tokenomia.ICO.Funds.Reception.Command
    ( RejectAdaFundsReason (..)
    , Command (..)
    , isRejectFundsWithNativeTokens
    , isReject
    , isAcceptWithPartialRefund
    , isAccept
    ) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada

import           Blockfrost.Pretty.Ada
import           Ledger ( Slot(..) )

import           Tokenomia.Common.TxOutRef
import           Tokenomia.ICO.Funds.Reception.ChildAddress.State hiding (receivedAt,txOutRef)
import           Tokenomia.ICO.Funds.Reception.ChildAddress.Types hiding (receivedAt,txOutRef)


data RejectAdaFundsReason
    = AddressSaturated
    | TransactionOutofRoundTimeRange
    | InsufficientFundsReceived
    deriving (Show,Eq)

data Command
    = RejectFundsWithNativeTokens 
        { investorRef :: WhiteListedInvestorRef
        , txOutRef :: TxOutRef
        , nativeTokens :: NativeTokens
        , receivedAt :: Slot }
    | Reject 
        { investorRef :: WhiteListedInvestorRef
        , txOutRef :: TxOutRef
        , adas :: Ada
        , receivedAt :: Slot
        , reason :: RejectAdaFundsReason}
    | AcceptWithPartialRefund 
        { investorRef :: WhiteListedInvestorRef
        , txOutRef :: TxOutRef
        , adas :: Ada
        , receivedAt :: Slot
        , refundAmount :: Ada}
    | Accept 
        { investorRef :: WhiteListedInvestorRef
        , txOutRef :: TxOutRef
        , adas :: Ada
        , receivedAt :: Slot} deriving Eq

instance Ord Command where 
    compare x y = case compare (receivedAt x) (receivedAt y) of 
      LT -> LT
      EQ -> compare (txOutRef x) (txOutRef y) 
      GT -> GT

instance Show Command where
    show RejectFundsWithNativeTokens { ..} 
        =  show (getSlot receivedAt) 
            <> " - " 
            <> showTxOutRef txOutRef 
            <> " : Reject ( NativeTokenFunds)"
    show Reject {..} 
        =  show (getSlot receivedAt) 
            <> " - " 
            <> showTxOutRef txOutRef 
            <> " : Reject (" <> show reason <> ")"
    show Accept {..} 
        =  show (getSlot receivedAt) 
            <> " - " 
            <>  showTxOutRef txOutRef <> " : Accept"
    show AcceptWithPartialRefund {..}
        =  show (getSlot receivedAt) 
            <> " - " 
            <> showTxOutRef txOutRef 
            <> " : Partially Accept ( refund of " <> show (prettyLovelaces $ fromIntegral refundAmount) <> ")"



isRejectFundsWithNativeTokens :: Command -> Bool
isRejectFundsWithNativeTokens RejectFundsWithNativeTokens {} = True 
isRejectFundsWithNativeTokens _ = False

isReject :: Command -> Bool
isReject Reject {} = True 
isReject _ = False

isAcceptWithPartialRefund :: Command -> Bool
isAcceptWithPartialRefund AcceptWithPartialRefund {} = True 
isAcceptWithPartialRefund _ = False

isAccept :: Command -> Bool
isAccept Accept {} = True 
isAccept _ = False
