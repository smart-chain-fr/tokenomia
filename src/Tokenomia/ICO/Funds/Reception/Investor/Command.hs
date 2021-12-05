{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Tokenomia.ICO.Funds.Reception.Investor.Command
    ( RejectAdaFundsReason (..)
    , Command (..)
    , isRefund
    , isSendOnExchangeAddressWithPartialRefund
    , isSendOnExchangeAddress
    , getAdas
    ) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada

import           Ledger ( Slot(..) )

import           Tokenomia.Common.TxOutRef
import           Tokenomia.ICO.Funds.Reception.ChildAddress.Types hiding (receivedAt,txOutRef)


data RejectAdaFundsReason
    = AddressSaturated
    | TransactionOutofRoundTimeRange
    deriving (Show,Eq)

data Command
    = Refund 
        { investorRef :: WhiteListedInvestorRef
        , txOutRef :: TxOutRef
        , refundAmount :: Ada
        , receivedAt :: Slot
        , reason :: RejectAdaFundsReason}
    | SendOnExchangeAddressWithPartialRefund 
        { investorRef :: WhiteListedInvestorRef
        , txOutRef :: TxOutRef
        , adasToSendOnExchange :: Ada
        , receivedAt :: Slot
        , refundAmount :: Ada}
    | SendOnExchangeAddress 
        { investorRef :: WhiteListedInvestorRef
        , txOutRef :: TxOutRef
        , adasToSendOnExchange :: Ada
        , receivedAt :: Slot} deriving Eq




instance Ord Command where 
    compare x y = case compare (receivedAt x) (receivedAt y) of 
      LT -> LT
      EQ -> compare (txOutRef x) (txOutRef y) 
      GT -> GT

instance Show Command where
    show Refund { ..}
        =  "\n Command : Refund "
        <> "\n   | received at : "  <> show (getSlot receivedAt)
        <> "\n   | txOutRef  : "    <> show txOutRef 
        <> "\n   | investorRef  : " <> show investorRef
        <> "\n   | amount  : "      <> show refundAmount
        <> "\n   | reason  : "      <> show reason

    show SendOnExchangeAddressWithPartialRefund { ..}
        =  "\n Command : SendOnExchangeAddressWithPartialRefund "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | txOutRef  : "    <> show txOutRef 
        <> "\n   | investorRef  : " <> show investorRef 
        <> "\n   | refund  : "     <> show refundAmount
        <> "\n   | to Sent on exchange adress   : " <> show adasToSendOnExchange
        

    show SendOnExchangeAddress {..}
        =  "\n Command : SendOnExchangeAddress "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | txOutRef  : "    <> show txOutRef 
        <> "\n   | investorRef  : " <> show investorRef
        <> "\n   | amount   : "     <> show adasToSendOnExchange
        


isRefund :: Command -> Bool
isRefund Refund {} = True 
isRefund _ = False

isSendOnExchangeAddressWithPartialRefund :: Command -> Bool
isSendOnExchangeAddressWithPartialRefund SendOnExchangeAddressWithPartialRefund {} = True 
isSendOnExchangeAddressWithPartialRefund _ = False

isSendOnExchangeAddress :: Command -> Bool
isSendOnExchangeAddress SendOnExchangeAddress {} = True 
isSendOnExchangeAddress _ = False


getAdas:: Command -> Ada    
getAdas Refund {..} = refundAmount
getAdas SendOnExchangeAddressWithPartialRefund {..} = adasToSendOnExchange + refundAmount
getAdas SendOnExchangeAddress  {..} = adasToSendOnExchange