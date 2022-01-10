{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module Tokenomia.ICO.Funds.Reception.Investor.Command
    ( RejectAdaFundsReason (..)
    , Command (..)
    , isRefund
    , isSendOnExchangeAddressWithPartialRefund
    , isSendOnExchangeAddress
    , getAdas
    , getAdasToRefundBecauseAdresseSaturated
    , getAdasToRefundBecauseOutOfRange
    , getAdasSendOnExchange
    ) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada

import           Ledger ( Slot(..) )

import           Tokenomia.Common.TxOutRef
import           Tokenomia.ICO.Funds.Reception.ChildAddress.Types hiding (receivedAt,txOutRef,getAdas)
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Data.Coerce

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
        <> "\n   | amount  : "      <> show refundAmount
        <> "\n   | reason  : "      <> show reason
        <> "\n   | txOutRef  : "    <> show txOutRef 
        <> "\n   | investorRef  : " <> showInvestorRef investorRef

    show SendOnExchangeAddressWithPartialRefund { ..}
        =  "\n Command : SendOnExchangeAddressWithPartialRefund "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | refund  : "     <> show refundAmount
        <> "\n   | to Sent on exchange adress   : " <> show adasToSendOnExchange
        <> "\n   | txOutRef  : "    <> show txOutRef 
        <> "\n   | investorRef  : " <> showInvestorRef investorRef 
        

    show SendOnExchangeAddress {..}
        =  "\n Command : SendOnExchangeAddress "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | amount   : "     <> show adasToSendOnExchange
        <> "\n   | txOutRef  : "    <> show txOutRef 
        <> "\n   | investorRef  : " <> showInvestorRef investorRef
        

showInvestorRef :: WhiteListedInvestorRef -> String
showInvestorRef WhiteListedInvestorRef {indexedAddress = IndexedAddress {childAddressRef = ChildAddressRef {..},..}, ..}
       =  "\n       > reception location"
       <> "\n         | address = " <> show address
       <> "\n         | index   = " <> (show @Integer . coerce) index
       <> "\n       > exchange payback address = " <> show exchangePaybackAddress

isRefund :: Command -> Bool
isRefund Refund {} = True 
isRefund _ = False

isSendOnExchangeAddressWithPartialRefund :: Command -> Bool
isSendOnExchangeAddressWithPartialRefund SendOnExchangeAddressWithPartialRefund {} = True 
isSendOnExchangeAddressWithPartialRefund _ = False

isSendOnExchangeAddress :: Command -> Bool
isSendOnExchangeAddress SendOnExchangeAddress {} = True 
isSendOnExchangeAddress _ = False

getAdasToRefundBecauseAdresseSaturated :: Command -> Ada
getAdasToRefundBecauseAdresseSaturated Refund {reason = AddressSaturated,..} = refundAmount
getAdasToRefundBecauseAdresseSaturated SendOnExchangeAddressWithPartialRefund {..} = refundAmount
getAdasToRefundBecauseAdresseSaturated _  =  0


getAdasSendOnExchange :: Command -> Ada
getAdasSendOnExchange SendOnExchangeAddress {..} = adasToSendOnExchange
getAdasSendOnExchange SendOnExchangeAddressWithPartialRefund {..} = adasToSendOnExchange
getAdasSendOnExchange _  =  0

getAdasToRefundBecauseOutOfRange :: Command -> Ada
getAdasToRefundBecauseOutOfRange Refund {reason = TransactionOutofRoundTimeRange,..} = refundAmount
getAdasToRefundBecauseOutOfRange _  =  0

getAdas:: Command -> Ada    
getAdas Refund {..} = refundAmount
getAdas SendOnExchangeAddressWithPartialRefund {..} = adasToSendOnExchange + refundAmount
getAdas SendOnExchangeAddress  {..} = adasToSendOnExchange