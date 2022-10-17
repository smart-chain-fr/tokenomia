{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
module Tokenomia.ICO.Funds.Validation.Investor.Command
    ( RejectAdaFundsReason (..)
    , Command (..)
    , isReject
    , isSendOnExchangeAddressAndPartiallyReject
    , isSendOnExchangeAddress
    , getAdas
    , getAdasToRejectBecauseAdresseSaturated
    , getAdasToRejectBecauseOutOfRange
    , getAdasSendOnExchange
    ) where

import           Prelude hiding (round,print)

import           Ledger.Ada

import           Ledger ( Slot(..) )

import           Tokenomia.Common.TxOutRef
import           Tokenomia.ICO.Funds.Validation.ChildAddress.Types hiding (receivedAt,txOutRef,getAdas)
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Data.Coerce

data RejectAdaFundsReason
    = AddressSaturated
    | TransactionOutofRoundTimeRange
    deriving (Show,Eq)

data Command
    = Reject 
        { investorRef :: WhiteListedInvestorRef
        , txOutRef :: TxOutRef
        , amountToReject :: Ada
        , receivedAt :: Slot
        , reason :: RejectAdaFundsReason }
    | SendOnExchangeAddressWithPartialReject 
        { investorRef :: WhiteListedInvestorRef
        , txOutRef :: TxOutRef
        , adasToSendOnExchange :: Ada
        , receivedAt :: Slot
        , amountToReject :: Ada }
    | SendOnExchangeAddress 
        { investorRef :: WhiteListedInvestorRef
        , txOutRef :: TxOutRef
        , adasToSendOnExchange :: Ada
        , receivedAt :: Slot } deriving Eq


instance Ord Command where 
    compare x y = case compare (receivedAt x) (receivedAt y) of 
      LT -> LT
      EQ -> compare (txOutRef x) (txOutRef y) 
      GT -> GT

instance Show Command where
    show Reject { ..}
        =  "\n Command : Reject "
        <> "\n   | received at : "  <> show (getSlot receivedAt)
        <> "\n   | amount  : "      <> show amountToReject
        <> "\n   | reason  : "      <> show reason
        <> "\n   | txOutRef  : "    <> show txOutRef 
        <> "\n   | investorRef  : " <> showInvestorRef investorRef

    show SendOnExchangeAddressWithPartialReject { ..}
        =  "\n Command : SendOnExchangeAddressAndPartiallyReject "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | reject  : "     <> show amountToReject
        <> "\n   | sent on exchange adress   : " <> show adasToSendOnExchange
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
       <> "\n       > exchange payback address = " <> show paybackAddress

isReject :: Command -> Bool
isReject Reject {} = True 
isReject _ = False

isSendOnExchangeAddressAndPartiallyReject :: Command -> Bool
isSendOnExchangeAddressAndPartiallyReject SendOnExchangeAddressWithPartialReject {} = True 
isSendOnExchangeAddressAndPartiallyReject _ = False

isSendOnExchangeAddress :: Command -> Bool
isSendOnExchangeAddress SendOnExchangeAddress {} = True 
isSendOnExchangeAddress _ = False

getAdasToRejectBecauseAdresseSaturated :: Command -> Ada
getAdasToRejectBecauseAdresseSaturated Reject {reason = AddressSaturated,..} = amountToReject
getAdasToRejectBecauseAdresseSaturated SendOnExchangeAddressWithPartialReject {..} = amountToReject
getAdasToRejectBecauseAdresseSaturated _  =  0


getAdasSendOnExchange :: Command -> Ada
getAdasSendOnExchange SendOnExchangeAddress {..} = adasToSendOnExchange
getAdasSendOnExchange SendOnExchangeAddressWithPartialReject {..} = adasToSendOnExchange
getAdasSendOnExchange _  =  0

getAdasToRejectBecauseOutOfRange :: Command -> Ada
getAdasToRejectBecauseOutOfRange Reject {reason = TransactionOutofRoundTimeRange,..} = amountToReject
getAdasToRejectBecauseOutOfRange _  =  0

getAdas:: Command -> Ada    
getAdas Reject {..} = amountToReject
getAdas SendOnExchangeAddressWithPartialReject {..} = adasToSendOnExchange + amountToReject
getAdas SendOnExchangeAddress  {..} = adasToSendOnExchange
