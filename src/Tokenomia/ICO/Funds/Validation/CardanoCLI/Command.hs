{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Tokenomia.ICO.Funds.Validation.CardanoCLI.Command
    ( Command (..)) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada
import           Ledger ( Slot(..) )

import           Tokenomia.Wallet.UTxO

import           Tokenomia.Common.Address
import           Tokenomia.ICO.Balanceable

data Command
    = SendOnExchangeAddressWithPartialRefund  
        { source :: WalletUTxO  
        , adasToSendOnExchange :: Ada 
        , datum :: FilePath 
        , refundAddress :: Address 
        , adasToBeRefund :: Ada
        , receivedAt :: Slot}
    | Refund  
        { source :: WalletUTxO  
        , refundAddress :: Address 
        , adasToBeRefund :: Ada
        , receivedAt :: Slot}
    | SendOnExchangeAddress          
        { source :: WalletUTxO 
        , adasToSendOnExchange :: Ada 
        , datum :: FilePath
        , receivedAt :: Slot} deriving (Eq)

instance Ord Command where 
    compare x y = case compare (receivedAt x) (receivedAt y) of 
      LT -> LT
      EQ -> compare (source x) (source y) 
      GT -> GT


instance AdaBalanceable Command where 
    adaBalance Refund {..} = getAdas source - adasToBeRefund
    adaBalance SendOnExchangeAddressWithPartialRefund {..}  = getAdas source - adasToSendOnExchange - adasToBeRefund
    adaBalance SendOnExchangeAddress {..}  = getAdas source - adasToSendOnExchange


instance Show Command where
    show Refund { ..}
        =  "\n Command : Refund "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source  : " <> show (getAdas source)
        <> "\n   | refund  : " <> show adasToBeRefund
    show SendOnExchangeAddressWithPartialRefund { ..}
        =  "\n Command : SendOnExchangeAddressWithPartialRefund "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source  : "     <> show (getAdas source)
        <> "\n   | adasToBeRefund  : "     <> show adasToBeRefund
        <> "\n   | adasToSendOnExchange   : " <> show adasToSendOnExchange
        

    show SendOnExchangeAddress {..}
        =  "\n Command : SendOnExchangeAddress "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source      : " <> show (getAdas source)
        <> "\n   | adasToSendOnExchange   : " <> show adasToSendOnExchange
        