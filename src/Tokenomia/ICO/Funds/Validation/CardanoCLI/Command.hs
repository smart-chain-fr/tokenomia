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
    = SendOnExchangeAddressAndPartiallyRefund  
        { source :: WalletUTxO  
        , exchangeAddress :: Address 
        , refundAddress :: Address
        , adasToSendOnExchange :: Ada 
        , datum :: FilePath 
        , adasToRefund :: Ada
        , receivedAt :: Slot}
    | SendOnExchangeAddressAndPartiallyMoveToNextRound  
        { source :: WalletUTxO  
        , exchangeAddress :: Address
        , nextRoundExchangeAddress :: Address 
        , adasToSendOnExchange :: Ada
        , datum :: FilePath 
        , adasToMove :: Ada
        , receivedAt :: Slot}
    | Refund  
        { source :: WalletUTxO  
        , refundAddress :: Address 
        , adasToRefund :: Ada
        , receivedAt :: Slot}
    | MoveToNextRound  
        { source :: WalletUTxO
        , nextRoundExchangeAddress :: Address   
        , datum :: FilePath 
        , adasToMove :: Ada
        , receivedAt :: Slot}
    | SendOnExchangeAddress          
        { source :: WalletUTxO 
        , exchangeAddress :: Address
        , adasToSendOnExchange :: Ada 
        , datum :: FilePath
        , receivedAt :: Slot} deriving (Eq)

instance Ord Command where 
    compare x y = case compare ((childAddressRef . source) x) ((childAddressRef . source) y) of 
      LT -> LT
      EQ -> case compare (receivedAt x) (receivedAt y) of 
                LT -> LT
                EQ -> compare (source x) (source y) 
                GT -> GT 
      GT -> GT
        


instance AdaBalanceable Command where 
    adaBalance Refund {..} = getAdas source - adasToRefund
    adaBalance MoveToNextRound {..} = getAdas source - adasToMove
    adaBalance SendOnExchangeAddressAndPartiallyRefund {..}  = getAdas source - adasToSendOnExchange - adasToRefund
    adaBalance SendOnExchangeAddressAndPartiallyMoveToNextRound {..}  = getAdas source - adasToSendOnExchange - adasToMove
    adaBalance SendOnExchangeAddress {..}  = getAdas source - adasToSendOnExchange


instance Show Command where
    show Refund { ..}
        =  "\n Command : Refund "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source  : " <> show (getAdas source)
        <> "\n   | amount  : " <> show adasToRefund
    show MoveToNextRound { ..}
        =  "\n Command : MoveToNextRound "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source  : " <> show (getAdas source)
        <> "\n   | amount  : " <> show adasToMove
    show SendOnExchangeAddressAndPartiallyRefund { ..}
        =  "\n Command : SendOnExchangeAddressAndPartiallyRefund "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source  : "     <> show (getAdas source)
        <> "\n   | adasToRefund  : "     <> show adasToRefund
        <> "\n   | adasToSendOnExchange   : " <> show adasToSendOnExchange
    show SendOnExchangeAddressAndPartiallyMoveToNextRound { ..}
        =  "\n Command : SendOnExchangeAddressAndPartiallyRefund "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source  : "     <> show (getAdas source)
        <> "\n   | adasToMove  : "     <> show adasToMove
        <> "\n   | adasToSendOnExchange   : " <> show adasToSendOnExchange
    show SendOnExchangeAddress {..}
        =  "\n Command : SendOnExchangeAddress "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source      : " <> show (getAdas source)
        <> "\n   | adasToSendOnExchange   : " <> show adasToSendOnExchange
        