{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Tokenomia.ICO.Funds.Reception.CardanoCLI.Command
    ( Command (..)) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada
import           Ledger ( Slot(..) )

import           Tokenomia.Wallet.UTxO

import           Tokenomia.Common.Address

data Command
    = TransferAndPartiallyRefund  
        { source :: WalletUTxO  
        , adas :: Ada 
        , datum :: FilePath 
        , refundAddress :: Address 
        , adasToBeRefund :: Ada
        , receivedAt :: Slot}
    | Refund  
        { source :: WalletUTxO  
        , refundAddress :: Address 
        , adasToBeRefund :: Ada
        , receivedAt :: Slot}
    | Transfer          
        { source :: WalletUTxO 
        , adas :: Ada 
        , datum :: FilePath
        , receivedAt :: Slot} deriving (Eq)

instance Ord Command where 
    compare x y = case compare (receivedAt x) (receivedAt y) of 
      LT -> LT
      EQ -> compare (source x) (source y) 
      GT -> GT


instance Show Command where
    show TransferAndPartiallyRefund {..} 
        =  show (getSlot receivedAt) 
            <> " - TransferAndPartiallyRefund : "
            <> show source 
    show Refund {..} 
        =  show (getSlot receivedAt) 
            <> " - Refund   : "
            <> show source 
    show Transfer {..} 
        =  show (getSlot receivedAt) 
            <> " - Transfer : "
            <> show source 
    