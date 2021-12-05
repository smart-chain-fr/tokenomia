{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Exchange.Command
    ( Command (..)
    , getTokensSum
    ) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada

import           Ledger ( Slot(..) )

import           Tokenomia.Common.Address
import           Tokenomia.Wallet.UTxO
import           Tokenomia.Common.Token
import           Data.Set.Ordered
import Tokenomia.ICO.Funds.Exchange.Balanceable

data Command
    = RefundBecauseTokensSoldOut
        { paybackAddress :: Address
        , source :: WalletUTxO
        , refundAmount :: Ada
        , receivedAt :: Slot}
    | ExchangeAndPartiallyRefund
        { paybackAddress :: Address
        , source :: WalletUTxO
        , collectedAmount :: Ada
        , refundAmount :: Ada
        , tokens :: Token
        , receivedAt :: Slot}
    | Exchange
        { paybackAddress :: Address
        , source :: WalletUTxO
        , collectedAmount :: Ada
        , tokens :: Token
        , receivedAt :: Slot} deriving Eq

instance Ord Command where
    compare x y = case compare (receivedAt x) (receivedAt y) of
      LT -> LT
      EQ -> compare (source x) (source y)
      GT -> GT

instance Show Command where
    show RefundBecauseTokensSoldOut { ..}
        =  "\n Command : RefundBecauseTokensSoldOut "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source  : " <> show (getAdas source)
        <> "\n   | refund  : " <> show refundAmount
    show ExchangeAndPartiallyRefund { ..}
        =  "\n Command : ExchangeAndPartiallyRefund "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source  : "     <> show (getAdas source)
        <> "\n   | refund  : "     <> show refundAmount
        <> "\n   | collected   : " <> show collectedAmount
        <> "\n   | token       : " <> show tokens

    show Exchange {..}
        =  "\n Command : Exchange "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source      : " <> show (getAdas source)
        <> "\n   | collected   : " <> show collectedAmount
        <> "\n   | token       : " <> show tokens

instance AdaBalanceable Command where 
    adaBalance RefundBecauseTokensSoldOut {..} = getAdas source - refundAmount
    adaBalance Exchange {tokens = Token {minimumAdaRequired},..}                    = getAdas source - collectedAmount - minimumAdaRequired
    adaBalance ExchangeAndPartiallyRefund {tokens = Token {minimumAdaRequired},..}  = getAdas source - collectedAmount - minimumAdaRequired - refundAmount

instance TokenBalanceable Command where 
    tokenBalance = getTokenAmount


getTokensSum ::  OSet Command -> Integer
getTokensSum xs = sum (getTokenAmount <$> toAscList xs)

getTokenAmount :: Command -> Integer    
getTokenAmount RefundBecauseTokensSoldOut {} = 0
getTokenAmount Exchange  {tokens = Token {..}} = amount
getTokenAmount ExchangeAndPartiallyRefund  {tokens = Token {..}} = amount