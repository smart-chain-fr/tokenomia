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
import           Tokenomia.Wallet.WalletUTxO
import           Tokenomia.Common.Token
import           Data.Set.Ordered
import Tokenomia.ICO.Balanceable
import Tokenomia.Wallet.ChildAddress.ChildAddressRef 

data Command
    = RejectBecauseTokensSoldOut
        { index :: ChildAddressIndex
        , paybackAddress :: Address
        , source :: WalletUTxO
        , rejectAmount :: Ada
        , receivedAt :: Slot}
    | ExchangeAndPartiallyReject
        { index :: ChildAddressIndex
        , paybackAddress :: Address
        , source :: WalletUTxO
        , collectedAmount :: Ada
        , rejectAmount :: Ada
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
    show RejectBecauseTokensSoldOut { ..}
        =  "\n Command : RejectBecauseTokensSoldOut "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source      : " <> show (getAdas source)
        <> "\n   | reject      : " <> show rejectAmount
    show ExchangeAndPartiallyReject { ..}
        =  "\n Command : ExchangeAndPartiallyReject "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source      : " <> show (getAdas source)
        <> "\n   | reject      : " <> show rejectAmount
        <> "\n   | collected   : " <> show collectedAmount
        <> "\n   | token       : " <> show tokens

    show Exchange {..}
        =  "\n Command : Exchange "
        <> "\n   | received at : " <> show (getSlot receivedAt)
        <> "\n   | source      : " <> show (getAdas source)
        <> "\n   | collected   : " <> show collectedAmount
        <> "\n   | token       : " <> show tokens

instance AdaBalanceable Command where 
    adaBalance RejectBecauseTokensSoldOut {..} = getAdas source - rejectAmount
    adaBalance Exchange {tokens = Token {minimumAdaRequired},..}                    = getAdas source - collectedAmount - minimumAdaRequired
    adaBalance ExchangeAndPartiallyReject {tokens = Token {minimumAdaRequired},..}  = getAdas source - collectedAmount - minimumAdaRequired - rejectAmount

instance TokenBalanceable Command where 
    tokenBalance = getTokenAmount


getTokensSum ::  OSet Command -> Integer
getTokensSum xs = sum (getTokenAmount <$> toAscList xs)

getTokenAmount :: Command -> Integer    
getTokenAmount RejectBecauseTokensSoldOut {} = 0
getTokenAmount Exchange  {tokens = Token {..}} = amount
getTokenAmount ExchangeAndPartiallyReject  {tokens = Token {..}} = amount