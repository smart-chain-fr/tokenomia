{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Tokenomia.ICO.RoundSettings
    ( RoundSettings (..)
    , RoundAddresses (..)
    , getCollateral
    , getFees
    , getExchangeAddress
    , getRoundAddresses) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada

import           Ledger ( Slot(..) )
import           Plutus.V1.Ledger.Interval

import           Data.Text.Prettyprint.Doc (pretty)
import           Tokenomia.Wallet.LocalRepository

import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Common.Address


data RoundSettings
        = RoundSettings
          { timeRange :: Interval Slot
          , maximumAdaPerAdress :: Ada
          , fundRange :: Interval Ada
          , wallet :: Wallet
          , addresses :: RoundAddresses }

data RoundAddresses = RoundAddresses
        { exchange :: IndexedAddress
        , collateral :: IndexedAddress
        , fees :: IndexedAddress}


getExchangeAddress :: RoundAddresses -> Address
getExchangeAddress RoundAddresses {exchange = IndexedAddress {address = exchangeAddress}} = exchangeAddress

getFees :: RoundAddresses -> FeeAddressRef
getFees RoundAddresses {fees = IndexedAddress {..}} =  FeeAddressRef childAddressRef


getCollateral :: RoundAddresses -> CollateralAddressRef
getCollateral RoundAddresses {collateral = IndexedAddress {..}} =  CollateralAddressRef childAddressRef

getRoundAddresses :: RoundAddresses -> [Address] 
getRoundAddresses     
    RoundAddresses
    {  exchange = IndexedAddress {address = exchange}
    ,  collateral = IndexedAddress {address = collateral}
    ,  fees = IndexedAddress {address = fees}}
    = [exchange,fees,collateral]

instance Show RoundSettings where
    show RoundSettings { .. }
        =  "\n | time range  = " <> (show . pretty) timeRange
        <> "\n | fund range = "  <> (show . pretty) fundRange

