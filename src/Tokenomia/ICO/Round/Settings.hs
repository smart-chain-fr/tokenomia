{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Tokenomia.ICO.Round.Settings
    ( RoundSettings (..)
    , RoundAddresses (..)
    , RatePerLovelace (..)
    , PreviousRound (..)
    , NextRound (..)
    , getCollateral
    , getFees
    , getExchangeAddress
    , getTokenAddress
    , getRoundAddresses
    , notElemFromRoundAddreses
    ) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada
import           Plutus.V1.Ledger.Value
import           Ledger ( Slot(..) )
import           Plutus.V1.Ledger.Interval

import           Data.Text.Prettyprint.Doc (pretty)
import           Tokenomia.Wallet.LocalRepository

import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Common.Address


newtype NextRound 
    = NextRound 
        { exchangeAddress :: Address}

instance Show NextRound where
    show NextRound { exchangeAddress = Address exchangeAddress}
        =  " Exchange Address = " <> exchangeAddress


newtype PreviousRound = PreviousRound { wallet :: Wallet}

instance Show PreviousRound where
    show PreviousRound { wallet = Wallet {name}}
        =  " Wallet Used : "  <> name

        

data RoundSettings
        = RoundSettings
          { timeRange :: Interval Slot
          , maximumAdaPerAddress :: Ada
          , minimumAdaPerFund :: Ada
          , investorsWallet :: Wallet
          , exchangeTokenId :: AssetClass
          , tokenRatePerLovelace :: RatePerLovelace
          , previousRoundMaybe :: Maybe PreviousRound
          , nextRoundMaybe :: Maybe NextRound
          , addresses :: RoundAddresses}

newtype RatePerLovelace 
        = RatePerLovelace Double deriving (Real,RealFrac,Fractional,Num,Enum,Eq,Ord)
                                 deriving (Show) via Double 

data RoundAddresses = RoundAddresses
        { exchange :: IndexedAddress
        , tokens :: IndexedAddress
        , collateral :: IndexedAddress
        , fees :: IndexedAddress
        , adaSink :: Address}

instance Show RoundSettings where
    show RoundSettings { investorsWallet = Wallet {name = investorsWallet}, addresses = RoundAddresses {adaSink = Address adaSink,..},.. }
        =  "\n|| Round Settings ||"
        <> "\n | Time range  = "  <> (show . pretty) timeRange
        <> "\n | Fund range  = "  <> (show . pretty) (interval minimumAdaPerFund maximumAdaPerAddress)
        <> "\n | Exchange Token class = " <> show exchangeTokenId
        <> "\n | Exchange Rate (1 lovelace = x tokens) = " <> show tokenRatePerLovelace
        <> "\n | Investors Wallet   = "  <> investorsWallet
        <> "\n | Token Address      = " <> show  tokens
        <> "\n | Exchange Address   = " <> show  exchange
        <> "\n | Collateral Address = " <> show  collateral
        <> "\n | Fees Address       = " <> show  fees
        <> "\n | Ada Sink Address   = " <> adaSink
        <> "\n | Previous Round     = " <> show previousRoundMaybe
        <> "\n | Next Round         = " <> show nextRoundMaybe
        

getTokenAddress :: RoundAddresses -> Address 
getTokenAddress RoundAddresses {tokens = IndexedAddress {..}} = address 

notElemFromRoundAddreses :: RoundAddresses -> Address -> Bool
notElemFromRoundAddreses r address = address `notElem` getRoundAddresses r


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
    ,  fees = IndexedAddress {address = fees}
    ,  tokens = IndexedAddress {address = tokens}}
    = [exchange,fees,collateral,tokens]


