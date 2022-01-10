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
module Tokenomia.ICO.RoundSettings
    ( RoundSettings (..)
    , RoundAddresses (..)
    , RatePerLovelace (..)
    , getCollateral
    , getFees
    , getExchangeAddress
    , getTokenAddress
    , getRoundAddresses
    , getRoundSettings
    , notElemFromRoundAddreses ) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada
import           Plutus.V1.Ledger.Value
import           Ledger ( Slot(..) )
import           Plutus.V1.Ledger.Interval

import           Data.Text.Prettyprint.Doc (pretty)
import           Tokenomia.Wallet.LocalRepository

import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Common.Address
import           Control.Monad.Reader
import           Tokenomia.Common.Environment
import           Tokenomia.Wallet.ChildAddress.LocalRepository as ChildAddress


data RoundSettings
        = RoundSettings
          { timeRange :: Interval Slot
          , maximumAdaPerAddress :: Ada
          , minimumAdaPerFund :: Ada
          , wallet :: Wallet
          , exchangeTokenId :: AssetClass
          , tokenRatePerLovelace :: RatePerLovelace
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
    show RoundSettings { wallet = Wallet {name}, addresses = RoundAddresses {adaSink = Address adaSink,..},.. }
        =  "\n|| Round Settings ||"
        <> "\n | Wallet Used : "  <> name
        <> "\n | Time range  = "  <> (show . pretty) timeRange
        <> "\n | Fund range  = "  <> (show . pretty) (interval minimumAdaPerFund maximumAdaPerAddress)
        <> "\n | Exchange Token class = " <> show exchangeTokenId
        <> "\n | Exchange Rate (1 lovelace = x tokens) = " <> show tokenRatePerLovelace
        <> "\n | Token Address    = " <> show  tokens
        <> "\n | Exchange Address = " <> show  exchange
        <> "\n | Ada Sink Address = " <> adaSink
        

getTokenAddress :: RoundAddresses -> Address 
getTokenAddress RoundAddresses {tokens = IndexedAddress {..}} = address 

notElemFromRoundAddreses :: RoundAddresses -> Address -> Bool
notElemFromRoundAddreses r address = address `notElem` getRoundAddresses r

getRoundSettings 
    :: ( MonadIO m
       , MonadReader Environment m)
       => Wallet
       -> m RoundSettings 
getRoundSettings wallet@Wallet{name} = do 
    collateral <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef name 0)
    fees <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef name 3)
    exchange <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef name 4)
    tokens <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef name 5)

    let roundAddresses = RoundAddresses
                            { exchange = exchange
                            , collateral = collateral
                            , tokens = tokens
                            , adaSink = "addr_test1qq2ngz976wwu9gyrk2psujg06v9jjwdmmp6gfu4d2t94ppgwga7r7zdwclwsxru6p989myzeemqrplzjz7l8sug9tykq636c39" 
                            , fees }
        round = RoundSettings
                { maximumAdaPerAddress = adaOf 1000
                , minimumAdaPerFund = adaOf 3
                , timeRange = interval 40354960 44653861
                , exchangeTokenId = assetClass "a0b03e9b2bf7228f54e0f51e6bd34f6e949eedb8ecae84f984452fc4" "506f736569646f6e6973"
                , wallet = wallet
                , tokenRatePerLovelace = 2 / 1_000_000
                , addresses = roundAddresses }
    return round

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


