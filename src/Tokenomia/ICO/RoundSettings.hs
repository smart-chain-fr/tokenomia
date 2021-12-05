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
module Tokenomia.ICO.RoundSettings
    ( RoundSettings (..)
    , RoundAddresses (..)
    , RatePerAda (..)
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
          , tokenRate :: RatePerAda
          , addresses :: RoundAddresses}

newtype RatePerAda = RatePerAda Integer deriving (Real,Integral,Num,Enum,Eq,Ord,Show)



data RoundAddresses = RoundAddresses
        { exchange :: IndexedAddress
        , tokens :: IndexedAddress
        , collateral :: IndexedAddress
        , fees :: IndexedAddress
        , adaSink :: Address}

instance Show RoundSettings where
    show RoundSettings { wallet = Wallet {name}, addresses = RoundAddresses {..},.. }
        =  "\n --------------------------------------"
        <> "\n|| Round Settings ||"
        <> "\n | Wallet : " <> name
        <> "\n | time range  = " <> (show . pretty) timeRange
        <> "\n | fund range = "  <> (show . pretty) (interval minimumAdaPerFund maximumAdaPerAddress)
        <> "\n | exchange Token class = " <> show exchangeTokenId
        <> "\n | Token Address" <> show  tokens
        <> "\n | Exchange Address" <> show  exchange
        <> "\n | Ada Sink Address" <> show  adaSink
        <> "\n --------------------------------------"

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
    fees <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef name 1)
    exchange <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef name 2)
    tokens <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef name 3)

    let roundAddresses = RoundAddresses
                            { exchange = exchange
                            , collateral = collateral
                            , tokens = tokens
                            , adaSink = "addr_test1qpltr6tke97fnq5j70c7vc6tul6styqu6egcq20nsn3n22swga7r7zdwclwsxru6p989myzeemqrplzjz7l8sug9tykqnxgmj0" 
                            , fees }
        round = RoundSettings
                { maximumAdaPerAddress = adaOf 1000
                , minimumAdaPerFund = adaOf 3
                , timeRange = interval 40354960 44653861
                , exchangeTokenId = assetClass "a0b03e9b2bf7228f54e0f51e6bd34f6e949eedb8ecae84f984452fc4" "506f736569646f6e6973"
                , wallet = wallet
                , tokenRate = 12
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


