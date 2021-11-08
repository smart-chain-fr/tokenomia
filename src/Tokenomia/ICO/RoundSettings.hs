{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
module Tokenomia.ICO.RoundSettings
    ( RoundSettings (..)) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada

import           Ledger ( Slot(..) )
import           Plutus.V1.Ledger.Interval

import           Data.Text.Prettyprint.Doc (pretty)
import           Tokenomia.Wallet.LocalRepository

import           Tokenomia.Wallet.ChildAddress.ChildAddressRef

data RoundSettings
        = RoundSettings
          { timeRange :: Interval Slot
          , maximumAdaPerAdress :: Ada
          , fundRange :: Interval Ada
          , wallet :: Wallet
          , exchangeAddressRef :: ChildAddressRef
          , collateralAddressRef :: ChildAddressRef}

instance Show RoundSettings where
    show RoundSettings { .. }
        =  "\n | time range  = " <> (show . pretty) timeRange
        <> "\n | fund range = "  <> (show . pretty) fundRange

