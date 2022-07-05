{-# OPTIONS_GHC -Wno-orphans              #-}

module Tokenomia.Common.Arbitrary.AssetClass
    () where

import Plutus.V1.Ledger.Value
    ( AssetClass(..)
    , CurrencySymbol (..)
    , TokenName (..)
    )

import Test.Tasty.QuickCheck
    ( Arbitrary
    , arbitrary
    , shrink
    )

import Tokenomia.Common.Arbitrary.Builtins ()


instance Arbitrary CurrencySymbol where
    arbitrary = CurrencySymbol <$> arbitrary
    shrink x  = CurrencySymbol <$> shrink (unCurrencySymbol x)

instance Arbitrary TokenName where
    arbitrary = TokenName <$> arbitrary
    shrink x  = TokenName <$> shrink (unTokenName x)

instance Arbitrary AssetClass where
    arbitrary = AssetClass <$> arbitrary
    shrink x  = AssetClass <$> shrink (unAssetClass x)
