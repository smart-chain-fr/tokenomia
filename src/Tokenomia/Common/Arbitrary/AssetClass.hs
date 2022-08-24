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
    , CoArbitrary
    , Function
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

instance CoArbitrary CurrencySymbol
instance CoArbitrary TokenName
instance CoArbitrary AssetClass

instance Function CurrencySymbol
instance Function TokenName
instance Function AssetClass
