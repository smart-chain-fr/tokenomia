{-# LANGUAGE DerivingStrategies             #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving     #-}
{-# LANGUAGE StandaloneDeriving             #-}
{-# OPTIONS_GHC -Wno-orphans                #-}

module Tokenomia.Common.Arbitrary.AssetClass
    () where

import Data.String                          ( fromString )
import Plutus.V1.Ledger.Value
    ( AssetClass(..)
    , CurrencySymbol (..)
    , TokenName (..)
    , adaSymbol
    , adaToken
    , assetClass
    )

import Test.Tasty.QuickCheck
    ( Arbitrary
    , arbitrary
    , frequency
    , resize
    , shrink
    )

import Tokenomia.Common.AssetClass          ( adaAssetClass )
import Tokenomia.Common.Arbitrary.Builtins  ( vectorOfHexBytes )
import Tokenomia.Common.Arbitrary.Modifiers ( Restricted(..) )


instance Arbitrary CurrencySymbol where
    arbitrary = CurrencySymbol <$> arbitrary
    shrink x  = CurrencySymbol <$> shrink (unCurrencySymbol x)

instance Arbitrary TokenName where
    arbitrary = TokenName <$> arbitrary
    shrink x  = TokenName <$> shrink (unTokenName x)

instance Arbitrary AssetClass where
    arbitrary = AssetClass <$> arbitrary
    shrink x  = AssetClass <$> shrink (unAssetClass x)

instance Arbitrary (Restricted CurrencySymbol) where
    arbitrary = frequency
        [ (3, pure $ Restricted adaSymbol)
        , (1, Restricted . fromString <$> vectorOfHexBytes 28)
        ]
    shrink (Restricted x)
        | x == adaSymbol = []
        | otherwise = [Restricted adaSymbol]

instance Arbitrary (Restricted TokenName) where
    arbitrary = Restricted . TokenName <$> resize 32 arbitrary
    shrink x  = Restricted . TokenName <$> shrink (unTokenName . getRestricted $ x)

instance Arbitrary (Restricted AssetClass) where
    arbitrary =
        do
            Restricted currencySymbol <- arbitrary
            Restricted tokenName <- arbitrary
            pure . Restricted $
                if currencySymbol == adaSymbol
                    then assetClass adaSymbol adaToken
                    else assetClass currencySymbol tokenName
    shrink (Restricted (AssetClass (currencySymbol, tokenName)))
        | currencySymbol == adaSymbol = []
        | otherwise =
            let shrinkedTokenNames = getRestricted <$> shrink (Restricted tokenName)
                shrinks = adaAssetClass : (assetClass currencySymbol <$> shrinkedTokenNames)
            in
                Restricted <$> shrinks

deriving newtype instance Ord (Restricted CurrencySymbol)
deriving newtype instance Ord (Restricted TokenName)
deriving newtype instance Ord (Restricted AssetClass)
