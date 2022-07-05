{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE ImportQualifiedPost          #-}
{-# OPTIONS_GHC -Wno-orphans              #-}

module Tokenomia.Common.Arbitrary.MultiAsset
    ( Restricted(..)
    ) where

import Data.Functor.Syntax              ( (<$$>) )
import Data.Set        qualified as Set ( toList )
import Data.Map.Strict qualified as Map ( fromList )

import Ledger.Value
    ( AssetClass
    )

import Test.Tasty.QuickCheck
    ( Arbitrary
    , Gen
    , arbitrary
    , getPositive
    , shrink
    )

import Tokenomia.Common.MultiAsset
    ( MultiAsset(..)
    , MultiAssetFormat(..)
    )

import Tokenomia.Common.Arbitrary.Modifiers  ( Restricted(..) )
import Tokenomia.Common.Arbitrary.AssetClass ()


instance Arbitrary MultiAsset where
    arbitrary = MultiAsset <$> arbitrary
    shrink x  = MultiAsset <$> shrink (unMultiAsset x)

instance Arbitrary (Restricted MultiAsset) where
    arbitrary = Restricted . MultiAsset . Map.fromList <$> gen
      where
        gen :: Gen [(AssetClass, Integer)]
        gen =
            zip
                <$> (Set.toList  <$>  arbitrary)
                <*> (getPositive <$$> arbitrary)
    shrink x  = Restricted <$> shrink (getRestricted x)

instance Arbitrary MultiAssetFormat where
    arbitrary = MultiAssetFormat <$> arbitrary
    shrink x  = MultiAssetFormat <$> shrink (unMultiAssetFormat x)
