{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE TypeApplications             #-}
{-# OPTIONS_GHC -Wno-orphans              #-}

module Tokenomia.Common.Arbitrary.Value
    ( Restricted(..)
    ) where

import Data.Functor.Syntax          ( (<$$>) )
import Data.Set qualified as Set    ( toList )

import Plutus.V1.Ledger.Value
    ( AssetClass(..)
    , Value(..)
    , assetClassValue
    )

import PlutusTx.AssocMap qualified as AssocMap
    ( Map
    , fromList
    , toList
    )

import Tokenomia.Common.MultiAsset
    ( MultiAsset(..)
    , FromValue(..)
    , ToValue(..)
    )

import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty.QuickCheck
    ( Arbitrary
    , Gen
    , arbitrary
    , getPositive
    , shrink
    )

import Tokenomia.Common.Arbitrary.Modifiers  ( Restricted(..) )
import Tokenomia.Common.Arbitrary.AssetClass ()
import Tokenomia.Common.Arbitrary.MultiAsset ()


instance (Arbitrary k, Arbitrary v) => Arbitrary (AssocMap.Map k v) where
    arbitrary = AssocMap.fromList <$> arbitrary
    shrink x  = AssocMap.fromList <$> shrink (AssocMap.toList x)

instance Arbitrary Value where
    arbitrary = Value <$> arbitrary
    shrink x  = Value <$> shrink (getValue x)

instance Arbitrary (Restricted Value) where
    arbitrary =
            Restricted . mconcat
        <$> uncurry assetClassValue <$$> gen
      where
        gen :: Gen [(AssetClass, Integer)]
        gen =
            zip
                <$> (Set.toList  <$>  arbitrary)
                <*> (getPositive <$$> arbitrary)
    shrink x  =
            Restricted . toValue
        <$> shrink (fromValue @MultiAsset $ getRestricted x)
