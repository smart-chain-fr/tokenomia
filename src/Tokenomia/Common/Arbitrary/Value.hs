{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE FlexibleInstances            #-}
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

import Test.Tasty.QuickCheck
    ( Arbitrary
    , Gen
    , arbitrary
    , getPositive
    )

import Tokenomia.Common.Arbitrary.Modifiers  ( Restricted(..) )
import Tokenomia.Common.Arbitrary.AssetClass ()


instance Arbitrary (Restricted Value) where
    arbitrary =
            Restricted . mconcat
                <$> uncurry assetClassValue <$$> gen
      where
        gen :: Gen [(AssetClass, Integer)]
        gen =
            zip
                <$> (getRestricted <$$> Set.toList <$> arbitrary)
                <*> (getPositive <$$> arbitrary)
