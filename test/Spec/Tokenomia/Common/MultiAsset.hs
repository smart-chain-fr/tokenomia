{-# LANGUAGE TypeApplications             #-}

module Spec.Tokenomia.Common.MultiAsset
    ( tests
    ) where

--
-- This module tests the `MultiAsset` wrapper.
--

import Test.Tasty               ( TestTree, testGroup )
import Test.Tasty.QuickCheck    ( testProperty )

import Tokenomia.Common.MultiAsset
    ( MultiAsset(..)
    , FromValue(..)
    , ToValue(..)
    )

import Tokenomia.Common.Arbitrary.Modifiers   ( Restricted(..) )
import Tokenomia.Common.Arbitrary.MultiAsset  ()
import Tokenomia.Common.Arbitrary.Value       ()


tests :: TestTree
tests = testGroup "Common.MultiAsset" [ properties ]

properties :: TestTree
properties = testGroup "Properties"
    [ testGroup "MultiAsset"
        [ testGroup "Bijection Laws"
            [ testProperty "fromValue . toValue == id"
                (\restricted ->
                    let x = getRestricted restricted
                    in (fromValue . toValue @MultiAsset $ x) == x
                )
            , testProperty "toValue . fromValue == id"
                (\restricted ->
                    let x = getRestricted restricted
                    in (toValue @MultiAsset . fromValue $ x) == x
                )
            ]
        ]
    ]
