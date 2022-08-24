module Tokenomia.Common.Arbitrary.Slot
    () where

import Ledger
    ( Slot(..) )

import Test.Tasty.QuickCheck
    ( Arbitrary
    , arbitrary
    , genericShrink
    , getNonNegative
    , shrink
    )


instance Arbitrary Slot where
    arbitrary = Slot . getNonNegative <$> arbitrary
    shrink = filter (>=0) . genericShrink
