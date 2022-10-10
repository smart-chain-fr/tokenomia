module Tokenomia.CardanoApi.Arbitrary.Slot
    () where

import Cardano.Api
    ( SlotNo(..) )

import Test.Tasty.QuickCheck
    ( Arbitrary
    , arbitrary
    , genericShrink
    , shrink
    )


instance Arbitrary SlotNo where
    arbitrary = SlotNo <$> arbitrary
    shrink = genericShrink
