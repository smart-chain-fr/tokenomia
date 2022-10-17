module Tokenomia.CardanoApi.Arbitrary.Slot
    () where

import Cardano.Api
    ( SlotNo(..) )

import Test.Tasty.QuickCheck
    ( Arbitrary
    , Small(..)
    , arbitrary
    , genericShrink
    , resize
    , shrink
    )


instance Arbitrary SlotNo where
    arbitrary = SlotNo . getSmall <$> resize 128 arbitrary
    shrink = genericShrink
