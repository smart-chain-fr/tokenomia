module Tokenomia.Common.Arbitrary.POSIXTime
    (
    ) where

import Cardano.Node.Emulator.TimeSlot                  ( SlotConfig(..) )
import Data.Default                                    ( def )
import Ledger                                          ( POSIXTime(..) )

import Test.Tasty.QuickCheck
    ( Arbitrary
    , arbitrary
    , genericShrink
    , getPositive
    , scale
    , shrink
    )


--
-- [Note: Generator scale]
--
-- As POSIXTime will be mostly converted to slot, it is interesting to scale the
-- generator in order to have different slots. With the default integer scale,
-- all generated POSIXTime would be enclosed by the same slot.
--

instance Arbitrary POSIXTime where
    arbitrary =
        let slotLength = fromInteger $ scSlotLength def
        in
            POSIXTime . getPositive <$> scale (* slotLength) arbitrary
    shrink = filter (>0) . genericShrink
