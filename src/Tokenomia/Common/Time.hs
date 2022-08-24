module Tokenomia.Common.Time
    ( posixTimeToEnclosingSlotNo
    , slotAfterNextBeginPOSIXTime
    , toCardanoSlotNo
    , toNextBeginPOSIXTime
    ) where

import Cardano.Api                          ( SlotNo(..) )
import Data.Default                         ( def )
import Ledger                               ( POSIXTime, Slot (..) )
import Ledger.TimeSlot                      ( posixTimeToEnclosingSlot, slotToBeginPOSIXTime )


-- | Convert from Plutus to Cardano slot representation
toCardanoSlotNo :: Slot -> SlotNo
toCardanoSlotNo = SlotNo . fromInteger . getSlot

-- | POSIXTime to enclosing SlotNo
posixTimeToEnclosingSlotNo :: POSIXTime -> SlotNo
posixTimeToEnclosingSlotNo = toCardanoSlotNo . posixTimeToEnclosingSlot def

-- | Smaller slot whose starting POSIXTime is greater or equal to the given time
slotAfterNextBeginPOSIXTime :: POSIXTime -> Slot
slotAfterNextBeginPOSIXTime time =
    let n = posixTimeToEnclosingSlot def time
    in
        if time == slotToBeginPOSIXTime def n
            then n
            else n + 1

-- | Smaller POSIXTime starting a slot that is greater or equal to the given time
toNextBeginPOSIXTime :: POSIXTime -> POSIXTime
toNextBeginPOSIXTime = slotToBeginPOSIXTime def . slotAfterNextBeginPOSIXTime
