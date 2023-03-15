module Tokenomia.CardanoApi.FromPlutus.Time
    ( fromPlutusSlot
    , nominalDiffTimeToPosixTime
    , posixTimeToNominalDiffTime
    , posixTimeToRelativeTime
    , relativeTimeToPosixTime
    ) where

import Data.Composition                                ( (.:) )
import Data.Time.Clock                                 ( NominalDiffTime, secondsToNominalDiffTime )

import Ledger                                          ( POSIXTime(..), Slot(..) )

import Cardano.Api                                     ( SlotNo(..) )
import Cardano.Slotting.Time                           ( SystemStart )

import Ouroboros.Consensus.BlockchainTime.WallClock.Types ( RelativeTime(..) )

import Tokenomia.CardanoApi.Time                       ( nominalDiffTimeToRelativeTime, relativeTimeToNominalDiffTime )


-- | Convert from Plutus to Cardano slot representation
fromPlutusSlot :: Slot -> SlotNo
fromPlutusSlot = SlotNo . fromInteger . getSlot

-- | Convert from Plutus to Cardano POSIXTime
posixTimeToNominalDiffTime :: POSIXTime -> NominalDiffTime
posixTimeToNominalDiffTime =
    secondsToNominalDiffTime . fromIntegral . (`div` 1000) . getPOSIXTime

-- | Convert from Cardano to Plutus POSIXTime
nominalDiffTimeToPosixTime :: NominalDiffTime -> POSIXTime
nominalDiffTimeToPosixTime =
    POSIXTime . truncate . (* 1000)

-- | Convert from Plutus POSIXTime to RelativeTime
posixTimeToRelativeTime :: SystemStart -> POSIXTime -> RelativeTime
posixTimeToRelativeTime systemStart =
    nominalDiffTimeToRelativeTime systemStart . posixTimeToNominalDiffTime

-- | Convert from RelativeTime to Plutus POSIXTime
relativeTimeToPosixTime :: SystemStart -> RelativeTime -> POSIXTime
relativeTimeToPosixTime =
    nominalDiffTimeToPosixTime .: relativeTimeToNominalDiffTime
