module Tokenomia.CardanoApi.Time
    ( nominalDiffTimeToRelativeTime
    , relativeTimeToNominalDiffTime
    ) where

import Cardano.Slotting.Time                ( SystemStart, fromRelativeTime, toRelativeTime )
import Data.Time.Clock                      ( NominalDiffTime )
import Data.Time.Clock.POSIX                ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Data.Composition                     ( (.:) )

import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime(..) )


-- | Convert a relative time to POSIX time
relativeTimeToNominalDiffTime :: SystemStart -> RelativeTime -> NominalDiffTime
relativeTimeToNominalDiffTime =
    utcTimeToPOSIXSeconds .: fromRelativeTime

-- | Convert a POSIX time to relative time
nominalDiffTimeToRelativeTime :: SystemStart -> NominalDiffTime -> RelativeTime
nominalDiffTimeToRelativeTime systemStart =
    toRelativeTime systemStart . posixSecondsToUTCTime
