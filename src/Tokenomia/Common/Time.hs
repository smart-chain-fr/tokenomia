module Tokenomia.Common.Time
    ( slotAfterNextBeginRelativeTime
    , toNextBeginNominalDiffTime
    , toNextBeginRelativeTime
    ) where

import Data.Time.Clock                      ( NominalDiffTime )

import Control.Monad.Reader                 ( MonadIO(..) )
import Control.Monad.Trans.Except           ( ExceptT )
import Control.Monad.Trans.Except.Extra     ( secondExceptT )

import Cardano.Api                          ( SlotNo(..) )
import Cardano.Api.Shelley                  ( CardanoMode, LocalNodeConnectInfo )
import Cardano.Slotting.Time                ( SystemStart )

import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime(..) )

import Tokenomia.CardanoApi.Query           ( QueryFailure, querySlotToWallclock', queryWallclockToSlot' )
import Tokenomia.CardanoApi.Time            ( relativeTimeToNominalDiffTime, nominalDiffTimeToRelativeTime )


-- | Smallest slot whose starting time is greater or equal than the given time
slotAfterNextBeginRelativeTime ::
     ( MonadIO m )
    => RelativeTime
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m SlotNo
slotAfterNextBeginRelativeTime time localNodeConnectInfo =
    do
        n <- queryWallclockToSlot' time localNodeConnectInfo
        beginTime <- querySlotToWallclock' n localNodeConnectInfo
        pure $
            if time == beginTime
                then n
                else n + 1

-- | Smallest time starting a slot that is greater or equal than the given relative time
toNextBeginRelativeTime ::
     ( MonadIO m )
    => RelativeTime
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m RelativeTime
toNextBeginRelativeTime time localNodeConnectInfo =
    do
        n <- slotAfterNextBeginRelativeTime time localNodeConnectInfo
        querySlotToWallclock' n localNodeConnectInfo

-- | Smallest time starting a slot that is greater or equal than the given time
toNextBeginNominalDiffTime ::
     ( MonadIO m )
    => SystemStart
    -> NominalDiffTime
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m NominalDiffTime
toNextBeginNominalDiffTime systemStart time =
    secondExceptT (relativeTimeToNominalDiffTime systemStart) .
        toNextBeginRelativeTime (nominalDiffTimeToRelativeTime systemStart time)
