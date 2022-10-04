{-# LANGUAGE DerivingStrategies             #-}
{-# LANGUAGE TypeFamilies                   #-}

module Tokenomia.CardanoApi.Query
    ( QueryFailure(..)
    , queryCurrentEra
    , queryGenesisParameters
    , queryNominalDiffTimeToSlot
    , querySlotToNominalDiffTime
    , querySlotToWallclock
    , querySlotToWallclock'
    , querySystemStart
    , queryUTxOByAddress
    , queryUTxOByTxIn
    , queryWallclockToSlot
    , queryWallclockToSlot'
    ) where

import Control.Monad                        ( join )
import Control.Monad.Reader                 ( MonadIO(..) )
import Control.Monad.Trans.Except           ( ExceptT )
import Control.Monad.Trans.Except.Extra     ( newExceptT, firstExceptT, secondExceptT )
import Control.Lens                         ( (^.), _1 )

import Data.Bifunctor                       ( first )
import Data.Composition                     ( (.:) )
import Data.Set                             ( Set )
import Data.Time.Clock                      ( NominalDiffTime )

import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime, SlotLength, SystemStart )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( EraMismatch )
import Ouroboros.Consensus.HardFork.History.Qry
    ( PastHorizonException
    , Qry
    , interpretQuery
    , slotToWallclock
    , wallclockToSlot
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure )

import Cardano.Api.Shelley
    ( AddressAny
    , AnyCardanoEra
    , CardanoMode
    , ConsensusModeIsMultiEra(CardanoModeIsMultiEra)
    , EraHistory(..)
    , EraInMode(ShelleyEraInCardanoMode)
    , GenesisParameters
    , LocalNodeConnectInfo
    , QueryInEra(QueryInShelleyBasedEra)
    , QueryInMode(QueryEraHistory, QueryInEra, QueryCurrentEra, QuerySystemStart)
    , QueryInShelleyBasedEra(QueryGenesisParameters, QueryUTxO)
    , QueryUTxOFilter(QueryUTxOByTxIn, QueryUTxOByAddress)
    , ShelleyBasedEra(ShelleyBasedEraShelley)
    , ShelleyEra
    , SlotNo
    , TxIn
    , UTxO
    , executeLocalStateQueryExpr
    , queryExpr
    )

import Tokenomia.CardanoApi.Time            ( nominalDiffTimeToRelativeTime, relativeTimeToNominalDiffTime )


data    QueryFailure
    =   QueryNetworkFailure AcquireFailure
    |   QueryHistoryFailure PastHorizonException
    |   QueryEraMismatch EraMismatch
    deriving stock ( Show )

-- | Return a `Left` value converted into an `ExceptT QueryFailure`
newExceptTQueryFailure ::
     ( MonadIO m )
    => (e -> QueryFailure)
    -> Either e a
    -> ExceptT QueryFailure m a
newExceptTQueryFailure =
    newExceptT . pure .: first

-- | Join the result of an `executeQueryExpr` that is returning an `Either`
joinQueryFailures ::
     ( MonadIO m )
    => (e -> QueryFailure)
    -> ExceptT QueryFailure m (Either e a)
    -> ExceptT QueryFailure m a
joinQueryFailures =
    join .: secondExceptT . newExceptTQueryFailure

-- | Execute simple query expressions
executeQueryExpr ::
     ( MonadIO m )
    => QueryInMode mode a
    -> LocalNodeConnectInfo mode
    -> ExceptT QueryFailure m a
executeQueryExpr query info =
    firstExceptT QueryNetworkFailure . newExceptT <$> liftIO $
        executeLocalStateQueryExpr info Nothing (\_ -> queryExpr query)

-- | Execute query expressions in shelley based era
executeQueryExprInShelleyBasedEra ::
     ( MonadIO m )
    => QueryInShelleyBasedEra ShelleyEra a
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m a
executeQueryExprInShelleyBasedEra =
    joinQueryFailures QueryEraMismatch .:
        executeQueryExpr .
            QueryInEra ShelleyEraInCardanoMode .
                QueryInShelleyBasedEra ShelleyBasedEraShelley

-- | Query the era of the tip
queryCurrentEra ::
     ( MonadIO m )
    => LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m AnyCardanoEra
queryCurrentEra = executeQueryExpr $ QueryCurrentEra CardanoModeIsMultiEra

-- | Query the system start
querySystemStart ::
     ( MonadIO m )
    => LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m SystemStart
querySystemStart = executeQueryExpr QuerySystemStart

-- | Query the history needed to interpret hardfork query
queryEraHistory ::
     ( MonadIO m )
    => LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m (EraHistory CardanoMode)
queryEraHistory = executeQueryExpr $ QueryEraHistory CardanoModeIsMultiEra

-- | Interpret a query with respect to the hardfork history
interpretQueryHistory ::
     ( MonadIO m )
    => Qry a
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m a
interpretQueryHistory query info =
    do
        EraHistory _ interpreter <- queryEraHistory info
        newExceptTQueryFailure QueryHistoryFailure $
            interpretQuery interpreter query

-- | Convert a time to its enclosing slot, with time spent and time left in this slot
queryWallclockToSlot ::
     ( MonadIO m )
    => RelativeTime
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m (SlotNo, NominalDiffTime, NominalDiffTime)
queryWallclockToSlot =
    interpretQueryHistory . wallclockToSlot

-- | Convert a relative time to its enclosing slot
queryWallclockToSlot' ::
     ( MonadIO m )
    => RelativeTime
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m SlotNo
queryWallclockToSlot' =
    secondExceptT (^._1) .: queryWallclockToSlot

-- | Convert a POSIXTime to its enclosing slot
queryNominalDiffTimeToSlot ::
     ( MonadIO m )
    => SystemStart
    -> NominalDiffTime
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m SlotNo
queryNominalDiffTimeToSlot =
    queryWallclockToSlot' .: nominalDiffTimeToRelativeTime

-- | Convert a slot to its begin time, with the slot length
querySlotToWallclock ::
     ( MonadIO m )
    => SlotNo
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m (RelativeTime, SlotLength)
querySlotToWallclock =
    interpretQueryHistory . slotToWallclock

-- | Convert a slot to its begin relative time
querySlotToWallclock' ::
     ( MonadIO m )
    => SlotNo
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m RelativeTime
querySlotToWallclock' =
    secondExceptT (^._1) .: querySlotToWallclock

-- | Convert a slot to its begin POSIXTime
querySlotToNominalDiffTime ::
     ( MonadIO m )
    => SystemStart
    -> SlotNo
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m NominalDiffTime
querySlotToNominalDiffTime systemStart =
    secondExceptT (relativeTimeToNominalDiffTime systemStart) .: querySlotToWallclock'

-- | Query UTxO filtered by addresses
queryUTxOByAddress ::
     ( MonadIO m )
    => Set AddressAny
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m (UTxO ShelleyEra)
queryUTxOByAddress =
    queryUTxO . QueryUTxOByAddress

-- | Query UTxO filtered by transaction inputs
queryUTxOByTxIn ::
     ( MonadIO m )
    => Set TxIn
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m (UTxO ShelleyEra)
queryUTxOByTxIn =
    queryUTxO . QueryUTxOByTxIn

-- | Query UTxO with a given filter
queryUTxO ::
     ( MonadIO m )
    => QueryUTxOFilter
    -> LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m (UTxO ShelleyEra)
queryUTxO =
    executeQueryExprInShelleyBasedEra . QueryUTxO

-- | Query the genesis parameters
queryGenesisParameters ::
     ( MonadIO m )
    => LocalNodeConnectInfo CardanoMode
    -> ExceptT QueryFailure m GenesisParameters
queryGenesisParameters =
    executeQueryExprInShelleyBasedEra QueryGenesisParameters
