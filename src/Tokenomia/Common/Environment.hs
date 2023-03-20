{-# LANGUAGE DerivingStrategies                        #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE ScopedTypeVariables                       #-}

module Tokenomia.Common.Environment
    ( Environment(..)
    , TokenomiaNetwork(..)
    , convertToExternalPosix
    , convertToInternalPosix
    , formatISO8601
    , getFirstShelleySlot
    , getFirstShelleySlotTime
    , getNetworkEnvironment
    , networkMagicNumber
    , readNetworkMagic
    , toPosixTime
    , toSlot
    ) where

import Control.Monad.Reader                            ( MonadIO(..), MonadReader(ask) )
import System.Environment                              ( getEnv )


import Cardano.Api
    ( CardanoMode
    , ConsensusModeParams(CardanoModeParams)
    , EpochSlots(EpochSlots)
    , LocalNodeConnectInfo(..)
    , NetworkId
    , NetworkMagic(NetworkMagic, unNetworkMagic)
    , QueryInMode(QuerySystemStart)
    , executeLocalStateQueryExpr
    , queryExpr
    , toNetworkMagic
    )

import Cardano.Api qualified                           ( NetworkId(Mainnet, Testnet) )
import Ledger                                          ( POSIXTime(POSIXTime), Slot(Slot) )
import Ouroboros.Consensus.BlockchainTime.WallClock.Types ( SystemStart(..) )

import Data.Coerce                                     ( coerce )
import Data.Time.Clock qualified as ExternalPosix
import Data.Time.Clock.POSIX qualified as ExternalPosix
import Data.Time.ISO8601 qualified as ExternalPosix


data TokenomiaNetwork =
        MainnetNetwork
    |   TestnetNetwork
    |   PreprodNetwork
    deriving stock (Show)

data Environment =
    Testnet
        { magicNumber :: Integer
        , localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
        , preShelleyEpochs :: Integer
        , byronSlotsPerEpoch :: Integer
        , byronSecondsPerSlot :: Integer
        , systemStart' :: SystemStart
        , systemStart :: ExternalPosix.POSIXTime }
    |  Mainnet
        { magicNumber :: Integer
        , localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
        , preShelleyEpochs :: Integer
        , byronSlotsPerEpoch :: Integer
        , byronSecondsPerSlot :: Integer
        , systemStart' :: SystemStart
        , systemStart :: ExternalPosix.POSIXTime }


networkMagicNumber :: TokenomiaNetwork -> Integer
networkMagicNumber = \case
    MainnetNetwork -> readNetworkMagic Cardano.Api.Mainnet
    TestnetNetwork -> 1097911063
    PreprodNetwork -> 1


getNetworkEnvironment :: MonadIO m => TokenomiaNetwork -> m Environment
getNetworkEnvironment = \case
    MainnetNetwork     -> getMainnetEnvironment
    TestnetNetwork     -> getTestnetEnvironment
    PreprodNetwork     -> getPreprodEnvironment


getMainnetEnvironment :: MonadIO m => m Environment
getMainnetEnvironment = do
    socketPath <- liftIO $ getEnv "CARDANO_NODE_SOCKET_PATH"
    let magicNumber = readNetworkMagic Cardano.Api.Mainnet
        localNodeConnectInfo = LocalNodeConnectInfo
            { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
            , localNodeNetworkId       = Cardano.Api.Mainnet
            , localNodeSocketPath      = socketPath
            }
        preShelleyEpochs = 208
        byronSlotsPerEpoch = 21600
        byronSecondsPerSlot = 20
    systemStart <- ExternalPosix.utcTimeToPOSIXSeconds . coerce <$> getSystemStart' localNodeConnectInfo
    systemStart' <- getSystemStart' localNodeConnectInfo

    return $ Mainnet {..}


getPreprodEnvironment :: MonadIO m => m Environment
getPreprodEnvironment = do
    socketPath <- liftIO $ getEnv "CARDANO_NODE_SOCKET_PATH"
    let magicNumber = networkMagicNumber PreprodNetwork
        localNodeConnectInfo = LocalNodeConnectInfo
            { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
            , localNodeNetworkId       = Cardano.Api.Testnet  (NetworkMagic (fromIntegral magicNumber))
            , localNodeSocketPath      = socketPath
            }
        preShelleyEpochs = 4
        byronSlotsPerEpoch = 21600
        byronSecondsPerSlot = 20
    systemStart <- ExternalPosix.utcTimeToPOSIXSeconds . coerce <$> getSystemStart' localNodeConnectInfo
    systemStart' <- getSystemStart' localNodeConnectInfo

    return $ Testnet {..}


getTestnetEnvironment :: MonadIO m => m Environment
getTestnetEnvironment = do
    socketPath <- liftIO $ getEnv "CARDANO_NODE_SOCKET_PATH"
    let magicNumber = networkMagicNumber TestnetNetwork
        localNodeConnectInfo = LocalNodeConnectInfo
            { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
            , localNodeNetworkId       = Cardano.Api.Testnet  (NetworkMagic (fromIntegral magicNumber))
            , localNodeSocketPath      = socketPath
            }
        preShelleyEpochs = 74
        byronSlotsPerEpoch = 21600
        byronSecondsPerSlot = 20
    systemStart <- ExternalPosix.utcTimeToPOSIXSeconds . coerce <$> getSystemStart' localNodeConnectInfo
    systemStart' <- getSystemStart' localNodeConnectInfo

    return $ Testnet {..}


readNetworkMagic :: NetworkId -> Integer
readNetworkMagic = read . show . unNetworkMagic . toNetworkMagic


-- N.H : This is not neccessary because the transactions are handling PosixTime directly and not Slot
-- as I was thinking initially... I won't delete the code from now, but it will be eventually...

getSystemStart' :: MonadIO m => LocalNodeConnectInfo mode -> m SystemStart
getSystemStart' localNodeConnectInfo = do
    liftIO $ executeLocalStateQueryExpr localNodeConnectInfo Nothing (\_ -> queryExpr QuerySystemStart)
        >>= \case
                Left x -> error $ show x
                Right systemStart -> return systemStart

toPosixTime :: MonadReader Environment m  => Slot -> m ExternalPosix.POSIXTime
toPosixTime slot = do
    environment <- ask
    shelleyDurationInS <- toShelleyDurationInS slot
    byronDurationInS <- getTotalByronDurationInS
    return $ systemStart environment + ExternalPosix.secondsToNominalDiffTime (fromIntegral (byronDurationInS + shelleyDurationInS))

toSlot :: MonadReader Environment m  => ExternalPosix.UTCTime  -> m Slot
toSlot timeGiven = do
    firstShelleyTime <- getFirstShelleySlotTime
    let posixTime = ExternalPosix.utcTimeToPOSIXSeconds timeGiven
    let timeSpentInShelley  = truncate $ posixTime - firstShelleyTime
    return $ Slot timeSpentInShelley

toShelleyDurationInS :: MonadReader Environment m  => Slot -> m Integer
toShelleyDurationInS slot = do
    firstShelleySlot <- getFirstShelleySlot
    return . coerce $ (slot - firstShelleySlot)

getFirstShelleySlot :: MonadReader Environment m  => m Slot
getFirstShelleySlot = do
    environment <- ask
    (return . Slot) (byronSlotsPerEpoch environment * preShelleyEpochs environment)

getTotalByronDurationInS :: MonadReader Environment m  => m Integer
getTotalByronDurationInS = do
    environment <- ask
    firstShelleySlot <- getFirstShelleySlot
    return . coerce $ firstShelleySlot * fromIntegral (byronSecondsPerSlot environment)

getFirstShelleySlotTime :: MonadReader Environment m  => m ExternalPosix.POSIXTime
getFirstShelleySlotTime = do
    environment <- ask
    byronDurationInS <- getTotalByronDurationInS
    return $ systemStart environment + ExternalPosix.secondsToNominalDiffTime (fromIntegral byronDurationInS)

convertToInternalPosix :: ExternalPosix.POSIXTime -> POSIXTime
convertToInternalPosix = POSIXTime . (* 1000) . truncate -- losing the milliseconds precision.

convertToExternalPosix :: POSIXTime -> ExternalPosix.POSIXTime
convertToExternalPosix p = ExternalPosix.secondsToNominalDiffTime (fromIntegral p / 1000.0)


formatISO8601 :: ExternalPosix.POSIXTime -> String
formatISO8601 = ExternalPosix.formatISO8601 . ExternalPosix.posixSecondsToUTCTime
