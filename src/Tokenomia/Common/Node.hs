{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Tokenomia.Common.Node
    ( getCurrentSlotSynced
    ) where

import Control.Monad.Reader ( MonadReader, MonadIO(..), asks )

import Cardano.Api
    ( SlotNo(SlotNo),
      getLocalChainTip,
      ChainTip(ChainTip, ChainTipAtGenesis) )
import Ledger ( Slot(Slot) )
import Tokenomia.Common.Environment
    ( Environment(localNodeConnectInfo) )

getCurrentSlotSynced
    :: ( MonadIO m
       , MonadReader Environment m )
    => m Slot
getCurrentSlotSynced = do
    nodeInfo <- asks localNodeConnectInfo
    liftIO (getLocalChainTip nodeInfo)
        >>= \case
            ChainTipAtGenesis -> error "Got ChainTipAtGenesis as a last Slot..."
            ChainTip (SlotNo slot) _ _ -> (return . Slot . fromIntegral ) slot
