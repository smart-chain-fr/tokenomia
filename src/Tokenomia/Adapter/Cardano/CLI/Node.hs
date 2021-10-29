{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}


module Tokenomia.Adapter.Cardano.CLI.Node
    ( getCurrentSlotSynced
    ) where


import Control.Monad.Reader ( MonadReader, MonadIO(..), asks )

import Cardano.Api
import Ledger 
import Tokenomia.Adapter.Cardano.CLI.Environment
    

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
