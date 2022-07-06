{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Tokenomia.Common.Node (
  getCurrentSlotSynced,
) where

import Control.Monad.Reader (MonadIO (..), MonadReader, asks)

import Cardano.Api
import Ledger
import Tokenomia.Common.Environment

getCurrentSlotSynced ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  m Slot
getCurrentSlotSynced = do
  nodeInfo <- asks localNodeConnectInfo
  liftIO (getLocalChainTip nodeInfo)
    >>= \case
      ChainTipAtGenesis -> error "Got ChainTipAtGenesis as a last Slot..."
      ChainTip (SlotNo slot) _ _ -> (return . Slot . fromIntegral) slot
