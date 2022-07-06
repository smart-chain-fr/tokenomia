{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Common.Blockfrost (projectFromEnv'') where

import Blockfrost.Client qualified as B

import Tokenomia.Common.Environment (
  Environment (Mainnet, Testnet),
 )
import Prelude hiding (head)

import Control.Monad.Reader (MonadIO (..), MonadReader, asks)

projectFromEnv'' ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  m B.Project
projectFromEnv'' = do
  environmentPath <-
    asks
      ( \case
          Testnet {} -> "BLOCKFROST_TOKEN_TESTNET_PATH"
          Mainnet {} -> "BLOCKFROST_TOKEN_MAINNET_PATH"
      )
  liftIO $ B.projectFromEnv' environmentPath
