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
