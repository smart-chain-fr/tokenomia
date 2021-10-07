{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}

{- | It's a simple wrapper around the cardano-cli with Shh library. Waiting for the PAB to be available 
     on the Testnet and the Mainnet, Whe have a need for a complex sequences of transactions. 
     To be less error prone we have chosen this alternative over using bash scripts for example.
     It allows you to use a part of your Off chain codebase in Haskell basically. 
-}
module Tokenomia.Adapter.Cardano.CLI
    ( -- Write 
      Internal.run_tx
    , Internal.register_minting_script_file
    , Internal.get_monetary_policy_path
    , Internal.register_shelley_wallet
    , Internal.remove_shelley_wallet
    , Internal.restore_from_seed_phrase
      -- Read 
    , Internal.query_registered_wallets  
    , getUTxOs
    , Internal.query_tip
    , Internal.Wallet (..)
    , Internal.WalletAddress
    , Internal.Environment (..)) where


import           Control.Monad.Reader

import qualified Tokenomia.Adapter.Cardano.CLI.Internal as Internal
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import           Tokenomia.Adapter.Cardano.CLI.Serialise ( FromCLI(fromCLI) )


getUTxOs 
  :: ( MonadIO m 
     , MonadReader Internal.Environment m )
  => Internal.WalletAddress 
  -> m [UTxO]
getUTxOs  a = fromCLI <$> (Internal.query_utxo a)

