{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
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
    , Internal.register_shelley_wallet
    , Internal.remove_shelley_wallet
      -- Read 
    , Internal.query_registered_wallets  
    , getUTxOs
    , Internal.query_tip
    , Internal.Wallet (..)
    , Internal.WalletAddress) where


import qualified Tokenomia.Adapter.Cardano.CLI.Internal as Internal
import Tokenomia.Adapter.Cardano.CLI.UTxO 
import Tokenomia.Adapter.Cardano.CLI.Serialise ( FromCLI(fromCLI) )
import Control.Monad.IO.Class ( MonadIO(..) )


getUTxOs :: MonadIO m => Internal.WalletAddress -> m [UTxO]
getUTxOs  a = fromCLI <$> liftIO (Internal.query_utxo a)

