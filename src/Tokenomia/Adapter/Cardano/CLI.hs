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


module Tokenomia.Adapter.Cardano.CLI
    ( -- Write 
      Internal.submitTx
    , Internal.registerMintingScriptFile
    , Internal.registerValidatorScriptFile
    , Internal.registerVestingIndex
    , Internal.getMonetaryPolicyPath
    , Internal.register_shelley_wallet
    , Internal.remove_shelley_wallet
    , Internal.restore_from_seed_phrase
      -- Read 
    , Internal.query_registered_wallets
    , Internal.getVestingIndex
    , Internal.getScriptLocation
    , Internal.ScriptLocation (..)
    , getUTxOs
    , Internal.getCurrentSlotSynced
    , Internal.Wallet (..)
    , Internal.Address
    , Internal.getTestnetEnvironmment
    , Internal.getDataHash
    , Internal.persistDataInTMP
    , Internal.Environment (..)
    ) where


import           Control.Monad.Reader ( MonadIO, MonadReader )

import qualified Tokenomia.Adapter.Cardano.CLI.Internal as Internal
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Adapter.Cardano.CLI.Serialise ( FromCLI(fromCLI) )



getUTxOs
  :: ( MonadIO m
     , MonadReader Internal.Environment m )
  => Internal.Address
  -> m [UTxO]
getUTxOs  a = fromCLI <$> Internal.query_utxo a

