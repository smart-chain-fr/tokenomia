{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Tokenomia.Wallet.Collateral 
    ( createCollateral
    , getCollateral) where

import Control.Monad.Reader

import Shh
    ( load,
      ExecReference(SearchPath) )

import Tokenomia.Adapter.Cardano.CLI
import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Data.Text as T
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO 
import qualified Ledger.Ada as Ada


{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo"]

createCollateral :: (MonadIO m, MonadReader Environment m)  => m ()
createCollateral = do
    liftIO $ echo "Select the sender's wallet" 
    Wallet.select
        >>= \case 
            Nothing -> liftIO $ print "No Wallet Registered !"
            Just senderWallet@Wallet {paymentAddress = senderAddr,..} -> do
                getCollateral senderWallet
                    >>= \case
                        Just _ -> liftIO $ print "You already have a collateral UTxO containing 2 ADA !"
                        Nothing -> do
                            liftIO $ echo "> Select the utxo containing ADAs for fees :" 
                            Wallet.selectUTxO senderWallet
                                >>= \case 
                                    Nothing -> liftIO $ echo "Please, add a ADA to your wallet"
                                    Just utxoWithFees -> do
                                        liftIO $ echo "> Select the utxo in order to create the collateral (must contain ONLY ADA and at least 2)  :" 
                                        Wallet.selectUTxOFilterBy containingStrictlyADAs senderWallet 
                                            >>= \case  
                                                Nothing -> liftIO $ echo "UTxO containing ONLY Ada not found in your wallet."
                                                Just utxoWithAda  -> do
                                                    run_tx paymentSigningKeyPath 
                                                            [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithAda
                                                            , "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees
                                                            , "--tx-out" , senderAddr <> " 2000000 lovelace"
                                                            , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithAda 
                                                            , "--change-address"  , senderAddr]


getCollateral
  :: ( MonadIO m
     , MonadReader Environment m )
     => Wallet
    -> m (Maybe UTxO)
getCollateral Wallet {..} =
    filter containsCollateral <$> getUTxOs paymentAddress
        >>= \case
            [] -> return Nothing
            (x:_)  -> (return . Just) x


containsCollateral :: UTxO -> Bool
containsCollateral UTxO {..}
   = Ada.lovelaceValueOf 2000000 == value