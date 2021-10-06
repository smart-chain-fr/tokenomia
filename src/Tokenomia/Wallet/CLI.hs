{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Tokenomia.Wallet.CLI
  ( select
  , selectUTxO
  , selectUTxOFilterBy
  , createAndRegister
  , recover
  , list
  , remove)
  where

import Control.Monad.Catch ( MonadMask )
import Control.Monad.Reader

import Data.List.NonEmpty hiding (filter)

import Shh
import Tokenomia.Common.Shell.InteractiveMenu (askSelect)


import Tokenomia.Adapter.Cardano.CLI as CardanoCLI
import Tokenomia.Adapter.Cardano.CLI.UTxO

load SearchPath ["echo","printf"]

select :: (MonadIO m , MonadMask m) => m (Maybe Wallet)
select = do
    query_registered_wallets
      >>=  \case
            Nothing -> return Nothing
            Just a -> Just <$> showMenu a
          . nonEmpty
  where
  showMenu :: (MonadIO m , MonadMask m) => NonEmpty Wallet -> m Wallet
  showMenu wallets = liftIO $ askSelect wallets

selectUTxO
  ::( MonadIO m
    , MonadMask m
    , MonadReader Environment m)
  =>  Wallet
  ->  m (Maybe UTxO)
selectUTxO = selectUTxOFilterBy (const True)

selectUTxOFilterBy
  ::( MonadIO m
    , MonadMask m
    , MonadReader Environment m)
  => (UTxO -> Bool)
  -> Wallet
  ->  m (Maybe UTxO)
selectUTxOFilterBy predicate  Wallet {..}  =
    filter predicate <$> getUTxOs paymentAddress
      >>=  \case
            Nothing -> return Nothing
            Just a -> Just <$> showMenu a
          . nonEmpty

  where
    showMenu :: (MonadIO m , MonadMask m) =>  NonEmpty UTxO -> m UTxO
    showMenu a =  liftIO $ askSelect a


createAndRegister
  ::( MonadIO m, MonadReader Environment m)
  => m ()
createAndRegister = do
  liftIO $ echo "-----------------------------------"
  walletName <- liftIO $ echo "-n" "> Wallet Name : " >>  getLine
  CardanoCLI.register_shelley_wallet walletName
  liftIO $ echo "Wallet Created and Registered!"
  liftIO $ echo "-----------------------------------"


list
  ::( MonadIO m, MonadReader Environment m)
  => m ()
list = do
  CardanoCLI.query_registered_wallets
   >>= \case
         [] -> liftIO $ echo "No Wallet Registered!"
         wallets -> do
           liftIO $ echo "-----------------------------------"
           liftIO $ echo "Wallets Registered"
           liftIO $ echo "-----------------------------------"
           mapM_ (\Wallet{..} -> do
            liftIO $ echo ("> " <> name)
              <> echo ("    Payment Address : " <> paymentAddress)
            utxos <- getUTxOs paymentAddress
            case utxos of
              [] -> liftIO $ echo "\t(No UTxOs Available)"
              a  -> mapM_ (\utxo -> liftIO $ echo ("\t- " <> show utxo)) a
            ) wallets
           liftIO $ echo "-----------------------------------"

remove :: (MonadIO m) => m ()
remove = do
  liftIO $ echo "-----------------------------------"
  liftIO $ echo "Select the Wallet to remove :"
    >> select
    >>= \case
        Nothing ->
          echo "No Wallet Registered !"
        Just Wallet {..} -> CardanoCLI.remove_shelley_wallet name

  liftIO $ echo "-----------------------------------"

getSeedPhrase :: IO String
getSeedPhrase = do
  seedPhrase <- liftIO $ echo "-n" "> please enter your 24 words mnemonics then press enter : " >> getLine
  if Prelude.length (words seedPhrase) /= 24
    then do
      liftIO $ printf "\n We said 24 words !\n"
      getSeedPhrase
    else return seedPhrase

recover :: (MonadIO m, MonadReader Environment m) => m ()
recover = do
  liftIO $ echo "-----------------------------------"
  walletName <- liftIO $ echo "-n" "> Wallet Name : " >>  getLine
  seedPhrase <- liftIO getSeedPhrase
  CardanoCLI.recover_from_seed_phrase walletName seedPhrase
  liftIO $ echo "-----------------------------------"
    