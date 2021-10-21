{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Tokenomia.Wallet.CLI
  ( askAmongGivenWallets
  , askAmongAllWallets
  , askUTxO
  , askUTxOFilterBy
  , selectUTxOForFees
  , createAndRegister
  , restore
  , list
  , remove)
  where

import Prelude hiding (filter,head,last)
import qualified Prelude as P
import Control.Monad.Reader

import Data.List.NonEmpty

import Shh
import Tokenomia.Common.Shell.InteractiveMenu (askMenu)


import Tokenomia.Adapter.Cardano.CLI as CardanoCLI
import Tokenomia.Adapter.Cardano.CLI.UTxO
import Plutus.V1.Ledger.Value (flattenValue)



load SearchPath ["echo","printf"]



askAmongAllWallets :: (MonadIO m, MonadReader Environment m) => m (Maybe Wallet)
askAmongAllWallets = do
    CardanoCLI.query_registered_wallets
      >>=  \case
            Nothing -> return Nothing
            Just a -> Just <$> askMenu a
          . nonEmpty

askAmongGivenWallets :: (MonadIO m, MonadReader Environment m) 
  => NonEmpty Wallet
  -> m Wallet
askAmongGivenWallets = askMenu

askUTxO
  ::( MonadIO m
    , MonadReader Environment m)
  =>  Wallet
  ->  m (Maybe UTxO)
askUTxO = askUTxOFilterBy (const True)


selectUTxOForFees
  ::( MonadIO m
    , MonadReader Environment m)
  => Wallet
  ->  m (Maybe UTxO)
selectUTxOForFees Wallet {..} = do
  adas :: Maybe (NonEmpty UTxO) <- nonEmpty . P.filter containingStrictlyADAs <$> getUTxOs paymentAddress
  return (last . sortWith (\UTxO {value} ->
                        maybe
                          0
                          (third . head)
                          (nonEmpty $ flattenValue value))
           <$> adas)
  where 
    third :: (a,b,c) -> c
    third (_,_,c) = c

askUTxOFilterBy
  ::( MonadIO m
    , MonadReader Environment m)
  => (UTxO -> Bool)
  -> Wallet
  ->  m (Maybe UTxO)
askUTxOFilterBy predicate  Wallet {..}  = 
  (nonEmpty . P.filter predicate  <$> getUTxOs paymentAddress) 
    >>= \case 
          Nothing -> return Nothing 
          Just a -> Just <$> askMenu a
  



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
              <> echo ("    Public key : "      <> show publicKeyHash)
              <> echo ("    Payment Address : " <> paymentAddress)
            utxos <- getUTxOs paymentAddress
            case utxos of
              [] -> liftIO $ echo "\t(No UTxOs Available)"
              a  -> mapM_ (\utxo -> liftIO $ echo ("\t- " <> show utxo)) a
            ) wallets
           liftIO $ echo "-----------------------------------"

remove :: (MonadIO m, MonadReader Environment m) => m ()
remove = do
  liftIO $ echo "-----------------------------------"
  liftIO $ echo "Select the Wallet to remove :"
  askAmongAllWallets
    >>= \case
        Nothing ->
          liftIO $ echo "No Wallet Registered !"
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

restore :: (MonadIO m, MonadReader Environment m) => m ()
restore = do
  liftIO $ echo "-----------------------------------"
  walletName <- liftIO $ echo "-n" "> Wallet Name : " >>  getLine
  seedPhrase <- liftIO getSeedPhrase
  CardanoCLI.restore_from_seed_phrase walletName seedPhrase
  liftIO $ echo "-----------------------------------"


