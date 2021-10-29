{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Tokenomia.Wallet.CLI
  ( askToChooseAmongGivenWallets
  , askAmongAllWallets
  , askUTxO
  , askUTxOFilterBy
  , fetchUTxOFilterBy
  , askToChooseAmongGivenUTxOs
  , selectBiggestStrictlyADAsNotCollateral
  , createAndRegister
  , restore
  , list
  , remove)
  where

import           Prelude hiding (filter,head,last)
import qualified Prelude as P

import           Data.List.NonEmpty

import           Control.Monad.Reader

import           Tokenomia.Common.Shell.Console (printLn, printOpt)
import           Plutus.V1.Ledger.Value (flattenValue)

import           Tokenomia.Common.Shell.InteractiveMenu (askMenu)

import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Wallet as CardanoCLI
import           Tokenomia.Adapter.Cardano.CLI.UTxO

import qualified Tokenomia.Adapter.Cardano.CLI.UTxO.Query as UTxOs


askAmongAllWallets :: (MonadIO m, MonadReader Environment m) => m (Maybe Wallet)
askAmongAllWallets =
    CardanoCLI.query_registered_wallets
  >>=  \case
        Nothing -> return Nothing
        Just a -> Just <$> askMenu a
      . nonEmpty

askToChooseAmongGivenWallets :: (MonadIO m, MonadReader Environment m)
  => NonEmpty Wallet
  -> m Wallet
askToChooseAmongGivenWallets = askMenu

askUTxO
  ::( MonadIO m
    , MonadReader Environment m)
  =>  Wallet
  ->  m (Maybe UTxO)
askUTxO = askUTxOFilterBy (const True)



selectBiggestStrictlyADAsNotCollateral
  ::( MonadIO m
    , MonadReader Environment m)
  => Wallet
  -> m (Maybe UTxO)
selectBiggestStrictlyADAsNotCollateral Wallet {..} = do
  adas :: Maybe (NonEmpty UTxO) <- nonEmpty . P.filter ((&&) <$> containingStrictlyADAs <*> not . containsCollateral)  <$> UTxOs.query paymentAddress
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
  UTxOs.query paymentAddress >>= (\case
          Nothing -> return Nothing
          Just a -> Just <$> askMenu a) . nonEmpty . P.filter predicate


fetchUTxOFilterBy
  ::( MonadIO m
    , MonadReader Environment m)
  => (UTxO -> Bool)
  -> Wallet
  ->  m (Maybe (NonEmpty UTxO))
fetchUTxOFilterBy predicate  Wallet {..}  =  nonEmpty . P.filter predicate  <$> UTxOs.query paymentAddress


askToChooseAmongGivenUTxOs :: (MonadIO m, MonadReader Environment m)
  => NonEmpty UTxO
  -> m UTxO
askToChooseAmongGivenUTxOs = askMenu




createAndRegister
  ::( MonadIO m, MonadReader Environment m)
  => m ()
createAndRegister = do
  printLn "-----------------------------------"
  walletName <- printOpt "> Wallet Name : " "-n" >> liftIO getLine
  CardanoCLI.register_shelley_wallet walletName
  printLn "Wallet Created and Registered!"
  printLn "-----------------------------------"


list
  ::( MonadIO m, MonadReader Environment m)
  => m ()
list =
  CardanoCLI.query_registered_wallets
 >>= \case
       [] -> printLn "No Wallet Registered!"
       wallets -> do
         printLn "-----------------------------------"
         printLn "Wallets Registered"
         printLn "-----------------------------------"
         mapM_ (\Wallet{..} -> do
            printLn ("> " <> name)
            printLn ("    Public key : "      <> show publicKeyHash)
            printLn ("    Payment Address : " <> paymentAddress)
            utxos <- UTxOs.query paymentAddress
            case utxos of
              [] -> printLn "\t(No UTxOs Available)"
              a  -> mapM_ (\utxo -> printLn ("\t- " <> show utxo)) a
          ) wallets
         printLn "-----------------------------------"

remove :: (MonadIO m, MonadReader Environment m) => m ()
remove = do
  printLn "-----------------------------------"
  printLn "Select the Wallet to remove :"
  askAmongAllWallets
    >>= \case
        Nothing ->
          printLn "No Wallet Registered !"
        Just Wallet {..} -> CardanoCLI.remove_shelley_wallet name

  printLn "-----------------------------------"

getSeedPhrase :: IO String
getSeedPhrase = do
  seedPhrase <- printOpt "> please enter your 24 words mnemonics then press enter : " "-n" >> getLine
  if Prelude.length (words seedPhrase) /= 24
    then do
      printLn "\n We said 24 words !\n"
      getSeedPhrase
    else return seedPhrase

restore :: (MonadIO m, MonadReader Environment m) => m ()
restore = do
  printLn "-----------------------------------"
  walletName <- printOpt "> Wallet Name : " "-n" >> liftIO  getLine
  seedPhrase <- liftIO getSeedPhrase
  CardanoCLI.restore_from_seed_phrase walletName seedPhrase
  printLn "-----------------------------------"


