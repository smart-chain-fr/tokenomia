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

module Tokenomia.Wallet.CLI 
  ( select
  , selectUTxO
  , createAndRegister
  , list
  , remove) 
  where

import Data.Function ((&))
import Control.Monad.Catch ( MonadMask )
import Control.Monad.Reader
import qualified Data.Text as T 
import Data.Text ( Text,pack )
import Data.Maybe ( fromJust )

import Data.List.NonEmpty

import Shh 
import Byline.Menu
    ( runBylineT,
      text,
      askWithMenuRepeatedly,
      menu,
      menuSuffix,
      Stylized,
      ToStylizedText(..),
      Menu )
import Byline.Internal.Stylized ()


import Tokenomia.Adapter.Cardano.CLI as CardanoCLI
import Tokenomia.Adapter.Cardano.CLI.UTxO

load SearchPath ["echo","ssh","cat"]

select :: (MonadIO m , MonadMask m) => m (Maybe Wallet)
select = do
    query_registered_wallets
      >>=  \case 
            Nothing -> return Nothing 
            Just a -> Just <$> showMenu a
          . nonEmpty
  where           
  showMenu :: (MonadIO m , MonadMask m) => NonEmpty Wallet -> m Wallet
  showMenu wallets = fmap fromJust (runBylineT $ askWithMenuRepeatedly (menuConfig wallets) prompt onError)

  menuConfig :: NonEmpty Wallet -> Menu Wallet
  menuConfig wallets = menu wallets & menuSuffix "- "

  prompt :: Stylized Text
  prompt = text "> please choose a wallet (provide the index) : "

  onError :: Stylized Text
  onError = text "> invalid index provided ! "

selectUTxO 
  ::( MonadIO m 
    , MonadMask m
    , MonadReader Environment m) 
  =>  Wallet 
  ->  m (Maybe UTxO) 
selectUTxO Wallet {..}= 
    getUTxOs paymentAddress
      >>=  \case 
            Nothing -> return Nothing 
            Just a -> Just <$> showMenu a
          . nonEmpty
      
  where           
  showMenu :: (MonadIO m , MonadMask m) =>  NonEmpty UTxO -> m UTxO
  showMenu a =  fmap fromJust (runBylineT $ askWithMenuRepeatedly (menuConfig a) prompt onError)

  menuConfig :: NonEmpty UTxO -> Menu UTxO
  menuConfig a = menu a & menuSuffix "- "

  prompt :: Stylized Text
  prompt = text "> please choose an utxo (provide the index) : "

  onError :: Stylized Text
  onError = text "> invalid index provided ! "

instance ToStylizedText UTxO where
  toStylizedText = text . T.pack . show 

instance ToStylizedText Wallet where
  toStylizedText Wallet {..} = text . pack $ name
    
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
