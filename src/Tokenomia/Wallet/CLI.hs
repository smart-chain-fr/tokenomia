{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tokenomia.Wallet.CLI 
  ( select
  , selectUTxO
  , createAndRegister
  , receiveADAsByFaucet
  , list) 
  where

import Shh 
import Data.Function ((&))
import Tokenomia.Adapter.Cardano.CLI as CardanoCLI
import Byline.Menu
    ( runBylineT,
      text,
      askWithMenuRepeatedly,
      menu,
      menuSuffix,
      Stylized,
      ToStylizedText(..),
      Menu )
import Data.Text ( Text,pack )
import Data.Maybe ( fromJust )
import Byline.Internal.Stylized ()
import Control.Monad.IO.Class ( MonadIO(..) )
import Tokenomia.Adapter.Cardano.CLI.UTxO
import Data.List.NonEmpty
import qualified Data.Text as T 
import Control.Monad.Catch ( MonadMask )

load SearchPath ["echo","ssh","cat","curl"]

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

selectUTxO :: (MonadIO m , MonadMask m) =>  Wallet ->  m (Maybe UTxO) 
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
    
createAndRegister :: IO ()
createAndRegister = do
  echo "-----------------------------------"
  walletName <- echo "-n" "> Wallet Name : " >>  getLine
  CardanoCLI.register_shelley_wallet walletName
  echo "Wallet Created and Registered!"
  echo "-----------------------------------"

receiveADAsByFaucet :: IO ()
receiveADAsByFaucet = do 
  echo "-----------------------------------"
  echo "Select the Wallet for receiving Money :"
    >>  select 
    >>= \case 
        Nothing -> 
          echo "No Wallet Registered !"
        Just Wallet {..} -> 
          curl "-v" "-XPOST" 
            ("https://faucet.alonzo-purple.dev.cardano.org/send-money/" <> paymentAddress <>"?apiKey=jv3NBtZeaL0lZUxgqq8slTttX3BzViI7")  
  
  echo "-----------------------------------"

list :: IO ()
list = do
  CardanoCLI.query_registered_wallets
   >>= \case 
         [] -> echo "No Wallet Registered!"
         wallets -> do 
           echo "-----------------------------------" 
           echo "Wallets Registered" 
           echo "-----------------------------------" 
           mapM_ (\Wallet{..} -> do  
            echo ("> " <> name)
              <> echo ("    Payment Address : " <> paymentAddress)
            utxos <- getUTxOs paymentAddress
            case utxos of 
              [] -> echo "\t(No UTxOs Available)"  
              a  -> mapM_ (\utxo -> echo ("\t- " <> show utxo)) a  
            ) wallets
           echo "-----------------------------------"
  
  
