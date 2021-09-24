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
module Tokenomia.CLI (main) where

import Shh 
import Data.Function ((&))
import  qualified Tokenomia.Token.CLAPStyle.Mint.CLI as Mint
import  qualified Tokenomia.Transfer.CLI as Transfer
import Tokenomia.Adapter.Cardano.CardanoCLI as CardanoCLI
import Data.List.NonEmpty as NonEmpty ( NonEmpty, fromList )
import Byline.Menu
    ( runBylineT,
      text,
      askWithMenuRepeatedly,
      menu,
      menuSuffix,
      Stylized,
      ToStylizedText(..),
      Menu )
import Data.Text ( Text )
import Data.Maybe ( fromJust )
import Byline.Internal.Stylized ()
import Tokenomia.Wallet.CLI (selectWallet) 

load SearchPath ["echo","ssh","cat","curl"]

main :: IO ()
main = do
    echo "----------------------"
    echo "Select An action"
    echo "----------------------"
    showActionMenu >>=
     \case
        WalletList -> listWallet 
        WalletAdd  -> addWallet
        WalletRemove  -> echo "TODO"
        TokenMint  -> 
          echo "Select the Minter Wallet :"
          >>  selectWallet 
          >>= \case 
              Nothing -> 
                echo "No Wallet Registered !"
              Just wallet -> Mint.mintI wallet 
        TokenBurn  ->  echo "TODO"
        Transfer   -> Transfer.run 
        ReceiveByFaucet -> receiveByFaucet  
    main


addWallet :: IO ()
addWallet = do
  echo "-----------------------------------"
  walletName <- echo "-n" "> Wallet Name : " >>  getLine
  CardanoCLI.register_shelley_wallet walletName
  echo "Wallet Created and Registered!"
  echo "-----------------------------------"

removeWallet :: IO ()
removeWallet = do
  echo "-----------------------------------"
  echo "Select the Wallet to remove :"
    >> selectWallet
    >>= \case
        Nothing ->
          echo "No Wallet Registered !"
        Just Wallet {..} -> CardanoCLI.remove_shelley_wallet name

  echo "-----------------------------------"

receiveByFaucet :: IO ()
receiveByFaucet = do 
  echo "-----------------------------------"
  echo "Select the Wallet for receiving Money :"
    >>  selectWallet 
    >>= \case 
        Nothing -> 
          echo "No Wallet Registered !"
        Just Wallet {..} -> 
          curl "-v" "-XPOST" 
            ("https://faucet.alonzo-purple.dev.cardano.org/send-money/" <> paymentAddress <>"?apiKey=jv3NBtZeaL0lZUxgqq8slTttX3BzViI7")  
  
  echo "-----------------------------------"

listWallet :: IO ()
listWallet = do
  CardanoCLI.query_registered_wallets
   >>= \case 
         Nothing -> echo "No Wallet Registered!"
         Just wallets ->  mapM_ (\Wallet{..} ->  
            echo "######################"
              <> echo ("Name : " <> name)
              <> echo ("Payment Address : " <> paymentAddress)
              <> query_utxo paymentAddress) wallets
  echo "######################"
  


showActionMenu :: IO Action
showActionMenu = fmap fromJust (runBylineT $ askWithMenuRepeatedly menuConfig prompt onError)

menuConfig :: Menu Action
menuConfig = menu actions & menuSuffix "- "

prompt :: Stylized Text
prompt = text "> please choose an action (provide the index) : "

onError :: Stylized Text
onError = text "> please choose an action (provide the index) : "

actions :: NonEmpty Action
actions = NonEmpty.fromList [
    WalletList,
    WalletAdd,
    WalletRemove,
    TokenMint,
    TokenBurn,
    Transfer,
    ReceiveByFaucet
    ]

data Action
  = WalletList
  | WalletAdd
  | WalletRemove
  | TokenMint
  | TokenBurn
  | Transfer
  | ReceiveByFaucet
  deriving (Show)


instance ToStylizedText Action where
  toStylizedText item = case item of
    WalletList   -> "[Wallet] - List Registered Ones" 
    WalletAdd    -> "[Wallet] - Add "
    WalletRemove -> "[Wallet] - Remove (TODO)"
    TokenMint    -> "[Token]  - Mint (Fix Total Supply | one-time Minting and open Burning Policy )"
    TokenBurn    -> "[Token]  - Burn (TODO)"
    Transfer     -> "Transfer "
    ReceiveByFaucet -> "Ask ADAs from Faucet (Testnet Only)"
