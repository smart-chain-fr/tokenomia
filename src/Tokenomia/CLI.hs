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
import Tokenomia.Adapter.Cardano.CLI as CardanoCLI
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
import qualified Tokenomia.Wallet.CLI as Wallet

load SearchPath ["echo","ssh","cat","curl"]

main :: IO ()
main = do
    echo "----------------------"
    echo "Select An action"
    echo "----------------------"
    showActionMenu >>=
     \case
        WalletList -> Wallet.list
        WalletAdd  -> Wallet.createAndRegister
        WalletRemove  -> Wallet.remove
        WalletReceiveByFaucet -> Wallet.receiveADAsByFaucet
        TokenMint  -> 
          echo "Select the Minter Wallet :"
          >>  Wallet.select
          >>= \case 
              Nothing -> echo "No Wallet Registered !"
              Just wallet -> Mint.mintI wallet 
        TokenBurn  ->  echo "TODO"
        Transfer   -> Transfer.run 
 
    main


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
    WalletReceiveByFaucet,
    TokenMint,
    TokenBurn,
    Transfer
    ]

data Action
  = WalletList
  | WalletAdd
  | WalletRemove
  | WalletReceiveByFaucet
  | TokenMint
  | TokenBurn
  | Transfer
  deriving (Show)


instance ToStylizedText Action where
  toStylizedText item = case item of
    WalletList   -> "[Wallet] - List Registered Ones" 
    WalletAdd    -> "[Wallet] - Add "
    WalletRemove -> "[Wallet] - Remove (TODO)"
    WalletReceiveByFaucet -> "[Wallet] - Ask ADAs from Faucet (Testnet Only)"
    TokenMint    -> "[Token]  - Mint (Fix Total Supply | one-time Minting and open Burning Policy )"
    TokenBurn    -> "[Token]  - Burn (TODO)"
    Transfer     -> "Transfer "
