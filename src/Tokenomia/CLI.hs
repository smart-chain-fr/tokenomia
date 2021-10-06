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
module Tokenomia.CLI (main) where

import Tokenomia.Common.Shell.InteractiveMenu

import Control.Monad.Reader 
import Control.Monad.Catch ( MonadMask )

import Data.List.NonEmpty as NonEmpty ( NonEmpty, fromList )

import Shh

import           Tokenomia.Adapter.Cardano.CLI
import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Tokenomia.Token.CLAPStyle.Mint as Token
import qualified Tokenomia.Token.CLAPStyle.Burn as Token
import qualified Tokenomia.Token.Transfer as Token

load SearchPath ["echo","cardano-cli","clear"]

main ::  IO ()
main = do 
    clear
    echo "#############################"
    echo "#   Welcome to Tokenomia    #"
    echo "#############################"
    echo ""
    echo "FYI >> you'll operate over the Testnet Network (magic number = 1097911063)"
    echo ""
    waitAndClear
    runReaderT recursiveMenu (Testnet {magicNumber = 1097911063}) 

    echo "#############################"
    echo "#   End of Tokenomia        #"
    echo "#############################"

waitAndClear :: IO()
waitAndClear = do 
   _ <- echo "-n" "> press enter to continue..." >>  getLine
   clear

recursiveMenu :: (MonadMask m,MonadIO m,MonadReader Environment m) =>  m()
recursiveMenu = do
  liftIO $ echo "----------------------"
  liftIO $ echo "  Select an action"
  liftIO $ echo "----------------------"
  r <- liftIO $ askSelect actions
  case r of
      WalletList    -> Wallet.list
      WalletAdd     -> Wallet.createAndRegister
      WalletRecover -> Wallet.recover
      WalletRemove  -> Wallet.remove
      TokenMint     -> Token.mint
      TokenBurn     -> Token.burn
      TokenTransfer -> Token.transfer
  liftIO waitAndClear         
  recursiveMenu

actions :: NonEmpty Action
actions = NonEmpty.fromList [
    WalletList,
    WalletAdd,
    WalletRemove,
    WalletRecover,
    TokenMint,
    TokenBurn,
    TokenTransfer
    ]

data Action
  = WalletList
  | WalletAdd
  | WalletRecover
  | WalletRemove
  | TokenMint
  | TokenBurn
  | TokenTransfer

instance DisplayMenuItem Action where
  displayMenuItem item = case item of
    WalletList    -> "[Wallet] - List Registered Ones" 
    WalletAdd     -> "[Wallet] - Add "
    WalletRecover -> "[Wallet] - Recover from 24 words seed phrase"
    WalletRemove  -> "[Wallet] - Remove"
    TokenMint     -> "[Token]  - Mint with CLAP type policy (Fix Total Supply | one-time Minting and open Burning Policy )"
    TokenBurn     -> "[Token]  - Burn Tokens with CLAP type policy"
    TokenTransfer -> "[Token]  - Transfer "
