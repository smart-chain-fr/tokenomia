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


import           Control.Monad.Reader
import           Control.Monad.Except


import Data.List.NonEmpty as NonEmpty ( NonEmpty, fromList )

import Shh

import           Tokenomia.Common.Shell.InteractiveMenu

import           Tokenomia.Adapter.Cardano.CLI.Environment

import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Tokenomia.Wallet.Collateral as Wallet

import qualified Tokenomia.Token.CLAPStyle.Mint as Token
import qualified Tokenomia.Token.CLAPStyle.Burn as Token
import qualified Tokenomia.Token.Transfer as Token

import qualified Tokenomia.Ada.Transfer as Ada

import qualified Tokenomia.Vesting.Vest as Vesting
import qualified Tokenomia.Vesting.Retrieve as Vesting

import qualified Tokenomia.Node.Status as Node

import           Tokenomia.Adapter.Cardano.CLI.Transaction

load SearchPath ["echo","cardano-cli","clear"]

main ::  IO ()
main = do 
    clear
    echo "#############################"
    echo "#   Welcome to Tokenomia    #"
    echo "#############################"
    echo ""
    selectNetwork

    echo "#############################"
    echo "#   End of Tokenomia        #"
    echo "#############################"

waitAndClear :: IO()
waitAndClear = do 
   _ <- echo "-n" "> press enter to continue..." >>  getLine
   clear

selectNetwork :: IO()
selectNetwork = do
  liftIO $ echo "----------------------"
  liftIO $ echo "  Select a network"
  liftIO $ echo "----------------------"  
  environment <- liftIO $ askMenu networks >>= \case 
      SelectTestnet     -> getTestnetEnvironmment 1097911063 
      SelectMainnet     -> getMainnetEnvironmment 764824073
  clear
  result :: Either BuildingTxError () <- runExceptT $ runReaderT recursiveMenu environment 
  case result of 
          Left e -> liftIO $ echo $ "An unexpected error occured :" <> show e
          Right _ -> return ()



networks :: NonEmpty SelectEnvironment
networks = NonEmpty.fromList [
  SelectTestnet,
  SelectMainnet
  ]

data SelectEnvironment 
  = SelectTestnet
  | SelectMainnet

instance DisplayMenuItem SelectEnvironment where
  displayMenuItem item = case item of
    SelectTestnet   -> "Testnet (magicNumber 1097911063)" 
    SelectMainnet   -> "Mainnet (magicNumber 764824073)"


recursiveMenu 
  :: ( MonadIO m
     , MonadReader Environment m
     , MonadError BuildingTxError m) =>  m ()
recursiveMenu = do
  liftIO $ echo "----------------------"
  liftIO $ echo "  Select an action"
  liftIO $ echo "----------------------"
  r <- liftIO $ askMenu actions
  case r of
      WalletList       -> Wallet.list
      WalletCreate        -> Wallet.createAndRegister
      WalletCollateral -> Wallet.createCollateral 
                            `catchError` 
                              (\case 
                                NoWalletRegistered ->        liftIO $ echo "Register a Wallet First..."
                                NoWalletWithoutCollateral -> liftIO $ echo "All Wallets contain collateral..."  
                                NoWalletWithCollateral    -> liftIO $ echo "No Wallets with collateral..."
                                WalletWithoutCollateral   -> liftIO $ echo "Wallets selected without a required collateral..."
                                AlreadyACollateral utxo   -> liftIO $ echo ("Collateral Already Created..." <> show utxo)
                                NoADAInWallet ->             liftIO $ echo "Please, add ADAs to your wallet..."
                                NoUTxOWithOnlyOneToken ->    liftIO $ echo "Please, add tokens to your wallet..."
                                TryingToBurnTokenWithoutScriptRegistered -> liftIO $ echo "You can't burn tokens without the monetary script registered in Tokenomia")


      WalletRestore    -> Wallet.restore
      WalletRemove     -> Wallet.remove
      TokenMint        -> Token.mint
      TokenBurn        -> Token.burn
      TokenTransfer    -> Token.transfer
      AdaTransfer      -> Ada.transfer
      VestingVestFunds  -> Vesting.vestFunds
      VestingRetrieveFunds -> Vesting.retrieveFunds
      NodeStatus           -> Node.displayStatus
  liftIO waitAndClear         
  recursiveMenu

actions :: NonEmpty Action
actions = NonEmpty.fromList [
    WalletList,
    WalletCreate,
    WalletCollateral,
    WalletRemove,
    WalletRestore,
    TokenMint,
    TokenBurn,
    TokenTransfer,
    AdaTransfer,
    VestingVestFunds,
    VestingRetrieveFunds,
    NodeStatus
    ]

data Action
  = WalletList
  | WalletCreate
  | WalletCollateral
  | WalletRestore
  | WalletRemove
  | TokenMint
  | TokenBurn
  | TokenTransfer
  | AdaTransfer
  | VestingVestFunds
  | VestingRetrieveFunds
  | NodeStatus 

instance DisplayMenuItem Action where
  displayMenuItem item = case item of
    WalletList            -> " [Wallet]  - List Registered Wallets" 
    WalletRestore         -> " [Wallet]  - Restore Wallets from your 24 words seed phrase (Shelley Wallet)"
    WalletCreate          -> " [Wallet]  - Create a new Wallet"
    WalletCollateral      -> " [Wallet]  - Create a unique collateral for transfer"
    WalletRemove          -> " [Wallet]  - Remove an existing Wallet"
    TokenMint             -> " [Token]   - Mint with CLAP type policy (Fix Total Supply | one-time Minting and open Burning Policy)"
    TokenBurn             -> " [Token]   - Burn Tokens with CLAP type policy"
    TokenTransfer         -> " [Token]   - Transfer Tokens"
    AdaTransfer           -> " [Ada]     - Transfer ADAs"
    VestingVestFunds      ->  "[Vesting] - Vest Funds"
    VestingRetrieveFunds  ->  "[Vesting] - Retrieve Funds"
    NodeStatus            ->  "[Node]    - Status"


