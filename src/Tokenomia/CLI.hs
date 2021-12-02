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
import           Tokenomia.Common.Shell.Console (printLn, clearConsole, printOpt)
import           Tokenomia.Adapter.Cardano.CLI.Environment

import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Tokenomia.Wallet.Collateral.Write as Wallet

import qualified Tokenomia.Token.CLAPStyle.Mint as Token
import qualified Tokenomia.Token.CLAPStyle.Burn as Token
import qualified Tokenomia.Token.Transfer as Token

import qualified Tokenomia.Ada.Transfer as Ada

import qualified Tokenomia.Vesting.Vest as Vesting
import qualified Tokenomia.Vesting.Retrieve as Vesting

import qualified Tokenomia.Node.Status as Node

import           Tokenomia.Adapter.Cardano.CLI.Transaction


load SearchPath ["cardano-cli"]

main ::  IO ()
main = do 
    clearConsole
    printLn "#############################"
    printLn "#   Welcome to Tokenomia    #"
    printLn "#############################"
    printLn ""
    selectNetwork

    printLn "#############################"
    printLn "#   End of Tokenomia        #"
    printLn "#############################"

waitAndClear :: IO()
waitAndClear = do 
   _ <- printOpt "-n" "> press enter to continue..."  >>  getLine
   clearConsole

selectNetwork :: IO()
selectNetwork = do
  printLn "----------------------"
  printLn "  Select a network"
  printLn "----------------------"  
  environment <- liftIO $ askMenu networks >>= \case 
      SelectTestnet     -> getTestnetEnvironmment 1097911063 
      SelectMainnet     -> getMainnetEnvironmment 764824073
  clearConsole
  result :: Either BuildingTxError () <- runExceptT $ runReaderT recursiveMenu environment 
  case result of 
          Left e -> printLn $ "An unexpected error occured :" <> show e
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
  printLn "----------------------"
  printLn "  Select an action"
  printLn "----------------------"
  action <- askMenu actions 
  runAction action 
    `catchError`
      (\case 
        NoWalletRegistered ->        printLn "Register a Wallet First..."
        NoWalletWithoutCollateral -> printLn "All Wallets contain collateral..."  
        NoWalletWithCollateral    -> printLn "No Wallets with collateral..."
        WalletWithoutCollateral   -> printLn "Wallets selected without a required collateral..."
        AlreadyACollateral utxo   -> printLn ("Collateral Already Created..." <> show utxo)
        NoADAInWallet ->             printLn "Please, add ADAs to your wallet..."
        NoUTxOWithOnlyOneToken    -> printLn "Please, add tokens to your wallet..."
        TryingToBurnTokenWithoutScriptRegistered 
                                  -> printLn "You can't burn tokens without the monetary script registered in Tokenomia"
        NoVestingInProgress       -> printLn "No vesting in progress"
        NoFundsToBeRetrieved      -> printLn "No funds to be retrieved"
        AllFundsLocked            -> printLn "All the funds alerady retrieved" 
        FundAlreadyRetrieved      -> printLn "All the funds are locked and can't be retrieve so far..")
            
  liftIO waitAndClear         
  recursiveMenu


runAction :: ( MonadIO m
     , MonadReader Environment m
     , MonadError BuildingTxError m) 
     => Action 
     -> m ()
runAction = \case    
      WalletList       -> Wallet.list
      WalletCreate     -> Wallet.createAndRegister
      WalletCollateral -> Wallet.createCollateral 
      WalletRestore    -> Wallet.restore
      WalletRemove     -> Wallet.remove
      TokenMint        -> Token.mint
      TokenBurn        -> Token.burn
      TokenTransfer    -> Token.transfer
      AdaTransfer      -> Ada.transfer
      VestingVestFunds -> Vesting.vestFunds
      VestingRetrieveFunds -> Vesting.retrieveFunds
      NodeStatus           -> Node.displayStatus

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


