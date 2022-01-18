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
import           Tokenomia.Common.Error

import Data.List.NonEmpty as NonEmpty ( NonEmpty, fromList )

import Shh

import           Tokenomia.Common.Shell.InteractiveMenu
import           Tokenomia.Common.Shell.Console (printLn, clearConsole, printOpt)
import           Tokenomia.Common.Environment

import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Tokenomia.Wallet.Collateral.Write as Wallet

import qualified Tokenomia.Token.CLAPStyle.Mint as Token
import qualified Tokenomia.Token.CLAPStyle.Burn as Token
import qualified Tokenomia.Token.Transfer as Token

import qualified Tokenomia.Ada.Transfer as Ada

import qualified Tokenomia.Vesting.Vest as Vesting
import qualified Tokenomia.Vesting.Retrieve as Vesting

import qualified Tokenomia.Node.Status as Node
import qualified Tokenomia.ICO.Funds.Validation.Run as ICO.Validation
import qualified Tokenomia.ICO.Funds.Validation.Simulation.Transfer as ICO.Simulation
import qualified Tokenomia.ICO.Funds.Exchange.Run as ICO.Exchange
import qualified Tokenomia.ICO.LocalRepository as ICO
import qualified Tokenomia.ICO.Status as ICO
import qualified Streamly.Prelude as S

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
  result :: Either TokenomiaError () <- runExceptT $ runReaderT recursiveMenu environment 
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
  :: ( S.MonadAsync m
     , MonadReader Environment m
     , MonadError TokenomiaError m) =>  m ()
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
        AlreadyACollateral        -> printLn "Collateral Already Created..."
        NoADAsOnChildAddress ->             printLn "Please, add ADAs to your wallet..."
        NoUTxOWithOnlyOneToken    -> printLn "Please, add tokens to your wallet..."
        TryingToBurnTokenWithoutScriptRegistered 
                                  -> printLn "You can't burn tokens without the monetary script registered in Tokenomia"
        NoVestingInProgress       -> printLn "No vesting in progress"
        NoFundsToBeRetrieved      -> printLn "No funds to be retrieved"
        AllFundsLocked            -> printLn "All the funds alerady retrieved" 
        FundAlreadyRetrieved      -> printLn "All the funds are locked and can't be retrieve so far.."
        BlockFrostError e         -> printLn $ "Blockfrost issue " <> show e
        NoActiveAddressesOnWallet -> printLn "No Active Addresses, add funds on this wallet"
        InconsistenciesBlockFrostVSLocalNode errorMsg ->
                                     printLn $ "Inconsistencies Blockfrost vs Local Node  :" <> errorMsg
        NoICOTransactionsToBePerformOnThisWallet  
                                  -> printLn "No ICO Transactions to be performed on wallet used "
        NoDerivedChildAddress  -> printLn "No derived child adresses on this wallet"
        NoUTxOsFound           -> printLn "No UTxOs found"
        ICOExchangeUtxoWithoutHash 
                               -> printLn "ICO - Echange UTxOs without Hashes"
        ICOTokensDispatchedOnMultipleUTxOs 
                               -> printLn "ICO - Tokens Dispatched On Multiple UTxOs"
        ICONoValidTxs message  -> printLn $ "ICO - No Valid Txs Found : " <>  message
        InvalidTransaction e -> printLn $ "Invalid Transaction : " <> e
        ChildAddressNotIndexed w address 
                                  -> printLn $ "Address not indexed " <> show (w,address) <>", please generate your indexes appropriately")
            
  liftIO waitAndClear         
  recursiveMenu


runAction 
  :: ( S.MonadAsync m
     , MonadReader Environment m
     , MonadError TokenomiaError m) 
     => Action 
     -> m ()
runAction = \case    
      WalletList       -> Wallet.displayAll
      WalletDisplay    -> Wallet.askDisplayOne
      WalletDisplayWihtinIndexRange -> Wallet.askDisplayOneWithinIndexRange
      WalletCreate     -> Wallet.register
      WalletGenerateChildAddresses
                      -> Wallet.generateChildAddresses
      WalletCollateral -> Wallet.createCollateral 
      WalletRestore    -> Wallet.restoreByMnemonics
      WalletRemove     -> Wallet.remove
      TokenMint        -> Token.mint
      TokenBurn        -> Token.burn
      TokenTransfer    -> Token.transfer
      AdaTransfer      -> Ada.transfer
      VestingVestFunds -> Vesting.vestFunds
      VestingRetrieveFunds -> Vesting.retrieveFunds
      NodeStatus           -> Node.displayStatus
      ICOStatus                  -> ICO.askRoundSettings >>= ICO.displayStatus  
      ICOFundsValidationDryRun   -> ICO.askRoundSettings >>= ICO.Validation.dryRun
      ICOFundsValidationRun      -> ICO.askRoundSettings >>= ICO.Validation.run
      ICOExchangeDryRun          -> ICO.askRoundSettings >>= ICO.Exchange.dryRun
      ICOExchangeRun             -> ICO.askRoundSettings >>= ICO.Exchange.run  
      ICOFundsDispatchSimulation -> ICO.Simulation.dispatchAdasOnChildAdresses


actions :: NonEmpty Action
actions = NonEmpty.fromList [
    WalletList,
    WalletDisplay,
    WalletDisplayWihtinIndexRange,
    WalletCreate,
    WalletGenerateChildAddresses,
    WalletCollateral,
    WalletRemove,
    WalletRestore,
    TokenMint,
    TokenBurn,
    TokenTransfer,
    AdaTransfer,
    VestingVestFunds,
    VestingRetrieveFunds,
    NodeStatus,
    ICOStatus,
    ICOFundsValidationDryRun,
    ICOFundsValidationRun,
    ICOExchangeDryRun,
    ICOExchangeRun,
    ICOFundsDispatchSimulation
    ]

data Action
  = WalletList
  | WalletDisplay
  | WalletDisplayWihtinIndexRange
  | WalletCreate
  | WalletCollateral
  | WalletRestore
  | WalletRemove
  | WalletGenerateChildAddresses
  | TokenMint
  | TokenBurn
  | TokenTransfer
  | AdaTransfer
  | VestingVestFunds
  | VestingRetrieveFunds
  | NodeStatus 
  | ICOStatus
  | ICOFundsValidationDryRun
  | ICOFundsValidationRun
  | ICOExchangeDryRun
  | ICOExchangeRun
  | ICOFundsDispatchSimulation

instance DisplayMenuItem Action where
  displayMenuItem item = case item of
    WalletList            -> " [Wallet]  - List Registered Wallets" 
    WalletDisplay         -> " [Wallet]  - Display wallet utxos"
    WalletDisplayWihtinIndexRange   
                          -> " [Wallet]  - Display wallet utxos within child address index range"     
    WalletRestore         -> " [Wallet]  - Restore Wallets from your 24 words seed phrase (Shelley Wallet)"
    WalletCreate          -> " [Wallet]  - Create a new Wallet"
    WalletGenerateChildAddresses 
                          -> " [Wallet]  - Derive Child Adresses"
    WalletCollateral      -> " [Wallet]  - Create a unique collateral for transfer"
    WalletRemove          -> " [Wallet]  - Remove an existing Wallet"
    TokenMint             -> " [Token]   - Mint with CLAP type policy (Fix Total Supply | one-time Minting and open Burning Policy)"
    TokenBurn             ->  "[Token]   - Burn Tokens with CLAP type policy"
    TokenTransfer         ->  "[Token]   - Transfer Tokens"
    AdaTransfer           ->  "[Ada]     - Transfer ADAs"
    VestingVestFunds      ->  "[Vesting] - Vest Funds"
    VestingRetrieveFunds  ->  "[Vesting] - Retrieve Funds"
    NodeStatus            ->  "[Node]    - Status"
    ICOStatus                  ->  "[ICO]     - Status"
    ICOFundsValidationDryRun   ->  "[ICO]     - Funds Validation Dry Run"
    ICOFundsValidationRun      ->  "[ICO]     - Funds Validation Run"
    ICOExchangeDryRun          ->  "[ICO]     - Funds Exchange Dry Run"
    ICOExchangeRun             ->  "[ICO]     - Funds Exchange Run"
    ICOFundsDispatchSimulation ->  "[ICO]     - Funds Simulation (Dispatch ADAs on child addresses )"


