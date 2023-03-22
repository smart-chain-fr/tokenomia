{-# LANGUAGE DataKinds                                 #-}
{-# LANGUAGE ExtendedDefaultRules                      #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE OverloadedStrings                         #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
{-# LANGUAGE TemplateHaskell                           #-}
{-# LANGUAGE TypeApplications                          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures           #-}
{-# OPTIONS_GHC -fno-warn-type-defaults                #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds             #-}

module Tokenomia.CLI
    ( main
    ) where

import Control.Monad.Except                            ( MonadError(catchError), MonadIO(liftIO), runExceptT )
import Control.Monad.Reader                            ( MonadReader, ReaderT(runReaderT) )
import Data.List.NonEmpty
    as NonEmpty                                        ( NonEmpty, fromList )
import Shh                                             ( ExecReference(SearchPath), load )
import Streamly.Prelude qualified as S

import Tokenomia.Ada.Transfer qualified as Ada
import Tokenomia.Common.Environment
    ( CustomNetworkArgs(..)
    , Environment
    , TokenomiaNetwork(..)
    , getNetworkEnvironment
    , readCustomNetworkArgsFile
    )
import Tokenomia.Common.Error                          ( TokenomiaError(..) )
import Tokenomia.Common.Shell.Console                  ( clearConsole, printLn, printOpt )
import Tokenomia.Common.Shell.InteractiveMenu          ( DisplayMenuItem(..), askMenu, askString )
import Tokenomia.Node.Status qualified as Node
import Tokenomia.Token.CLAPStyle.Burn qualified as Token
import Tokenomia.Token.CLAPStyle.Mint qualified as Token
import Tokenomia.Token.Transfer qualified as Token
import Tokenomia.Vesting.GenerateNative qualified as Vesting
import Tokenomia.Vesting.Sendings qualified as Vesting
import Tokenomia.Wallet.CLI qualified as Wallet
import Tokenomia.Wallet.Collateral.Write qualified as Wallet

load SearchPath ["cardano-cli"]

main :: IO ()
main = do
    clearConsole
    printLn "#############################"
    printLn "#   Welcome to Tokenomia    #"
    printLn "#############################"
    printLn ""
    network <- liftIO selectNetwork
    environment <- getNetworkEnvironment network
    clearConsole
    runExceptT (runReaderT recursiveMenu environment) >>= \case
        Left e -> printLn $ "An unexpected error occured :" <> show e
        Right _ -> return ()

    printLn "#############################"
    printLn "#   End of Tokenomia        #"
    printLn "#############################"


selectNetwork :: IO TokenomiaNetwork
selectNetwork = do
    printLn "----------------------"
    printLn "  Select a network"
    printLn "----------------------"
    askMenu networks >>= \case
        SelectMainnet -> pure MainnetNetwork
        SelectPreprod -> pure PreprodNetwork
        SelectTestnet -> pure TestnetNetwork
        SelectCustom  -> CustomNetwork <$> inputCustomNetworkArgs


inputCustomNetworkArgs :: IO CustomNetworkArgs
inputCustomNetworkArgs = do
    clearConsole
    printLn "--------------------------------------------"
    printLn "  Enter custom network arguments file path"
    printLn "--------------------------------------------"
    path <- askString "- File path : "
    readCustomNetworkArgsFile path >>= \case
        Right args -> pure args
        Left e -> do
            printLn ""
            printLn $ "Invalid custom network arguments :" <> e
            printLn ""
            waitAndClear
            inputCustomNetworkArgs


networks :: NonEmpty SelectEnvironment
networks = NonEmpty.fromList
  [ SelectMainnet
  , SelectPreprod
  , SelectTestnet
  , SelectCustom
  ]

data SelectEnvironment
  = SelectMainnet
  | SelectPreprod
  | SelectTestnet
  | SelectCustom

instance DisplayMenuItem SelectEnvironment where
  displayMenuItem item = case item of
    SelectMainnet   -> "Mainnet (magicNumber 764824073)"
    SelectPreprod   -> "Preprod (magicNumber 1)"
    SelectTestnet   -> "`Old` Testnet (magicNumber 1097911063)"
    SelectCustom    -> "Use a custom network"


waitAndClear :: IO ()
waitAndClear = do
    _ <- printOpt "-n" "> press enter to continue..."  >>  getLine
    clearConsole


recursiveMenu ::
  ( S.MonadAsync m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>  m ()
recursiveMenu = do
  printLn "----------------------"
  printLn "  Select an action"
  printLn "----------------------"
  action <- askMenu actions
  runAction action
    `catchError`
      (\case
        NetworkNotSupported errorMsg ->     printLn $ "Network not supported : " <> errorMsg
        NoWalletRegistered ->        printLn "Register a Wallet First..."
        NoWalletWithoutCollateral -> printLn "All Wallets contain collateral..."
        NoWalletWithCollateral    -> printLn "No Wallets with collateral..."
        WalletWithoutCollateral   -> printLn "Wallets selected without a required collateral..."
        AlreadyACollateral        -> printLn "Collateral Already Created..."
        NoADAsOnChildAddress ->             printLn "Please, add ADAs to your wallet..."
        NoUTxOWithOnlyOneToken    -> printLn "Please, add tokens to your wallet..."
        TryingToBurnTokenWithoutScriptRegistered
                                  -> printLn "You can't burn tokens without the monetary script registered in Tokenomia"
        BlockFrostError e         -> printLn $ "Blockfrost issue " <> show e
        NoActiveAddressesOnWallet -> printLn "No Active Addresses, add funds on this wallet"
        InconsistenciesBlockFrostVSLocalNode errorMsg ->
                                     printLn $ "Inconsistencies Blockfrost vs Local Node  :" <> errorMsg
        NoDerivedChildAddress  -> printLn "No derived child adresses on this wallet"
        NoUTxOsFound           -> printLn "No UTxOs found"
        InvalidTransaction e -> printLn $ "Invalid Transaction : " <> e
        InvalidPrivateSale e -> printLn $ "Invalid Private sale input : " <> e
        QueryFailure e       -> printLn $ "QueryFailure : " <> e
        ChildAddressNotIndexed w address
                                  -> printLn $ "Address not indexed " <> show (w,address) <>", please generate your indexes appropriately"
        MalformedAddress            -> printLn "Sendings - Invalid treasury address"
        SendingsContainsZeroValue           -> printLn "Sendings - Input file contains entry with zero value"
        SendingsNoSuchTransactions txhs     -> printLn $ "Sendings - No such transactions : " <> show txhs
        SendingsJSONDecodingFailure jsonErr -> printLn $ "Sendings - JSON Decoding Failure : " <> show jsonErr
        SendingsValueMismatch _             -> printLn "Sendings - Value mismatch"
        )


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
      VestingVerifySendings -> Vesting.verifySendings
      VestingGenerateNative -> Vesting.generatePrivateSaleFiles
      NodeStatus           -> Node.displayStatus
      NodeTranslateSlotToTime    -> Node.translateSlotToTime
      NodeTranslateTimeToSlot    -> Node.translateTimeToSlot


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
    VestingVerifySendings,
    VestingGenerateNative,
    NodeStatus,
    NodeTranslateSlotToTime,
    NodeTranslateTimeToSlot
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
  | VestingVerifySendings
  | VestingGenerateNative
  | NodeStatus
  | NodeTranslateSlotToTime
  | NodeTranslateTimeToSlot

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
    VestingVerifySendings -> "[Vesting] - Verify Sendings"
    VestingGenerateNative -> "[Vesting] - Generate Database and airdrop outputs"
    NodeStatus            ->  "[Node]    - Status"
    NodeTranslateSlotToTime -> "[Node]    - Translate Slot To Time"
    NodeTranslateTimeToSlot -> "[Node]    - Translate Time To Slot"
