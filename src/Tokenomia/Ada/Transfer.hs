{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.Ada.Transfer
    ( transfer
    , transfer' ) where

import           Data.List.NonEmpty
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except

import           Ledger.Ada ( lovelaceValueOf )
import           Tokenomia.Common.Environment
import           Tokenomia.Common.Transacting hiding (value)


import           Tokenomia.Wallet.UTxO
import           Tokenomia.Wallet.LocalRepository
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.Collateral.Read
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu (askString, ask, askStringLeaveBlankOption)
import           Tokenomia.Common.Value
import           Tokenomia.Wallet.Type
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Common.Address

transfer :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
    => m ()
transfer = do
    Wallet {name} <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
        >>= \wallets -> do
            printLn "Select the minter wallet : "
            askToChooseAmongGivenWallets wallets
    WalletUTxO {utxo = UTxO {value}} <- selectBiggestStrictlyADAsNotCollateral (ChildAddressRef name 0) >>= whenNothingThrow NoADAInWallet

    printLn  $                                       "- Amount Available                : " <> showValue value
    amount <- ask @Integer                           "- Amount of Lovelaces to transfer : "
    receiverAddr <- Address <$> askString         "- Receiver address : "
    labelMaybe <- askStringLeaveBlankOption "- Add label to your transaction (leave blank if no) : "
    transfer' name  receiverAddr amount labelMaybe


type MetadataLabel = String

transfer' :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
    => WalletName
    -> Address
    -> Integer
    -> Maybe MetadataLabel
    -> m ()
transfer' senderWallet receiverAddr amount labelMaybe = do
    let firstAddress = ChildAddressRef senderWallet 0
    ada <- selectBiggestStrictlyADAsNotCollateral firstAddress >>= whenNothingThrow NoADAInWallet
    metadataMaybe <- mapM (fmap Metadata . createMetadataFile)  labelMaybe

    submit
      TxBuild
        { inputsFromWallet  = FromWallet ada :| []
        , inputsFromScript  = Nothing 
        , outputs = ToWallet receiverAddr (lovelaceValueOf amount) :| []
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..}



