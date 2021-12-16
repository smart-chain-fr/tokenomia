{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.Ada.Transfer
    ( transfer
    , transfer' ) where

import           Prelude
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except

import           Tokenomia.Common.Environment
import           Tokenomia.Common.Transacting

import Data.List.NonEmpty as NEL
import           Ledger.Ada as Ada
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
            printLn "Select the wallet containing funds : "
            askToChooseAmongGivenWallets wallets
    WalletUTxO {utxo = UTxO {value}} <- selectBiggestStrictlyADAsNotCollateral (ChildAddressRef name 0) >>= whenNothingThrow NoADAsOnChildAddress

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
    ada <- selectBiggestStrictlyADAsNotCollateral firstAddress >>= whenNothingThrow NoADAsOnChildAddress
    metadataMaybe <- mapM (fmap Metadata . createMetadataFile)  labelMaybe

    buildAndSubmit
      (Unbalanced $ FeeAddressRef firstAddress)
      (Just $ CollateralAddressRef firstAddress)
      TxBuild
        { inputsFromWallet  = FromWallet ada :| []
        , inputsFromScript  = Nothing
        , outputs = ToWallet receiverAddr (lovelaceValueOf amount) Nothing :| []
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..}

