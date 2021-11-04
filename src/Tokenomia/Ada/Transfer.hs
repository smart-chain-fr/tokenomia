{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Ada.Transfer
    ( transfer ) where

import           Data.List.NonEmpty
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except

import           Ledger.Ada ( lovelaceValueOf )
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Adapter.Cardano.CLI.Transaction hiding (value)


import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.Collateral
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu (askString, ask, askStringLeaveBlankOption)


transfer
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => m ()
transfer = do
    wallet <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
        >>= \wallets -> do
            printLn "Select the minter wallet : "
            askToChooseAmongGivenWallets wallets
    utxo <- selectBiggestStrictlyADAsNotCollateral wallet >>= whenNothingThrow NoADAInWallet

    printLn  $                                       "- Amount Available                : " <> showValue (value utxo)
    amount <- ask @Integer                           "- Amount of Lovelaces to transfer : "
    receiverAddr <- Address <$> askString         "- Receiver address : "
    labelMaybe <- askStringLeaveBlankOption "- Add label to your transaction (leave blank if no) : "
    transfer' wallet  receiverAddr amount labelMaybe


type MetadataLabel = String

transfer'
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m)
    => Wallet
    -> Address
    -> Integer
    -> Maybe MetadataLabel
    -> m ()
transfer' senderWallet receiverAddr amount labelMaybe = do
    collateral <- txOutRef <$> (fetchCollateral senderWallet                        >>= whenNothingThrow WalletWithoutCollateral)
    adaAndFees <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral senderWallet >>= whenNothingThrow NoADAInWallet)
    metadataMaybe <- mapM (fmap Metadata . createMetadataFile)  labelMaybe

    submit'
      TxBuild
        { signingKeyPath = paymentSigningKeyPath senderWallet
        , txIns = FromWallet adaAndFees :| []
        , txOuts = ToWallet receiverAddr (lovelaceValueOf amount) :| []
        , changeAdress = paymentAddress senderWallet
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..}



