{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE NamedFieldPuns                            #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE TypeApplications                          #-}

module Tokenomia.Ada.Transfer
    ( transfer
    , transfer'
    ) where

import Control.Monad.Except                            ( MonadError )
import Control.Monad.Reader                            ( MonadIO, MonadReader )
import Data.List.NonEmpty                              ( NonEmpty((:|)) )
import Ledger.Ada                                      ( lovelaceValueOf )
import Tokenomia.Common.Address                        ( Address(..) )
import Tokenomia.Common.Environment                    ( Environment )
import Tokenomia.Common.Error                          ( TokenomiaError(..), whenNothingThrow, whenNullThrow )
import Tokenomia.Common.Shell.Console                  ( printLn )
import Tokenomia.Common.Shell.InteractiveMenu          ( ask, askString, askStringLeaveBlankOption )
import Tokenomia.Common.Transacting
    ( Metadata(..)
    , TxBalance(..)
    , TxBuild(..)
    , TxInFromWallet(..)
    , TxOut(..)
    , buildAndSubmit
    , createMetadataFile
    )
import Tokenomia.Common.Value                          ( showValueUtf8 )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
    ( ChildAddressRef(..)
    , CollateralAddressRef(..)
    , FeeAddressRef(..)
    )
import Tokenomia.Wallet.CLI
    ( askToChooseAmongGivenWallets
    , selectBiggestStrictlyADAsNotCollateral
    )
import Tokenomia.Wallet.Collateral.Read                ( fetchWalletsWithCollateral )
import Tokenomia.Wallet.Type                           ( Wallet(..), WalletName )
import Tokenomia.Wallet.UTxO                           ( UTxO(..) )
import Tokenomia.Wallet.WalletUTxO                     ( WalletUTxO(..) )


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

    printLn  $                                       "- Amount Available                : " <> showValueUtf8 value
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
