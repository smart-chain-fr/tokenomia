{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE NamedFieldPuns                            #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE TypeApplications                          #-}

module Tokenomia.Token.CLAPStyle.Burn
    ( burn
    ) where

import PlutusTx.Prelude                                ( AdditiveGroup((-)), AdditiveSemigroup((+)) )
import Prelude hiding                                  ( print, (+), (-) )

import Control.Monad.Except                            ( MonadError )
import Control.Monad.Reader                            ( MonadIO, MonadReader )


import Data.List.NonEmpty                              ( NonEmpty((:|)) )
import Ledger.Ada                                      ( lovelaceValueOf )
import Ledger.Value                                    ( singleton )

import Tokenomia.Common.Environment                    ( Environment )
import Tokenomia.Common.Transacting
    ( MonetaryAction(..)
    , TxBalance(Unbalanced)
    , TxBuild(..)
    , TxInFromWallet(FromWallet)
    , TxOut(ToWallet)
    , buildAndSubmit
    )
import Tokenomia.Script.LocalRepository                ( getMonetaryPolicyPath )
import Tokenomia.Wallet.UTxO
    as UTxO                                            ( UTxO(..) )
import Tokenomia.Wallet.WalletUTxO                     ( WalletUTxO(..) )

import Tokenomia.Common.Error                          ( TokenomiaError(..), whenNothingThrow, whenNullThrow )
import Tokenomia.Common.Shell.Console                  ( printLn )
import Tokenomia.Common.Shell.InteractiveMenu          ( ask )
import Tokenomia.Common.Value                          ( containingOneToken, getTokenFrom )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
    ( ChildAddressRef(..)
    , CollateralAddressRef(..)
    , FeeAddressRef(..)
    )
import Tokenomia.Wallet.ChildAddress.LocalRepository   ( ChildAddress(..), fetchById )
import Tokenomia.Wallet.CLI                            ( askToChooseAmongGivenWallets, askUTxOFilterBy )
import Tokenomia.Wallet.Collateral.Read                ( fetchWalletsWithCollateral )
import Tokenomia.Wallet.Type                           ( Wallet(..), WalletName )

burn
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => m ()
burn = do
    Wallet{name} <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
        >>= \wallets -> do
            printLn "Select the burner wallet : "
            askToChooseAmongGivenWallets wallets
    printLn "- Select the utxo containing the tokens to burn :"
    utxoWithTokensToBurn <- askUTxOFilterBy (containingOneToken . UTxO.value . utxo ) (ChildAddressRef name 0) >>= whenNothingThrow NoUTxOWithOnlyOneToken
    amountToBurn  <- ask @Integer "- Amount to burn : "
    burn' name utxoWithTokensToBurn amountToBurn

burn'
    :: (  MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => WalletName
    -> WalletUTxO
    -> Integer
    -> m ()
burn' walletName walletUTxO@WalletUTxO {utxo = UTxO {value = totalAmountBurnable}} amountToBurn = do

    let (tokenPolicyHash,tokenNameSelected,_) = getTokenFrom totalAmountBurnable
        valueToBurn = singleton tokenPolicyHash tokenNameSelected amountToBurn
        change = totalAmountBurnable - valueToBurn
        firstChildAddress = ChildAddressRef walletName 0


    monetaryScript <- getMonetaryPolicyPath tokenPolicyHash >>= whenNothingThrow TryingToBurnTokenWithoutScriptRegistered
    ChildAddress {address} <- fetchById firstChildAddress
    buildAndSubmit
      (Unbalanced (FeeAddressRef firstChildAddress))
      (Just $ CollateralAddressRef firstChildAddress)
      TxBuild
        { inputsFromWallet =  FromWallet walletUTxO :| []
        , inputsFromScript =  Nothing
        , outputs = ToWallet address (change + lovelaceValueOf 1379280) Nothing:| []
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Just $ Burn { amount = valueToBurn, script = monetaryScript} :| []
        , metadataMaybe = Nothing
        , ..}
