{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.Token.CLAPStyle.Burn (burn) where

import           Prelude hiding ((+),(-),print)
import           PlutusTx.Prelude  (AdditiveSemigroup((+)),AdditiveGroup((-)))

import Control.Monad.Reader ( MonadIO, MonadReader )
import Control.Monad.Except ( MonadError )


import Data.List.NonEmpty ( NonEmpty((:|)) )
import Ledger.Value ( singleton )
import Ledger.Ada ( lovelaceValueOf )

import Tokenomia.Common.Environment ( Environment )
import Tokenomia.Wallet.UTxO as UTxO ( UTxO(..) )
import Tokenomia.Wallet.WalletUTxO ( WalletUTxO(..) )
import Tokenomia.Common.Transacting
    ( buildAndSubmit,
      MonetaryAction(..),
      TxBalance(Unbalanced),
      TxBuild(..),
      TxInFromWallet(FromWallet),
      TxOut(ToWallet) )
import Tokenomia.Script.LocalRepository ( getMonetaryPolicyPath )

import Tokenomia.Wallet.Collateral.Read
    ( fetchWalletsWithCollateral )
import Tokenomia.Wallet.CLI
    ( askToChooseAmongGivenWallets, askUTxOFilterBy )
import Tokenomia.Common.Error
    ( whenNothingThrow,
      whenNullThrow,
      TokenomiaError(..) )
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu (ask)
import Tokenomia.Common.Value ( containingOneToken, getTokenFrom )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
    ( ChildAddressRef(..),
      CollateralAddressRef(..),
      FeeAddressRef(..) )
import Tokenomia.Wallet.Type ( Wallet(..), WalletName )
import Tokenomia.Wallet.ChildAddress.LocalRepository
    ( fetchById,
      ChildAddress(..) )

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
