{-# LANGUAGE DuplicateRecordFields                     #-}
{-# LANGUAGE ExtendedDefaultRules                      #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE NamedFieldPuns                            #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
{-# LANGUAGE TypeApplications                          #-}

module Tokenomia.Token.CLAPStyle.Mint
    ( mint
    , mint'
    ) where

import PlutusTx.Prelude                                ( AdditiveSemigroup((+)) )
import Prelude hiding                                  ( print, (+) )

import Control.Monad.Reader                            ( MonadIO, MonadReader )

import Data.List.NonEmpty                              ( NonEmpty((:|)) )
import Data.Text qualified as Text
import Data.Text.Encoding qualified as E

import Control.Monad.Except                            ( MonadError )

import Ledger                                          ( CurrencySymbol, TokenName )
import Ledger.Ada                                      ( lovelaceValueOf )
import Ledger.Value qualified as L
import Plutus.Script.Utils.V1.Scripts                  ( scriptCurrencySymbol )

import Tokenomia.Common.Environment                    ( Environment )
import Tokenomia.Token.CLAPStyle.MonetaryPolicy        ( Params(..), mkMonetaryPolicyScript )
import Tokenomia.Wallet.CLI
    ( askToChooseAmongGivenWallets
    , selectBiggestStrictlyADAsNotCollateral
    )

import Tokenomia.Common.Transacting
    ( MonetaryAction(Mint, amount, script)
    , TxBalance(Unbalanced)
    , TxBuild(..)
    , TxInFromWallet(FromWallet)
    , TxOut(ToWallet)
    , buildAndSubmit
    )
import Tokenomia.Wallet.UTxO                           ( UTxO(txOutRef) )
import Tokenomia.Wallet.WalletUTxO                     ( WalletUTxO(utxo) )

import Tokenomia.Common.Error
    ( TokenomiaError(NoADAsOnChildAddress, NoWalletWithCollateral)
    , whenNothingThrow
    , whenNullThrow
    )
import Tokenomia.Script.LocalRepository                ( registerMintingScriptFile )
import Tokenomia.Wallet.Collateral.Read                ( fetchWalletsWithCollateral )

import Tokenomia.Common.Shell.Console                  ( printLn )
import Tokenomia.Common.Shell.InteractiveMenu          ( ask, askString )

import Tokenomia.Wallet.ChildAddress.ChildAddressRef
    ( ChildAddressRef(ChildAddressRef)
    , CollateralAddressRef(CollateralAddressRef)
    , FeeAddressRef(FeeAddressRef)
    )
import Tokenomia.Wallet.ChildAddress.LocalRepository   ( ChildAddress(..), Wallet(..), fetchById )
import Tokenomia.Wallet.Type                           ( WalletName )

mint ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
    => m ()
mint = do
    Wallet{name} <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
        >>= \wallets -> do
            printLn "Select the minter wallet : "
            askToChooseAmongGivenWallets wallets
    tokenNameToMint  <- fromText . Text.pack <$> askString "> Token Name : "
    amountToMint     <- ask @Integer "> Total Supply to Mint : "
    _ <- mint' name tokenNameToMint amountToMint
    return ()
  where
        fromText :: Text.Text -> TokenName
        fromText = L.tokenName . E.encodeUtf8


mint' ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
    => WalletName
    -> TokenName
    -> Integer
    -> m CurrencySymbol
mint' walletName tokenName amount = do
    let firstChildAddress = ChildAddressRef walletName 0
    walletUTxOToConsume <- selectBiggestStrictlyADAsNotCollateral firstChildAddress >>= whenNothingThrow NoADAsOnChildAddress
    ChildAddress {address} <- fetchById firstChildAddress
    let monetaryPolicy = mkMonetaryPolicyScript Params { txOutRefToConsume = txOutRef . utxo $ walletUTxOToConsume , .. }
        policyhash = scriptCurrencySymbol monetaryPolicy
        valueToMint = L.singleton policyhash tokenName amount

    printLn   "-------------------------"
    printLn $ "Policy hash will be : " <> show policyhash
    printLn   "-------------------------"

    monetaryScript <- registerMintingScriptFile monetaryPolicy

    buildAndSubmit
      (Unbalanced (FeeAddressRef firstChildAddress))
      (Just $ CollateralAddressRef firstChildAddress)
      TxBuild
        { inputsFromWallet =  FromWallet walletUTxOToConsume :| []
        , inputsFromScript = Nothing
        , outputs = ToWallet address (valueToMint + lovelaceValueOf 1379280 ) Nothing :| []
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Just $ Mint { amount = valueToMint, script = monetaryScript} :| []
        , metadataMaybe = Nothing
        , ..}
    return policyhash
