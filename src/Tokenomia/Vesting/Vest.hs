{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tokenomia.Vesting.Vest (vestFunds) where

import PlutusTx.Prelude (AdditiveGroup ((-)), AdditiveSemigroup ((+)))
import Prelude hiding (print, (+), (-))
import Prelude qualified as P

import Control.Monad.Except
import Control.Monad.Reader hiding (ask)
import Data.List.NonEmpty
import Data.Time.Clock.POSIX qualified as POSIX

import Ledger.Value
import Plutus.V1.Ledger.Ada

import Tokenomia.Common.Datum qualified as Script
import Tokenomia.Script.LocalRepository qualified as Script

import Tokenomia.Wallet.UTxO as Wallet
import Tokenomia.Wallet.WalletUTxO

import Tokenomia.Wallet.LocalRepository hiding (fetchById)

import Tokenomia.Common.Environment
import Tokenomia.Common.Transacting
import Tokenomia.Vesting.Contract

import Tokenomia.Vesting.Repository qualified as Vesting.Repository

import Tokenomia.Common.Error
import Tokenomia.Common.Shell.Console
import Tokenomia.Common.Shell.InteractiveMenu
import Tokenomia.Common.Value
import Tokenomia.Wallet.CLI
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.ChildAddress.LocalRepository
import Tokenomia.Wallet.Collateral.Read
import Tokenomia.Wallet.Type

vestFunds ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  m ()
vestFunds = do
  Wallet {name = ownerWalletName} <-
    fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
      >>= \wallets -> do
        printLn "Select the token owner wallet :"
        askToChooseAmongGivenWallets wallets
  utxosWithOneToken <- fetchUTxOFilterBy (containingOneToken . Wallet.value . utxo) (ChildAddressRef ownerWalletName 0) >>= whenNothingThrow NoUTxOWithOnlyOneToken
  printLn "- Select the utxo and associated tokens to vest  :"
  utxoWithToken <- askToChooseAmongGivenUTxOs utxosWithOneToken

  printLn "- First Tranche :"
  nbSecondsTranche1 <- ask @Integer "> How many seconds will you vest the tokens ? : "
  nbTokenTranche1 <- ask @Integer "> How many tokens will you vest ? : "

  printLn "- Second Tranche :"
  nbSecondsTranche2 <- ask @Integer "> How many seconds will you vest the tokens ? : "
  nbTokenTranche2 <- ask @Integer "> How many tokens will you vest ? : "

  printLn "- Select the investor's wallet :"

  Wallet {name = investorWalletName} <- askAmongAllWallets >>= whenNothingThrow NoWalletRegistered
  vestFunds'
    ownerWalletName
    investorWalletName
    utxoWithToken
    (nbSecondsTranche1, nbTokenTranche1)
    (nbSecondsTranche2, nbTokenTranche2)

vestFunds' ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  WalletName ->
  WalletName ->
  WalletUTxO ->
  (Integer, Integer) ->
  (Integer, Integer) ->
  m ()
vestFunds'
  ownerWalletName
  investorWalletName
  utxoWithTokens
  (nbSecondsTranche1, nbTokenTranche1)
  (nbSecondsTranche2, nbTokenTranche2) = do
    let (cu, tn, totalAmountToken) = getTokenFrom . Wallet.value . utxo $ utxoWithTokens
        valueTotalToken = singleton cu tn totalAmountToken
        valueTokenTranche1 = singleton cu tn nbTokenTranche1
        valueTokenTranche2 = singleton cu tn nbTokenTranche2
        valueToBeVested = valueTokenTranche1 + valueTokenTranche2
        changeBackToOwner = valueTotalToken - valueToBeVested

    now <- liftIO POSIX.getPOSIXTime
    ChildAddress {publicKeyHash = investorPublicKeyHash} <- fetchById $ ChildAddressRef investorWalletName 0
    ChildAddress {address = ownerAddr} <- fetchById $ ChildAddressRef ownerWalletName 0
    let params =
          VestingParams
            { vestingTranche1 =
                VestingTranche
                  { vestingTrancheDate = convertToInternalPosix (now P.+ fromIntegral nbSecondsTranche1)
                  , vestingTrancheAmount = valueTokenTranche1 + lovelaceValueOf 1689618
                  }
            , vestingTranche2 =
                VestingTranche
                  { vestingTrancheDate = convertToInternalPosix (now P.+ fromIntegral nbSecondsTranche2)
                  , vestingTrancheAmount = valueTokenTranche2 + lovelaceValueOf 1689618
                  }
            , vestingOwner = investorPublicKeyHash
            }

    let vscript = vestingScript params
    scriptLocation <- Script.registerValidatorScriptFile vscript

    datumVoidHash <- Script.getDataHash ()

    buildAndSubmit
      (Unbalanced (FeeAddressRef $ ChildAddressRef ownerWalletName 0))
      (Just $ CollateralAddressRef $ ChildAddressRef ownerWalletName 0)
      TxBuild
        { inputsFromWallet = FromWallet utxoWithTokens :| []
        , inputsFromScript = Nothing
        , outputs =
            ToWallet ownerAddr (changeBackToOwner + lovelaceValueOf 1379280) Nothing
              :| [ ToScript
                    { address = Script.onChain scriptLocation
                    , value = (vestingTrancheAmount . vestingTranche1) params
                    , datumHash = datumVoidHash
                    }
                 , ToScript
                    { address = Script.onChain scriptLocation
                    , value = (vestingTrancheAmount . vestingTranche2) params
                    , datumHash = datumVoidHash
                    }
                 ]
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , metadataMaybe = Nothing
        , ..
        }

    printLn "-----------------------------------------"
    printLn "- Investor can retrieve the funds from :"
    printLn $ "\t - Tranche 1 after " <> (formatISO8601 . convertToExternalPosix . vestingTrancheDate . vestingTranche1) params
    printLn $ "\t - Tranche 2 after " <> (formatISO8601 . convertToExternalPosix . vestingTrancheDate . vestingTranche2) params
    printLn $ "- Funds location : " <> show (Script.onChain scriptLocation)
    printLn "-----------------------------------------"

    Vesting.Repository.register params
