{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}


module Tokenomia.Wallet.Collateral.Write
    ( createCollateral
    , createCollateral'
    ) where

import           Control.Monad.Reader


import           Data.Maybe
import           Data.List.NonEmpty
import           Control.Monad.Except

import           Ledger.Ada

import           Tokenomia.Common.Transacting
import           Tokenomia.Common.Environment

import           Tokenomia.Wallet.CLI as Wallet
import           Tokenomia.Wallet.LocalRepository hiding (fetchById)

import           Tokenomia.Common.Error
import           Tokenomia.Common.Shell.Console (printLn)
import           Prelude hiding (print)
import           Tokenomia.Wallet.Collateral.Read
import           Tokenomia.Wallet.Type as Wallet
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Wallet.ChildAddress.LocalRepository

createCollateral ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m )
    => m ()
createCollateral = do
    target <- fetchAll
                >>= whenNullThrow    NoWalletRegistered
                >>= filterWalletsWithCollateral  >>= whenNothingThrow NoWalletWithoutCollateral
                >>= \wallets -> do
                    printLn "Select the wallet to receive the collateral"
                    askToChooseAmongGivenWallets wallets

    source <- fetchAll
                >>= whenNullThrow    NoWalletRegistered
                >>= \wallets -> do
                        printLn "Select the source wallet containing ADAs "
                        askToChooseAmongGivenWallets wallets

    createCollateral' (Wallet.name source) (Wallet.name target)

createCollateral'
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
       => WalletName
       -> WalletName
       -> m ()
createCollateral' sourceWalletName targetWalletName = do
    let firstAddressSource = ChildAddressRef sourceWalletName 0
        firstAddressTarget = ChildAddressRef targetWalletName 0
    assertCollateralNotAlreadyCreated firstAddressTarget
    ada <- selectBiggestStrictlyADAsNotCollateral firstAddressSource >>= whenNothingThrow NoADAsOnChildAddress
    ChildAddress {address = senderAddr} <- fetchById firstAddressTarget
    buildAndSubmit
      (Unbalanced (FeeAddressRef firstAddressSource ))
      Nothing
      TxBuild
        { inputsFromWallet =  FromWallet ada :| []
        , inputsFromScript = Nothing
        , outputs = ToWallet senderAddr (adaValueOf 2.0) Nothing :| []
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , metadataMaybe = Nothing }

assertCollateralNotAlreadyCreated ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m )
    => ChildAddressRef
    -> m ()
assertCollateralNotAlreadyCreated childAddressRef = fetchCollateral childAddressRef >>=  whenSomethingThrow (const AlreadyACollateral)
