module Tokenomia.Wallet.Collateral.Write (
  createCollateral,
  createCollateral',
) where

import Control.Monad.Reader

import Control.Monad.Except
import Data.List.NonEmpty
import Data.Maybe

import Ledger.Ada

import Tokenomia.Common.Environment
import Tokenomia.Common.Transacting (TxBalance (Unbalanced), TxBuild (..), TxInFromWallet (..), TxOut (ToWallet), buildAndSubmit)

import Tokenomia.Wallet.CLI as Wallet
import Tokenomia.Wallet.LocalRepository hiding (fetchById)

import Tokenomia.Common.Error
import Tokenomia.Common.Shell.Console (printLn)
import Tokenomia.Wallet.ChildAddress.ChildAddressRef (ChildAddressRef (ChildAddressRef), FeeAddressRef (FeeAddressRef))
import Tokenomia.Wallet.ChildAddress.LocalRepository (ChildAddress (ChildAddress, address), fetchById)
import Tokenomia.Wallet.Collateral.Read
import Tokenomia.Wallet.Type as Wallet
import Prelude hiding (print)

createCollateral ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  m ()
createCollateral = do
  target <-
    fetchAll
      >>= whenNullThrow NoWalletRegistered
      >>= filterWalletsWithCollateral
      >>= whenNothingThrow NoWalletWithoutCollateral
      >>= \wallets -> do
        printLn "Select the wallet to receive the collateral"
        askToChooseAmongGivenWallets wallets

  source <-
    fetchAll
      >>= whenNullThrow NoWalletRegistered
      >>= \wallets -> do
        printLn "Select the source wallet containing ADAs "
        askToChooseAmongGivenWallets wallets

  createCollateral' (Wallet.name source) (Wallet.name target)

createCollateral' ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  WalletName ->
  WalletName ->
  m ()
createCollateral' sourceWalletName targetWalletName = do
  let firstAddressSource = ChildAddressRef sourceWalletName 0
      firstAddressTarget = ChildAddressRef targetWalletName 0
  assertCollateralNotAlreadyCreated firstAddressTarget
  ada <- selectBiggestStrictlyADAsNotCollateral firstAddressSource >>= whenNothingThrow NoADAsOnChildAddress
  ChildAddress {address = senderAddr} <- fetchById firstAddressTarget
  buildAndSubmit
    (Unbalanced (FeeAddressRef firstAddressSource))
    Nothing
    TxBuild
      { inputsFromWallet = FromWallet ada :| []
      , inputsFromScript = Nothing
      , outputs = ToWallet senderAddr (adaValueOf 2.0) Nothing :| []
      , validitySlotRangeMaybe = Nothing
      , tokenSupplyChangesMaybe = Nothing
      , metadataMaybe = Nothing
      }

assertCollateralNotAlreadyCreated ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  ChildAddressRef ->
  m ()
assertCollateralNotAlreadyCreated childAddressRef = fetchCollateral childAddressRef >>= whenSomethingThrow (const AlreadyACollateral)
