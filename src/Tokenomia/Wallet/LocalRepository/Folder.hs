{-# LANGUAGE DuplicateRecordFields #-}

module Tokenomia.Wallet.LocalRepository.Folder (
  WalletFile (..),
  getWalletPath,
  getWalletFilePath,
) where

import Control.Monad.Reader

import Tokenomia.Common.Environment

import Tokenomia.Common.Folder (Folder (..), getFolderPath)

import Tokenomia.Wallet.Type

data WalletFile
  = StakePublicKeyTxt
  | StakeAddressTxt
  | RootPrivateKeyTxt
  | MnemonicsTxt

getWalletPath ::
  (MonadIO m, MonadReader Environment m) =>
  WalletName ->
  m FilePath
getWalletPath name = (<> name <> "/") <$> getFolderPath Wallets

getWalletFilePath ::
  (MonadIO m, MonadReader Environment m) =>
  WalletName ->
  WalletFile ->
  m FilePath
getWalletFilePath walletName file =
  case file of
    StakePublicKeyTxt -> (<> "stake-public-key.txt")
    StakeAddressTxt -> (<> "stake-address.txt")
    RootPrivateKeyTxt -> (<> "root-private-key.txt")
    MnemonicsTxt -> (<> "mnemonics.txt")
    <$> getWalletPath walletName
