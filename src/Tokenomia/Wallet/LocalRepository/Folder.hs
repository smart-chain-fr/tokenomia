{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Tokenomia.Wallet.LocalRepository.Folder
    ( WalletFile (..)
    , getWalletPath
    , getWalletFilePath
    ) where

import Control.Monad.Reader ( MonadIO, MonadReader )

import Tokenomia.Common.Environment ( Environment )

import           Tokenomia.Common.Folder (getFolderPath,Folder (..))

import Tokenomia.Wallet.Type ( WalletName )


data WalletFile
        = StakePublicKeyTxt
        | StakeAddressTxt
        | RootPrivateKeyTxt
        | MnemonicsTxt

getWalletPath
    :: (MonadIO m, MonadReader Environment m)
    =>  WalletName
    ->  m FilePath
getWalletPath walletName = (<> walletName <>"/" ) <$> getFolderPath Wallets

getWalletFilePath
    :: (MonadIO m, MonadReader Environment m)
    =>  WalletName
    ->  WalletFile
    ->  m FilePath
getWalletFilePath walletName file
    = case file of
        StakePublicKeyTxt ->  (<> "stake-public-key.txt")
        StakeAddressTxt   ->  (<> "stake-address.txt")
        RootPrivateKeyTxt ->  (<> "root-private-key.txt")
        MnemonicsTxt      ->  (<> "mnemonics.txt")
    <$> getWalletPath walletName
