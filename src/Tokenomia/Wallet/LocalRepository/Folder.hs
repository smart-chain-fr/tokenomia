{-# LANGUAGE DuplicateRecordFields                     #-}
{-# LANGUAGE ExtendedDefaultRules                      #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE FlexibleInstances                         #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures           #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds             #-}


module Tokenomia.Wallet.LocalRepository.Folder
    ( WalletFile(..)
    , getWalletFilePath
    , getWalletPath
    ) where

import Control.Monad.Reader                            ( MonadIO, MonadReader )

import Tokenomia.Common.Environment                    ( Environment )

import Tokenomia.Common.Folder                         ( Folder(..), getFolderPath )

import Tokenomia.Wallet.Type                           ( WalletName )


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
