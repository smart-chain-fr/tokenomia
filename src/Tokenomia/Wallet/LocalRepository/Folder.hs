{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Tokenomia.Wallet.LocalRepository.Folder
    ( WalletFile (..)
    , getWalletPath
    , getWalletFilePath
    ) where

import           Control.Monad.Reader

import           Tokenomia.Common.Environment

import           Tokenomia.Common.Folder (getFolderPath,Folder (..))

import           Tokenomia.Wallet.Type


data WalletFile
        = StakePublicKeyTxt
        | StakeAddressTxt
        | RootPrivateKeyTxt
        | MnemonicsTxt

getWalletPath
    :: (MonadIO m, MonadReader Environment m)
    =>  WalletName
    ->  m FilePath
getWalletPath name = (<> name <>"/" ) <$> getFolderPath Wallets

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
