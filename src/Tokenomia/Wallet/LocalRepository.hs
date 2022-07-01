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


module Tokenomia.Wallet.LocalRepository
    ( register
    , remove
    , exists
    , restoreByMnemonics
    , fetchAll
    , fetchById
    , Wallet (..)
    ) where

import           Data.String
import qualified Data.ByteString.Lazy.Char8 as C
import           Control.Monad.Reader
import           Shh.Internal
import           System.Directory ( doesDirectoryExist )


import           Tokenomia.Common.Environment

import           Tokenomia.Common.Folder (getFolderPath,Folder (..))
import           Tokenomia.Common.Address

import           Tokenomia.Wallet.Type
import           Tokenomia.Wallet.LocalRepository.Folder 

import           Tokenomia.Wallet.ChildAddress.LocalRepository hiding (fetchById)
load SearchPath ["cat","mkdir","cardano-cli","awk","ls", "rm", "cardano-address","echo", "find" ]


fetchAll ::
    ( MonadIO m
    , MonadReader Environment m )
    => m [Wallet]
fetchAll = do
   walletsPath <- getFolderPath Wallets
   walletNames <- liftIO $ (fmap.fmap) C.unpack (ls walletsPath |> captureWords)
   mapM fetchById walletNames

fetchById ::
    ( MonadIO m
    , MonadReader Environment m )
    => WalletName
    -> m Wallet
fetchById name =
    Wallet name
        <$> (getWalletFilePath name StakeAddressTxt >>= (\path -> Address . C.unpack  <$> liftIO (cat path |> captureTrim)))
        

register ::
    ( MonadIO m
    , MonadReader Environment m )
    => WalletName
    -> m Wallet
register name = do
    let generateWalletFile' = generateWalletFile name
        
    getWalletPath name         >>= \path -> liftIO $ mkdir "-p" path
    getAddressIndexesPath name >>= \path -> liftIO $ mkdir "-p" path
    getChildAddressesPath name >>= \path -> liftIO $ mkdir "-p" path

    generateWalletFile' MnemonicsTxt
    generateWalletFile' RootPrivateKeyTxt
    generateWalletFile' StakePublicKeyTxt
    generateWalletFile' StakeAddressTxt
    deriveChildAddressesWithingRange name 0 10
    fetchById name


restoreByMnemonics ::
    ( MonadIO m
    , MonadReader Environment m )
    => WalletName
    -> [String]
    -> m Wallet
restoreByMnemonics name mnemonics = do
    let generateWalletFile' = generateWalletFile name
    getWalletPath name                  >>= \path -> liftIO $ mkdir "-p" path
    getAddressIndexesPath name          >>= \path -> liftIO $ mkdir "-p" path
    getChildAddressesPath name          >>= \path -> liftIO $ mkdir "-p" path
    
    getWalletFilePath name MnemonicsTxt >>= \path -> liftIO $ echo (unwords  mnemonics) &> (Truncate . fromString) path
    generateWalletFile' RootPrivateKeyTxt
    generateWalletFile' StakePublicKeyTxt
    generateWalletFile' StakeAddressTxt
    deriveChildAddressesWithingRange name 0 10
    fetchById name


generateWalletFile
    :: ( MonadIO m
       , MonadReader Environment m )
    => WalletName -> WalletFile -> m ()
generateWalletFile name fileType =  do
  netWorkTag <- asks (\case
                Testnet {} -> "testnet"
                Mainnet {} -> "mainnet")
  case fileType of
    StakePublicKeyTxt -> do
        rootPrivateKeyPath <- getWalletFilePath name RootPrivateKeyTxt
        getWalletFilePath name StakePublicKeyTxt >>= \path ->
            liftIO $ (cat rootPrivateKeyPath |> cardano_address "key" "child" "1852H/1815H/0H/2/0")
                        |> cardano_address "key" "public" "--with-chain-code"
                        &> (Truncate . fromString) path
    StakeAddressTxt    -> do
        stakePublicKeyTxtPath <- getWalletFilePath name StakePublicKeyTxt
        getWalletFilePath name StakeAddressTxt >>= \path ->
            liftIO $ (cat stakePublicKeyTxtPath |> cardano_address "address" "stake" "--network-tag" netWorkTag)
                        &> (Truncate . fromString) path
    RootPrivateKeyTxt  -> do
        mnemonicsPath <- getWalletFilePath name MnemonicsTxt
        getWalletFilePath name RootPrivateKeyTxt >>= \path ->
                liftIO $ (cat mnemonicsPath |> cardano_address "key" "from-recovery-phrase" "Shelley")
                        &> (Truncate . fromString) path
    MnemonicsTxt ->
        getWalletFilePath name MnemonicsTxt >>= \path ->
            liftIO $ cardano_address "recovery-phrase" "generate" "--size" "24"
                        &> (Truncate . fromString) path


remove ::
    ( MonadIO m
    , MonadReader Environment m )
    => WalletName
    -> m ()
remove walletName = getWalletPath walletName >>= \path -> liftIO $ rm "-rf" path

exists ::
    ( MonadIO m
    , MonadReader Environment m )
    => WalletName
    -> m Bool
exists walletName = getWalletPath walletName >>= liftIO . doesDirectoryExist
