{-# LANGUAGE DuplicateRecordFields                     #-}
{-# LANGUAGE ExtendedDefaultRules                      #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE FlexibleInstances                         #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE NamedFieldPuns                            #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
{-# LANGUAGE TemplateHaskell                           #-}
{-# LANGUAGE TupleSections                             #-}
{-# LANGUAGE TypeApplications                          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures           #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds             #-}


module Tokenomia.Wallet.LocalRepository
    ( exists
    , fetchAll
    , fetchById
    , register
    , remove
    , restoreByMnemonics
    ) where

import Control.Monad.Reader                            ( MonadIO(..), MonadReader, asks )
import Data.ByteString.Lazy.Char8 qualified as C
import Data.String                                     ( IsString(fromString) )
import Shh.Internal
    ( ExecReference(SearchPath)
    , Stream(Truncate)
    , captureTrim
    , captureWords
    , load
    , (&>)
    , (|>)
    )
import System.Directory                                ( doesDirectoryExist )


import Tokenomia.Common.Environment                    ( Environment(Mainnet, Testnet) )

import Tokenomia.Common.Address                        ( Address(Address) )
import Tokenomia.Common.Folder                         ( Folder(..), getFolderPath )

import Tokenomia.Wallet.LocalRepository.Folder         ( WalletFile(..), getWalletFilePath, getWalletPath )
import Tokenomia.Wallet.Type                           ( Wallet(Wallet), WalletName )

import Tokenomia.Wallet.ChildAddress.LocalRepository
    ( deriveChildAddressesWithingRange
    , getAddressIndexesPath
    , getChildAddressesPath
    )

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
