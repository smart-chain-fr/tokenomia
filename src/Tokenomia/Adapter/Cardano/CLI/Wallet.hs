{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}


module Tokenomia.Adapter.Cardano.CLI.Wallet
    ( register_shelley_wallet
    , remove_shelley_wallet
    , restore_from_seed_phrase
    , query_registered_wallets
    , Wallet (..)
    ) where

import           Data.String
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.UTF8 as BLU 

import           Control.Monad.Reader
import           Shh.Internal

import           Ledger.Crypto

import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Tokenomia.Common.Shell.InteractiveMenu
import           Tokenomia.Common.Shell.Console (printLn)

import           Tokenomia.Adapter.Cardano.CLI.Folder (getFolderPath,Folder (..))
import           Tokenomia.Adapter.Cardano.Types
{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["cat","mkdir","cardano-cli","awk","ls", "rm", "cardano-address" ]


type WalletName = String



data Wallet = Wallet
              { name :: WalletName
              , paymentAddress :: Address
              , paymentSigningKeyPath :: FilePath
              , publicKeyHash :: PubKeyHash  }

instance Show Wallet where
    show Wallet {..} = ">> " <> name
        <> " \n public key hash :" <> show publicKeyHash
        <> " \n payment addr :" <> show paymentAddress

instance DisplayMenuItem Wallet where
    displayMenuItem Wallet {..} = name


query_registered_wallets :: (MonadIO m, MonadReader Environment m) => m [Wallet]
query_registered_wallets = do
   keyPath <- getFolderPath Keys
   walletNames <- liftIO $ (fmap.fmap) C.unpack (ls keyPath |> captureWords)
   mapM (\name ->
        do
        let paymentAddressPath = keyPath <> name <> "/payment.addr"
            paymentSigningKeyPath = keyPath <> name <> "/payment-signing.skey"
            publickeyPath = keyPath <> name <> "/public-key.hash"
        paymentAddress <- liftIO $ Address . C.unpack  <$> (cat paymentAddressPath |> capture)
        publicKeyHash <- liftIO $ fromString . BLU.toString <$> (cat publickeyPath |> captureTrim)
        return $ Wallet {..} ) walletNames


generate_seed_phrase
    :: ( MonadIO m, MonadReader Environment m )
    => WalletName
    -> m ()
generate_seed_phrase walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        mnemonics = walletKeyPath <> "mnemonics.txt"
    liftIO $ mkdir "-p" walletKeyPath
    liftIO $ cardano_address "recovery-phrase" "generate" "--size" "24"
        &> (Truncate . fromString) mnemonics


register_shelley_wallet
    :: ( MonadIO m
       , MonadReader Environment m )
    => WalletName
    -> m ()
register_shelley_wallet walletName = do
    generate_seed_phrase walletName
    generate_keys walletName

restore_from_seed_phrase
    :: ( MonadIO m
       , MonadReader Environment m )
    => WalletName -> String
    -> m ()
restore_from_seed_phrase walletName seedPhrase = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        mnemonics = walletKeyPath <> "mnemonics.txt"
    liftIO $ mkdir "-p" walletKeyPath
    liftIO (printLn seedPhrase
        &> (Truncate . fromString) mnemonics)
    generate_keys walletName

generate_keys
    :: ( MonadIO m
       , MonadReader Environment m )
    => WalletName
    -> m ()
generate_keys walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        mnemonics = walletKeyPath <> "mnemonics.txt"
        root = walletKeyPath <> "root.xsk"
        paymentSigning = walletKeyPath <> "payment-signing.xsk"
        paymentVerification = walletKeyPath <> "payment-verification.xvk"
        stakeVerification = walletKeyPath <> "stake.xvk"
        shortPaymentAddress = walletKeyPath <> "payment.addr"

    liftIO $ mkdir "-p" walletKeyPath

    liftIO $ (cat mnemonics |> cardano_address "key" "from-recovery-phrase" "Shelley")
        &> (Truncate . fromString) root
    liftIO $ (cat root |> cardano_address "key" "child" "1852H/1815H/0H/0/0")
        &> (Truncate . fromString) paymentSigning
    liftIO $ (cat root |> cardano_address "key" "child" "1852H/1815H/0H/0/0") |> cardano_address "key" "public" "--with-chain-code"
        &> (Truncate . fromString) paymentVerification
    liftIO $ (cat root |> cardano_address "key" "child" "1852H/1815H/0H/2/0") |> cardano_address "key" "public" "--with-chain-code"
        &> (Truncate . fromString) stakeVerification

    netWorkTag <- asks (\case
                        Testnet {} -> "testnet"
                        Mainnet {} -> "mainnet")
    liftIO $ (cat paymentVerification |> cardano_address "address" "payment" "--network-tag" netWorkTag)
        &> (Truncate . fromString) shortPaymentAddress
    convertKeys walletName

convertKeys
    :: ( MonadIO m, MonadReader Environment m )
    => WalletName
    -> m ()
convertKeys walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
        paymentSigningConvertedPath = walletKeyPath <> "payment-signing.skey"
        paymentSigningPath = walletKeyPath <> "payment-signing.xsk"
        paymentVerificationConvertedPath = walletKeyPath <> "payment-verification.vkey"
        publickeyPath = walletKeyPath <> "public-key.hash"

    liftIO $ cardano_cli "key" "convert-cardano-address-key" "--shelley-payment-key" "--signing-key-file" paymentSigningPath "--out-file" paymentSigningConvertedPath
    liftIO $ cardano_cli "key" "verification-key" "--signing-key-file" paymentSigningConvertedPath "--verification-key-file" paymentVerificationConvertedPath
    liftIO $ cardano_cli "address" "key-hash" "--payment-verification-key-file" paymentVerificationConvertedPath &> (Truncate . fromString) publickeyPath

remove_shelley_wallet :: ( MonadIO m, MonadReader Environment m) =>WalletName -> m ()
remove_shelley_wallet walletName = do
    keyPath <- getFolderPath Keys
    let walletKeyPath = keyPath <> walletName <> "/"
    liftIO $ rm "-rf" walletKeyPath

