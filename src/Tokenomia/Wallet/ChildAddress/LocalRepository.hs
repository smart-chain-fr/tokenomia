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


module Tokenomia.Wallet.ChildAddress.LocalRepository
    ( fetchById
    , fetchByWallet
    , fetchByWalletWithinIndexRange
    , fetchDerivedChildAddressIndexes
    , deriveChildAddressesWithingRange
    , deriveChildAddress
    , getChildAddressPath
    , getChildAddressesPath
    , getAddressIndexesPath
    , Wallet (..)
    , ChildAddress (..)
    , fetchByAddresses
    , toIndexedAddress
    , fetchByWalletIndexedAddress
    , retrieveAddressesFromWallet
    ) where

import           Data.String
import           Data.List.Split (splitOn)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.UTF8 as BLU
import           Data.Set.NonEmpty as NES
import qualified Data.Set as Set
import qualified Data.List.NonEmpty as NEL
import           Control.Monad.Reader
import           Shh.Internal

import           Ledger.Crypto

import           Tokenomia.Common.Environment

import           Tokenomia.Common.Address

import           Tokenomia.Wallet.Type
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Wallet.LocalRepository.Folder
import           Data.Coerce
import           System.Directory
import           Tokenomia.Common.Error
import           Control.Monad.Except

import           Data.Maybe

load SearchPath ["cat","mkdir","cardano-cli","awk","ls", "rm", "cardano-address","echo", "find" ]

data ChildAddress = ChildAddress
              { childAddressRef :: ChildAddressRef
              , address :: Address
              , extendedPrivateKeyJSONPath :: FilePath
              , publicKeyHash :: PubKeyHash  } deriving Eq


toIndexedAddress :: ChildAddress -> IndexedAddress
toIndexedAddress ChildAddress {..} = IndexedAddress {..}

instance Ord ChildAddress where
    compare ChildAddress {childAddressRef = x} ChildAddress {childAddressRef = y} = compare x y

data ChildAddressFile
        = AddressTxt
        | ExtendedPublicKeyJSON
        | ExtendedPublicKeyTxt
        | ExtendedPrivateKeyJSON
        | ExtendedPrivateKeyTxt
        | PubKeyHashTxt

getChildAddressPath
    :: (MonadIO m, MonadReader Environment m)
    =>  ChildAddressRef
    ->  m FilePath
getChildAddressPath ChildAddressRef {index = (ChildAddressIndex index),name}
    = (<> show index <> "/") <$> getChildAddressesPath name

getChildAddressesPath
    :: (MonadIO m, MonadReader Environment m)
    =>  WalletName
    ->  m FilePath
getChildAddressesPath walletName
    = (<> "child-addresses/") <$> getWalletPath walletName

getAddressIndexesPath
    :: (MonadIO m, MonadReader Environment m)
    =>  WalletName
    ->  m FilePath
getAddressIndexesPath walletName
    = (<> "indexes/") <$> getWalletPath walletName

getAddressIndexPath
    :: (MonadIO m, MonadReader Environment m)
    =>  WalletName
    ->  Address
    ->  m FilePath
getAddressIndexPath walletName (Address addr) =
   (<> addr <> ".index") <$> getAddressIndexesPath walletName


getChildAddressFilePath
    :: (MonadIO m, MonadReader Environment m)
    =>  ChildAddressRef
    ->  ChildAddressFile
    ->  m FilePath
getChildAddressFilePath childAddressRef file
    = case file of
        AddressTxt              -> (<> "address.txt")
        ExtendedPublicKeyJSON   -> (<> "extended-public-key.json")
        ExtendedPublicKeyTxt    -> (<> "extended-public-key.txt")
        ExtendedPrivateKeyJSON  -> (<> "extended-private-key.json")
        ExtendedPrivateKeyTxt   -> (<> "extended-private-key.txt")
        PubKeyHashTxt           -> (<> "public-key-hash.txt")
     <$> getChildAddressPath childAddressRef


fetchByAddresses
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError  TokenomiaError m)
    => WalletName
    -> NEL.NonEmpty Address
    -> m (NEL.NonEmpty IndexedAddress)
fetchByAddresses walletName = mapM (fetchByAddressStrict walletName)

fetchByAddressStrict
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError  TokenomiaError m)
    => WalletName
    -> Address
    -> m IndexedAddress
fetchByAddressStrict walletName address = 
    fetchByAddress walletName address 
    >>= whenNothingThrow (ChildAddressNotIndexed walletName address)


retrieveAddressesFromWallet
    :: ( MonadIO m
       , MonadReader Environment m)
    => WalletName
    -> NEL.NonEmpty Address
    -> m (Maybe (NEL.NonEmpty Address))
retrieveAddressesFromWallet walletName addresses = do
    maybeAddresses  <- mapM (\address ->  
        fetchByAddress walletName address
         >>= (\case
                Just _ -> (return . Just) address
                Nothing -> return Nothing) ) addresses
    (return . NEL.nonEmpty . catMaybes . NEL.toList) maybeAddresses

fetchByAddress
    :: ( MonadIO m
       , MonadReader Environment m)
    => WalletName
    -> Address
    -> m (Maybe IndexedAddress)
fetchByAddress walletName address = do
    addressIndexPath <- getAddressIndexPath walletName address
    (liftIO $ doesFileExist addressIndexPath)
     >>= \case
            False -> return Nothing 
            True  -> do
                index <- read @Integer . C.unpack <$> (liftIO $ cat addressIndexPath |> captureTrim)
                return . Just $ IndexedAddress
                    { address = address
                    , childAddressRef = ChildAddressRef walletName (coerce index)}



fetchByWalletIndexedAddress
    :: ( MonadIO m
       , MonadReader Environment m 
       , MonadError  TokenomiaError m)
    => WalletName
    -> m (NEL.NonEmpty IndexedAddress  )
fetchByWalletIndexedAddress a = (fmap . fmap) toIndexedAddress (toAscList <$> fetchByWallet a) 


fetchDerivedChildAddressIndexes 
    :: ( MonadIO m
       , MonadReader Environment m 
       , MonadError  TokenomiaError m)
    => WalletName
    -> m (NEL.NonEmpty ChildAddressIndex)
fetchDerivedChildAddressIndexes  name = do 
    childAddressesPath <- getChildAddressesPath name
    liftIO $
        (fmap.fmap.fmap) (ChildAddressIndex . read @Integer . last . splitOn "/" . C.unpack)
        NEL.nonEmpty . tail <$>  -- remove ./    
        (find childAddressesPath "-type" "d" |> captureWords) -- return ./ ./0 ./1
    >>= \case
        Nothing -> throwError NoDerivedChildAddress
        Just indexes -> return $ NEL.sort indexes


fetchByWalletWithinIndexRange
    :: ( MonadIO m
       , MonadReader Environment m 
       , MonadError  TokenomiaError m)
    => Int 
    -> Int
    -> WalletName
    -> m (Set.Set ChildAddress)
fetchByWalletWithinIndexRange from to name = do
    indexes <- Prelude.take (to - from + 1) . NEL.drop from <$> fetchDerivedChildAddressIndexes name
    Set.fromList <$> mapM fetchById (ChildAddressRef name <$> indexes)
    
fetchByWallet
    :: ( MonadIO m
       , MonadReader Environment m 
       , MonadError  TokenomiaError m)
    => WalletName
    -> m (NESet ChildAddress)
fetchByWallet name = do
    fetchDerivedChildAddressIndexes name 
    >>= \indexes -> fromList <$> mapM fetchById (ChildAddressRef name <$> indexes)


fetchById
    :: ( MonadIO m
       , MonadReader Environment m )
    => ChildAddressRef
    -> m ChildAddress
fetchById childAddressRef = do
    let getPath = getChildAddressFilePath childAddressRef
    address       <- getPath AddressTxt
                        >>= \path -> Address . C.unpack  <$> liftIO (cat path |> captureTrim)
    publicKeyHash <- getPath PubKeyHashTxt
                        >>= \path -> fromString . BLU.toString <$> liftIO (cat path |> captureTrim)
    extendedPrivateKeyJSONPath <- getPath ExtendedPrivateKeyJSON
    return ChildAddress {..}


deriveChildAddressesWithingRange ::
    ( MonadIO m
    , MonadReader Environment m )
    => WalletName
    -> ChildAddressIndex
    -> ChildAddressIndex
    -> m ()
deriveChildAddressesWithingRange walletName from to = do
    mapM_ deriveChildAddress $ ChildAddressRef walletName  <$> [from..to]


deriveChildAddress
    :: ( MonadIO m
       , MonadReader Environment m )
    => ChildAddressRef ->  m ()
deriveChildAddress childAddressRef@ChildAddressRef{name,index = ChildAddressIndex indexInt } = do
   getChildAddressPath childAddressRef >>= \path -> liftIO $ mkdir "-p" path
   let generate = generateChildAddressFile childAddressRef
   generate  ExtendedPrivateKeyTxt
   generate  ExtendedPrivateKeyJSON
   generate  ExtendedPublicKeyTxt
   generate  ExtendedPublicKeyJSON
   generate  AddressTxt
   generate  PubKeyHashTxt

   let getPath = getChildAddressFilePath childAddressRef
   address <- getPath AddressTxt
            >>= \path -> Address . C.unpack  <$> liftIO (cat path |> captureTrim)
   addressIndexPath <- getAddressIndexPath name address
   liftIO $ echo indexInt &> (Truncate . fromString) addressIndexPath

generateChildAddressFile
    :: ( MonadIO m
       , MonadReader Environment m )
    => ChildAddressRef ->  ChildAddressFile -> m ()
generateChildAddressFile childAddressRef@ChildAddressRef{name,index = (ChildAddressIndex indexInteger)} fileType = do
  let getFilePath = getChildAddressFilePath childAddressRef
  netWorkTag <- asks (\case
                    Testnet {} -> "testnet"
                    Mainnet {} -> "mainnet")

  case fileType of
    AddressTxt -> getFilePath AddressTxt >>= \path -> do
        extendedPublicKeyTxtPath <- getFilePath ExtendedPublicKeyTxt
        stakePublicKey <- getWalletFilePath name StakePublicKeyTxt >>= \stakeKeyPath -> C.unpack <$> liftIO (cat stakeKeyPath |> capture)
        liftIO $ (cat extendedPublicKeyTxtPath
                    |> cardano_address "address" "payment" "--network-tag" netWorkTag
                    |> cardano_address "address" "delegation" stakePublicKey)
                &> (Truncate . fromString) path
    ExtendedPublicKeyJSON -> getFilePath ExtendedPublicKeyJSON >>= \path -> do
        extendedPrivateKeyJSONPath <- getFilePath ExtendedPrivateKeyJSON
        liftIO $ cardano_cli
                    "key" "verification-key"
                    "--signing-key-file" extendedPrivateKeyJSONPath
                    "--verification-key-file" path
    ExtendedPublicKeyTxt -> getFilePath ExtendedPublicKeyTxt >>= \path -> do
        extendedPrivateKeyTxtPath <- getFilePath ExtendedPrivateKeyTxt
        liftIO $ (cat extendedPrivateKeyTxtPath |> cardano_address "key" "public" "--with-chain-code")
                &> (Truncate . fromString) path
    ExtendedPrivateKeyJSON -> getFilePath ExtendedPrivateKeyJSON >>= \path -> do
        extendedPrivateKeyTxtPath <- getFilePath ExtendedPrivateKeyTxt
        liftIO $ cardano_cli
                    "key"
                    "convert-cardano-address-key"
                    "--shelley-payment-key"
                    "--signing-key-file" extendedPrivateKeyTxtPath
                    "--out-file" path
    ExtendedPrivateKeyTxt -> getFilePath ExtendedPrivateKeyTxt >>= \path -> do
        let derivationPath = "1852H/1815H/0H/0/" <> show indexInteger
        rootPrivateKeyTxtPath <- getWalletFilePath name RootPrivateKeyTxt
        liftIO $ (cat rootPrivateKeyTxtPath
                    |> cardano_address "key" "child" derivationPath)
                &> (Truncate . fromString) path
    PubKeyHashTxt -> getFilePath PubKeyHashTxt >>= \path -> do
        extendedPublicKeyJSONpATH <- getFilePath ExtendedPublicKeyJSON
        liftIO $ cardano_cli
                    "address"
                    "key-hash"
                    "--payment-verification-key-file" extendedPublicKeyJSONpATH
                    &> (Truncate . fromString) path

