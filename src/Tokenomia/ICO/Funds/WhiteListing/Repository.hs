{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.ICO.Funds.WhiteListing.Repository (
  fetchAllWhiteListedInvestorRef,
  fetchPaybackAddressStrict,
  update,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Prelude hiding (round)

import Data.Aeson
import Data.List qualified as L
import Data.List.NonEmpty qualified as NEL
import Data.String
import Tokenomia.Common.Environment
import Tokenomia.Common.Error
import Tokenomia.Common.Shell.Console
import Tokenomia.ICO.Funds.Validation.ChildAddress.Types
import Tokenomia.ICO.Round.Settings
import Tokenomia.Wallet.ChildAddress.ChildAddressRef

import Tokenomia.Common.Address

import Data.Coerce
import System.Directory
import Tokenomia.Wallet.LocalRepository.Folder
import Tokenomia.Wallet.Type

import Data.ByteString.Lazy.Char8 qualified as C
import Shh
import Tokenomia.ICO.Funds.WhiteListing.Types as W
import Tokenomia.Wallet.ChildAddress.LocalRepository

load SearchPath ["curl", "mkdir", "echo", "cat"]

fetchAllWhiteListedInvestorRef ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  NEL.NonEmpty IndexedAddress ->
  m (NEL.NonEmpty WhiteListedInvestorRef)
fetchAllWhiteListedInvestorRef RoundSettings {kycIntegration = Simulation paybackAddress} activeAddresses =
  return $ WhiteListedInvestorRef paybackAddress <$> activeAddresses
fetchAllWhiteListedInvestorRef settings activeAddresses =
  mapM
    ( \indexedAddress@IndexedAddress {childAddressRef = ChildAddressRef {..}} -> do
        paybackAddress <- fetchPaybackAddressStrict settings index
        return $ WhiteListedInvestorRef paybackAddress indexedAddress
    )
    activeAddresses

fetchPaybackAddressStrict ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  ChildAddressIndex ->
  m Address
fetchPaybackAddressStrict RoundSettings {kycIntegration = Simulation paybackAddress} _ = return paybackAddress
fetchPaybackAddressStrict RoundSettings {investorsWallet = Wallet {name}} index = do
  filePath <- getInvestorPaybackAddressFilePath name index
  liftIO (doesFileExist filePath)
    >>= \case
      False -> do
        printLn "<<<<<<<<WARNING>>>>>>"
        printLn "Adress with funds wihtout a valid KYC (need to be ignored)"
        printLn "<<<<<<<<WARNING>>>>>>"
        throwError $ ICOPaybackAddressNotAvailable name (fromIntegral index)
      True -> Address . C.unpack <$> liftIO (cat filePath |> captureTrim)

update ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  m ()
update settings@RoundSettings {kycIntegration = Integration {..}, investorsWallet = Wallet {name}} = do
  let fromIndex = ChildAddressIndex 0
  rawPayload <- liftIO (curl "-d" (params fromIndex) "-X" "POST" url |> captureTrim)
  case eitherDecode rawPayload of
    Left e -> do
      printLn $ "Inconsistent Payload : " <> e
      liftIO $ Prelude.print rawPayload
    Right Payload {investors} -> do
      printLn $ "Found " <> show (length investors) <> " Investors with a valid KYC."
      let sortedInvestors = L.sort investors
          lastInvestorIndex = W.index . L.last $ sortedInvestors
      lastDerivedAddress <- NEL.last <$> fetchDerivedChildAddressIndexes name
      when (lastDerivedAddress < fromIntegral lastInvestorIndex) $ do
        printLn $ "Updating Derived Child Addresses [" <> show lastDerivedAddress <> "," <> show lastInvestorIndex <> "]"
        let range :: [ChildAddressRef] = ChildAddressRef name . ChildAddressIndex <$> [fromIntegral lastDerivedAddress .. lastInvestorIndex]
        mapM_
          ( \childAddressRef@ChildAddressRef {index = i} -> do
              deriveChildAddress childAddressRef
              liftIO $ putStrLn $ " - Derived Child Address " <> (show @Integer . coerce $ i)
          )
          range

      mapM_ (saveKYCedInvestors settings) investors

      printLn "Whitelist updated."
update RoundSettings {kycIntegration = Simulation _} =
  printLn "Simulation : no necessary update in that case."

saveKYCedInvestors ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  Investor ->
  m ()
saveKYCedInvestors RoundSettings {investorsWallet = Wallet {name}} i@Investor {index, paybackAddress, childAddress} = do
  printLn $ "Investor > " <> show i
  whitelistFolderPath <- getWhitelistPath name
  IndexedAddress {childAddressRef = ChildAddressRef {index = indexRetrieved}} <- fetchByAddressStrict name (Address childAddress)
  when (indexRetrieved /= fromIntegral index) $ do
    throwError $ ICOWhitelistingNotValid index (fromIntegral indexRetrieved)
  liftIO $ mkdir "-p" whitelistFolderPath
  filePath <- getInvestorPaybackAddressFilePath name (fromIntegral index)
  liftIO $ echo paybackAddress &> (Truncate . fromString) filePath

getInvestorPaybackAddressFilePath ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  WalletName ->
  ChildAddressIndex ->
  m FilePath
getInvestorPaybackAddressFilePath name (ChildAddressIndex index) = do
  whitelistFolderPath <- getWhitelistPath name
  return (whitelistFolderPath <> show index <> ".payback.address")

getWhitelistPath ::
  (MonadIO m, MonadReader Environment m) =>
  WalletName ->
  m FilePath
getWhitelistPath walletName =
  (<> "whitelist/") <$> getWalletPath walletName
