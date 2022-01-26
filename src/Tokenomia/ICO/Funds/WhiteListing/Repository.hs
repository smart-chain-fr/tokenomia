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
{-# LANGUAGE ScopedTypeVariables #-}


module Tokenomia.ICO.Funds.WhiteListing.Repository
    ( fetchAllWhiteListedInvestorRef
    , fetchPaybackAddressStrict
    , update) where

import Prelude hiding (round,print)
import           Control.Monad.Reader
import           Control.Monad.Except

import           Tokenomia.ICO.Round.Settings
import           Tokenomia.Common.Environment
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.ICO.Funds.Validation.ChildAddress.Types
import qualified Data.List.NonEmpty as NEL
import qualified Data.List as L
import           Tokenomia.Common.Shell.Console
import Data.Aeson
import           Data.String

import           Tokenomia.Common.Address

import           Tokenomia.Wallet.Type
import           Tokenomia.Wallet.LocalRepository.Folder
import           Data.Coerce
import           System.Directory

import Shh
import Tokenomia.Wallet.ChildAddress.LocalRepository
import Tokenomia.ICO.Funds.WhiteListing.Types as W
import qualified Data.ByteString.Lazy.Char8 as C


load SearchPath ["curl","mkdir","echo","cat" ]


fetchAllWhiteListedInvestorRef
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => RoundSettings 
    -> NEL.NonEmpty IndexedAddress
    -> m (NEL.NonEmpty WhiteListedInvestorRef)
fetchAllWhiteListedInvestorRef RoundSettings {kycIntegration = Simulation paybackAddress} activeAddresses =
    return $ WhiteListedInvestorRef paybackAddress <$> activeAddresses
fetchAllWhiteListedInvestorRef settings activeAddresses 
    = mapM (\indexedAddress@IndexedAddress {childAddressRef = ChildAddressRef {..}} -> do 
               paybackAddress  <- fetchPaybackAddressStrict settings index
               return $ WhiteListedInvestorRef paybackAddress indexedAddress) activeAddresses


fetchPaybackAddressStrict
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError  TokenomiaError m)
    => RoundSettings 
    -> ChildAddressIndex
    -> m Address
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
             True ->  Address . C.unpack <$> (liftIO $ cat filePath |> captureTrim)
     

update 
     :: ( MonadIO m
        , MonadReader Environment m
        , MonadError TokenomiaError m)
    => RoundSettings 
    -> m ()  
update settings@RoundSettings {kycIntegration = Integration {..},investorsWallet = Wallet {name}} = do
   let fromIndex = ChildAddressIndex 0
   rawPayload <- liftIO (curl  "-d" (params fromIndex) "-X" "POST" url |> captureTrim) 
   case eitherDecode rawPayload of
           Left e -> do 
               printLn $ "Inconsistent Payload : " <> e
               liftIO $ putStrLn $ show rawPayload
           Right Payload {investors} -> do
               printLn $ "Found " <> show (length investors) <> " Investors with a valid KYC."
               let sortedInvestors = L.sort investors
                   lastInvestorIndex = W.index . L.last $ sortedInvestors 
               lastDerivedAddress <- NEL.last <$> fetchDerivedChildAddressIndexes name
               when (lastDerivedAddress < fromIntegral lastInvestorIndex) $ do
                    printLn $ "Updating Derived Child Addresses [" <> show lastDerivedAddress <> "," <> show lastInvestorIndex <> "]"
                    let range :: [ChildAddressRef] = ChildAddressRef name . ChildAddressIndex <$> [fromIntegral lastDerivedAddress..lastInvestorIndex]
                    mapM_ (\childAddressRef@ChildAddressRef{index = i} -> do
                                deriveChildAddress childAddressRef
                                liftIO $ putStrLn $ " - Derived Child Address " <> (show @Integer . coerce $  i))
                          range 
                          
                         
               mapM_ (saveKYCedInvestors settings) investors

               printLn   "Whitelist updated." 
    
update RoundSettings {kycIntegration = Simulation _} 
    = printLn "Simulation : no necessary update in that case."   
    

saveKYCedInvestors 
     :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => RoundSettings 
    -> Investor 
    -> m ()  
saveKYCedInvestors RoundSettings {investorsWallet = Wallet {name}} Investor {index,paybackAddress,childAddress} = do 
    whitelistFolderPath <- getWhitelistPath name 
    IndexedAddress {childAddressRef = ChildAddressRef {index = indexRetrieved} } <- fetchByAddressStrict name (Address childAddress) 
    when (indexRetrieved /= fromIntegral index ) $ do 
        throwError $ ICOWhitelistingNotValid index (fromIntegral indexRetrieved)
    liftIO $ mkdir "-p" whitelistFolderPath
    filePath <- getInvestorPaybackAddressFilePath name (fromIntegral index)
    liftIO $ echo paybackAddress &> (Truncate . fromString) filePath

getInvestorPaybackAddressFilePath 
    :: ( MonadIO m
       , MonadReader Environment m) 
    => WalletName 
    -> ChildAddressIndex  
    -> m FilePath   
getInvestorPaybackAddressFilePath name (ChildAddressIndex index) = do  
    whitelistFolderPath <- getWhitelistPath name 
    return (whitelistFolderPath <> show index <> ".payback.address")

getWhitelistPath
    :: (MonadIO m, MonadReader Environment m)
    =>  WalletName
    ->  m FilePath
getWhitelistPath walletName
    = (<> "whitelist/") <$> getWalletPath walletName

