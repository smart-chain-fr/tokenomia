{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
module Tokenomia.ICO.LocalRepository
    ( askRoundSettings
    ) where

import           Prelude hiding (round,print)

import           Plutus.V1.Ledger.Ada
import           Plutus.V1.Ledger.Value
import           Plutus.V1.Ledger.Interval
import           Data.List.NonEmpty
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Control.Monad.Reader
import           Tokenomia.Common.Environment
import           Tokenomia.Wallet.ChildAddress.LocalRepository as ChildAddress
import           Tokenomia.ICO.Round.Settings
import           Tokenomia.Wallet.LocalRepository as Wallet
import Tokenomia.Common.Shell.InteractiveMenu
    ( askMenu, DisplayMenuItem(..) )
import           Tokenomia.Common.Shell.Console (printLn)

data ICO
    = ICO
        { projectName :: String
        , rounds :: NonEmpty Round}

instance DisplayMenuItem ICO where
    displayMenuItem ICO {..} = projectName

data Round = Round { title :: String , settings :: RoundSettings  }

instance DisplayMenuItem Round where
    displayMenuItem Round {..} = title


askRoundSettings
    :: ( MonadIO m
       , MonadReader Environment m)
    => m RoundSettings
askRoundSettings  = do
    printLn "Select Your ICO :"
    ICO {..} <- getICOs >>= askMenu
    printLn "Select Your Round :"
    settings <$> askMenu rounds


getICOs
    :: ( MonadIO m
       , MonadReader Environment m)
    => m (NonEmpty ICO)
getICOs = do
    ask >>= \case 
        Testnet {} -> error "no testnet..."
        Mainnet {} ->  do   
            flashSale      <- getFlashSaleSettings
            publicSale     <- getPublicSaleSettings
            return $ ICO
                    { projectName = "CardaShift (Mainnet with CLAP !!!)"
                    , rounds = flashSale :| [publicSale]} 
                    :| []


getFlashSaleSettings
    :: ( MonadIO m
       , MonadReader Environment m)
       => m Round
getFlashSaleSettings  = do

    Wallet{name= feesWalletName} <- Wallet.fetchById "Cardashift.Fees"
    fees <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef feesWalletName 0)

    Wallet{name= exchangeWalletName} <- Wallet.fetchById "Cardashift.Flash.Sale.Exchange"
    exchange <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef exchangeWalletName 0)

    investorsWallet <- Wallet.fetchById "Cardashift.Flash.Sale.Investors"

    Wallet{name= tokenWalletName} <- Wallet.fetchById "Cardashift.Flash.Sale.Tokens"
    tokens <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef tokenWalletName 0)

    Wallet{name= nextExchangeWalletName} <- Wallet.fetchById "Cardashift.Public.Sale.Exchange"
    nextExchangeAddress <- ChildAddress.address <$> ChildAddress.fetchById (ChildAddressRef nextExchangeWalletName 0)

    Wallet{name= collateralWalletName} <- Wallet.fetchById "Global.Collateral"
    collateral <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef collateralWalletName 0)

    ask >>= (\case
        Mainnet {} ->         
           return $ Round
                { title = "Flash Sale (Mainnet !!!!!)"
                , settings = RoundSettings
                                { syncSlot = Nothing
                                , maximumAdaPerAddress = adaOf 50_000
                                , minimumAdaPerFund = adaOf 3
                                , timeRange = interval 51_73_71_99 52_80_66_55 
                                , kycIntegration = Integration { params = \(ChildAddressIndex index) -> "admin=xR0CE2Vk80fOoD57slc4&from_user=" <> show index <>  "&round=1"
                                                            , url = "https://api.cardashift.com/api/admin/participants"}
                                , exchangeTokenId = assetClass "db30c7905f598ed0154de14f970de0f61f0cb3943ed82c891968480a" "434c4150" -- "3657151839007cfd06deeb174987957dc9c21cfd3863d384c0d0293a" "434c41505554"
                                , investorsWallet = investorsWallet
                                , previousRoundMaybe = Nothing
                                , nextRoundMaybe = Just $ NextRound nextExchangeAddress
                                , tokenRatePerLovelace = 36_000 / 1_000_000
                                , addresses = RoundAddresses
                                                { exchange = exchange
                                                , collateral = collateral
                                                , tokens = tokens
                                                , adaSink = "DdzFFzCqrhsuG7R4n5w9vr2Zo6quuzVbqQbfcDm8BZV29p5T8yTfBnz4Jx3mmgsXCoDtjpCVyB61ttV4MVsVivnQHMKEzFozBHVE8Emq"
                                                , fees } }} 
        Testnet {} -> error "No Testnet for Flash Sale anymore")

         

getPublicSaleSettings
    :: ( MonadIO m
       , MonadReader Environment m)
       => m Round
getPublicSaleSettings  = do
    Wallet{name= collateralWalletName} <- Wallet.fetchById "Global.Collateral"
    collateral <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef collateralWalletName 0)

    Wallet{name= feesWalletName} <- Wallet.fetchById "Cardashift.Fees"
    fees <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef feesWalletName 0)

    previousInvestorWalletRound <-  Wallet.fetchById "Cardashift.Flash.Sale.Investors"
    previousExchangeWalletRound <-  Wallet.fetchById "Cardashift.Flash.Sale.Exchange"

    investorsWallet <- Wallet.fetchById "Cardashift.Public.Sale.Investors"

    Wallet{name= tokenWalletName} <- Wallet.fetchById "Cardashift.Public.Sale.Tokens"
    tokens <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef tokenWalletName 0)

    Wallet{name= exchangeWalletName} <- Wallet.fetchById "Cardashift.Public.Sale.Exchange"
    exchange <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef exchangeWalletName 0)

  
    ask >>= (\case
        Testnet {} -> error "No Testnet for Public Sale anymore"
        Mainnet {} ->         
           return $ Round
                { title = "Public Sale (Mainnet !!!!!)"
                , settings = RoundSettings
                                { syncSlot = Nothing
                                , maximumAdaPerAddress = adaOf 25_000
                                , minimumAdaPerFund = adaOf 3
                                , timeRange = interval 51_73_71_99 52_05_69_09 -- 51_80_66_55 
                                , kycIntegration = Integration { params = \(ChildAddressIndex index) -> "admin=xR0CE2Vk80fOoD57slc4&from_user=" <> show index <>  "&round=2"
                                                            , url = "https://api.cardashift.com/api/admin/participants"}
                                , exchangeTokenId = assetClass "db30c7905f598ed0154de14f970de0f61f0cb3943ed82c891968480a" "434c4150"
                                , investorsWallet = investorsWallet
                                , previousRoundMaybe = Just PreviousRound {investorsWallet = previousInvestorWalletRound , exchangeWallet = previousExchangeWalletRound}
                                , nextRoundMaybe = Nothing
                                , tokenRatePerLovelace = 30_000 / 1_000_000
                                , addresses = RoundAddresses
                                                { exchange = exchange
                                                , collateral = collateral
                                                , tokens = tokens
                                                , adaSink = "DdzFFzCqrhsuG7R4n5w9vr2Zo6quuzVbqQbfcDm8BZV29p5T8yTfBnz4Jx3mmgsXCoDtjpCVyB61ttV4MVsVivnQHMKEzFozBHVE8Emq"
                                                , fees } }}) 

