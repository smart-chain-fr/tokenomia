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
        Testnet {} -> do
            flashSale      <- getFlashSaleSettings
            publicSale     <- getPublicSaleSettings
            return $ ICO
                        { projectName = "CardaShift (Testnet)"
                        , rounds = flashSale :| [publicSale]} :| []
        Mainnet {} ->  do   
            flashSale      <- getFlashSaleSettings
            -- publicSale     <- getPublicSaleSettings
            return $ ICO
                    { projectName = "CardaShift (Mainnet with CLAP !!!)"
                    , rounds = flashSale :| []} 
                    :| []



getTestFlashSaleSettings
    :: ( MonadIO m
       , MonadReader Environment m)
       => m Round
getTestFlashSaleSettings  = do
    Wallet{name= collateralWalletName} <- Wallet.fetchById "Global.Collateral"
    collateral <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef collateralWalletName 0)

    Wallet{name= feesWalletName} <- Wallet.fetchById "Cardashift.Fees"
    fees <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef feesWalletName 0)


    investorsWallet <- Wallet.fetchById "Test.Cardashift.Flash.Sale.Investors"

    Wallet{name= tokenWalletName} <- Wallet.fetchById "Test.Cardashift.Flash.Sale.Tokens"
    tokens <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef tokenWalletName 0)

    Wallet{name= exchangeWalletName} <- Wallet.fetchById "Test.Cardashift.Flash.Sale.Exchange"
    exchange <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef exchangeWalletName 0)

    Wallet{name= nextExchangeWalletName} <- Wallet.fetchById "Test.Cardashift.Public.Sale.Exchange"
    nextExchangeAddress <- ChildAddress.address <$> ChildAddress.fetchById (ChildAddressRef nextExchangeWalletName 0)

    Wallet{name= sinkWalletName} <- Wallet.fetchById "Test.Cardashift.Flash.Sale.Sink"
    sinkAddress <- ChildAddress.address <$> ChildAddress.fetchById (ChildAddressRef sinkWalletName 0)

    Wallet{name= lambdaInvestorWalletName} <- Wallet.fetchById "Test.Cardashift.Flash.Lambda.Investor"
    lambdaInvestorAddress <- ChildAddress.address <$> ChildAddress.fetchById (ChildAddressRef lambdaInvestorWalletName 0)


    return $ Round
              { title = "Test Flash Sale"
              , settings = RoundSettings
                            { syncSlot = Nothing
                            , maximumAdaPerAddress = adaOf 9
                            , minimumAdaPerFund = adaOf 3
                            , timeRange = interval 40_354_960 51_663_079 --43_987_900 -- 44_653_861 
                            , kycIntegration = Simulation lambdaInvestorAddress
                            , exchangeTokenId = assetClass "a0b03e9b2bf7228f54e0f51e6bd34f6e949eedb8ecae84f984452fc4" "506f736569646f6e6973"
                            , investorsWallet = investorsWallet
                            , previousRoundMaybe = Nothing
                            , nextRoundMaybe = Just $ NextRound nextExchangeAddress
                            , tokenRatePerLovelace = 30 / 1_000_000
                            , addresses = RoundAddresses
                                            { exchange = exchange
                                            , collateral = collateral
                                            , tokens = tokens
                                            , adaSink = sinkAddress
                                            , fees } }}

getTestPublicSaleSettings
    :: ( MonadIO m
       , MonadReader Environment m)
       => m Round
getTestPublicSaleSettings  = do
    Wallet{name= collateralWalletName} <- Wallet.fetchById "Global.Collateral"
    collateral <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef collateralWalletName 0)

    Wallet{name= feesWalletName} <- Wallet.fetchById "Cardashift.Fees"
    fees <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef feesWalletName 0)

    previousRound <- PreviousRound <$> Wallet.fetchById "Test.Cardashift.Flash.Sale.Investors"

    investorsWallet <- Wallet.fetchById "Test.Cardashift.Public.Sale.Investors"

    Wallet{name= tokenWalletName} <- Wallet.fetchById "Test.Cardashift.Public.Sale.Tokens"
    tokens <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef tokenWalletName 0)

    Wallet{name= exchangeWalletName} <- Wallet.fetchById "Test.Cardashift.Public.Sale.Exchange"
    exchange <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef exchangeWalletName 0)

    Wallet{name= sinkWalletName} <- Wallet.fetchById "Test.Cardashift.Public.Sale.Sink"
    sinkAddress <- ChildAddress.address <$> ChildAddress.fetchById (ChildAddressRef sinkWalletName 0)

    Wallet{name= lambdaInvestorWalletName} <- Wallet.fetchById "Test.Cardashift.Public.Lambda.Investor"
    lambdaInvestorAddress <- ChildAddress.address <$> ChildAddress.fetchById (ChildAddressRef lambdaInvestorWalletName 0)

    return $ Round
              { title = "Test Public Sale (Mainnet)"
              , settings = RoundSettings
                            { syncSlot = Nothing
                            , maximumAdaPerAddress = adaOf 10
                            , minimumAdaPerFund = adaOf 3
                            , timeRange = interval 40354960 51_663_079-- 44653861
                            , kycIntegration = Simulation lambdaInvestorAddress
                            , exchangeTokenId = assetClass "a0b03e9b2bf7228f54e0f51e6bd34f6e949eedb8ecae84f984452fc4" "506f736569646f6e6973"
                            , investorsWallet = investorsWallet
                            , previousRoundMaybe = Just previousRound
                            , nextRoundMaybe = Nothing
                            , tokenRatePerLovelace = 36 / 1_000_000
                            , addresses = RoundAddresses
                                            { exchange = exchange
                                            , collateral = collateral
                                            , tokens = tokens
                                            , adaSink = sinkAddress
                                            , fees } }}


getFlashSaleSettings
    :: ( MonadIO m
       , MonadReader Environment m)
       => m Round
getFlashSaleSettings  = do
    Wallet{name= collateralWalletName} <- Wallet.fetchById "Global.Collateral"
    collateral <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef collateralWalletName 0)

    Wallet{name= feesWalletName} <- Wallet.fetchById "Cardashift.Fees"
    fees <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef feesWalletName 0)


    investorsWallet <- Wallet.fetchById "Cardashift.Flash.Sale.Investors"

    Wallet{name= tokenWalletName} <- Wallet.fetchById "Cardashift.Flash.Sale.Tokens"
    tokens <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef tokenWalletName 0)

    Wallet{name= exchangeWalletName} <- Wallet.fetchById "Cardashift.Flash.Sale.Exchange"
    exchange <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef exchangeWalletName 0)

    Wallet{name= nextExchangeWalletName} <- Wallet.fetchById "Cardashift.Public.Sale.Exchange"
    nextExchangeAddress <- ChildAddress.address <$> ChildAddress.fetchById (ChildAddressRef nextExchangeWalletName 0)

    
    let sinkAddress = "AddressPSAN ADRIEN"

    ask >>= (\case
       Mainnet {} ->         
           return $ Round
                { title = "Flash Sale (Mainnet !!!!!)"
                , settings = RoundSettings
                                { syncSlot = Just 44_653_861 --pas bon
                                , maximumAdaPerAddress = adaOf 50_000
                                , minimumAdaPerFund = adaOf 190
                                , timeRange = interval 40_354_960 51_663_079 -- pas bon
                                , kycIntegration = Integration { params = \(ChildAddressIndex index) -> "admin=xR0CE2Vk80fOoD57slc4&from_user=" <> show index <>  "&round=1"
                                                            , url = "https://api.cardashift.com/api/admin/participants"}
                                , exchangeTokenId = assetClass "db30c7905f598ed0154de14f970de0f61f0cb3943ed82c891968480a" "434c4150"
                                , investorsWallet = investorsWallet
                                , previousRoundMaybe = Nothing
                                , nextRoundMaybe = Just $ NextRound nextExchangeAddress
                                , tokenRatePerLovelace = 36 / 1_000_000
                                , addresses = RoundAddresses
                                                { exchange = exchange
                                                , collateral = collateral
                                                , tokens = tokens
                                                , adaSink = sinkAddress
                                                , fees } }} 
       Testnet {} -> do 
        -- let simulatedPaybackAddress = "addr_test1qpk305c9t7prcay2evscg8gxd0nfwh3m8568m647z6ecleav4wcz4fhkctpgjcjc4u3mftp25480vdagueptuxmcu4vsgndfqm"
        error "No Testnet for Flash Sale anymore")
        -- return $ Round
        --         { title = "Flash Sale (TestNet)"
        --         , settings = RoundSettings
        --                         { syncSlot = Just 44_653_861
        --                         , maximumAdaPerAddress = adaOf 1_000
        --                         , minimumAdaPerFund = adaOf 3
        --                         , timeRange = interval 40_354_960 51_663_079 -- 43_987_900 -- 44_653_861 
        --                         -- , kycIntegration = Simulation simulatedPaybackAddress
        --                         , kycIntegration = Integration { params = \(ChildAddressIndex index) -> "admin=xR0CE2Vk80fOoD57slc4&from_user=" <> show index <>  "&round=1"
        --                                                     , url = "https://api.cardashift.com/api/admin/participants"}
        --                         , exchangeTokenId = assetClass "a0b03e9b2bf7228f54e0f51e6bd34f6e949eedb8ecae84f984452fc4" "506f736569646f6e6973"
        --                         , investorsWallet = investorsWallet
        --                         , previousRoundMaybe = Nothing
        --                         , nextRoundMaybe = Just $ NextRound nextExchangeAddress
        --                         , tokenRatePerLovelace = 333298 / 1_000_000
        --                         , addresses = RoundAddresses
        --                                         { exchange = exchange
        --                                         , collateral = collateral
        --                                         , tokens = tokens
        --                                         , adaSink = sinkAddress
        --                                         , fees } }})
         

getPublicSaleSettings
    :: ( MonadIO m
       , MonadReader Environment m)
       => m Round
getPublicSaleSettings  = do
    Wallet{name= collateralWalletName} <- Wallet.fetchById "Global.Collateral"
    collateral <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef collateralWalletName 0)

    Wallet{name= feesWalletName} <- Wallet.fetchById "Cardashift.Fees"
    fees <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef feesWalletName 0)

    previousRound <- PreviousRound <$> Wallet.fetchById "Cardashift.Flash.Sale.Investors"

    investorsWallet <- Wallet.fetchById "Cardashift.Public.Sale.Investors"

    Wallet{name= tokenWalletName} <- Wallet.fetchById "Cardashift.Public.Sale.Tokens"
    tokens <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef tokenWalletName 0)

    Wallet{name= exchangeWalletName} <- Wallet.fetchById "Cardashift.Public.Sale.Exchange"
    exchange <- toIndexedAddress <$> ChildAddress.fetchById (ChildAddressRef exchangeWalletName 0)

    Wallet{name= sinkWalletName} <- Wallet.fetchById "Cardashift.Public.Sale.Sink"
    sinkAddress <- ChildAddress.address <$> ChildAddress.fetchById (ChildAddressRef sinkWalletName 0)

    ask >>= (\case
       Testnet {} -> do
            let simulatedPaybackAddress = "addr_test1qpk305c9t7prcay2evscg8gxd0nfwh3m8568m647z6ecleav4wcz4fhkctpgjcjc4u3mftp25480vdagueptuxmcu4vsgndfqm"

            return $ Round
                    { title = "Public Sale (Testnet)"
                    , settings = RoundSettings
                                    { syncSlot = Just 0
                                    , maximumAdaPerAddress = adaOf 1000
                                    , minimumAdaPerFund = adaOf 3
                                    , timeRange = interval 40354960 44653861
                                    , kycIntegration = Simulation simulatedPaybackAddress
                                    , exchangeTokenId = assetClass "a0b03e9b2bf7228f54e0f51e6bd34f6e949eedb8ecae84f984452fc4" "506f736569646f6e6973"
                                    , investorsWallet = investorsWallet
                                    , previousRoundMaybe = Just previousRound
                                    , nextRoundMaybe = Nothing
                                    , tokenRatePerLovelace = 6 / 1_000_000
                                    , addresses = RoundAddresses
                                                    { exchange = exchange
                                                    , collateral = collateral
                                                    , tokens = tokens
                                                    , adaSink = sinkAddress
                                                    , fees } }}
       Mainnet {} -> error "Public Sale Mainnet TODO") 

