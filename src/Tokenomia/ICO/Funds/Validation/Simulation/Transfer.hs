{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Validation.Simulation.Transfer
    ( dispatchAdasOnChildAdresses) where

import           Prelude
import           Control.Monad.Reader hiding (ask)
import           Control.Monad.Except

import           Tokenomia.Common.Environment
import           Tokenomia.Common.Transacting

import Data.List.NonEmpty as NEL
import           Ledger.Ada as Ada
import           Tokenomia.Wallet.UTxO
import           Tokenomia.Wallet.WalletUTxO
import           Tokenomia.Wallet.LocalRepository
import           Tokenomia.Common.Error
import           Tokenomia.Wallet.Collateral.Read
import           Tokenomia.Wallet.CLI
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu (ask)
import           Tokenomia.Common.Value
import           Tokenomia.Wallet.Type
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef
import           Tokenomia.Wallet.ChildAddress.LocalRepository


dispatchAdasOnChildAdresses ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
    => m ()
dispatchAdasOnChildAdresses = do
    Wallet {name = sourceAdaWalletName} <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
        >>= \wallets -> do
            printLn "Select the wallet containing funds: "
            askToChooseAmongGivenWallets wallets
    source@WalletUTxO {utxo = UTxO {value}} <- selectBiggestStrictlyADAsNotCollateral (ChildAddressRef sourceAdaWalletName 0) >>= whenNothingThrow NoADAsOnChildAddress

    Wallet {name = collateralWalletName} <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral 
        >>= \wallets -> do
            printLn "Select the wallet containing collaterals : "
            askToChooseAmongGivenWallets wallets
    Wallet {name = investorWalletName} <- fetchAll >>= whenNullThrow NoWalletRegistered 
        >>= \wallets -> do
            printLn "Select the investor wallet : "
            askToChooseAmongGivenWallets wallets
    
    printLn $ "- We'll dispatch this amount " <> showValueUtf8 value 
    chunkSize <- Ada.adaOf . fromIntegral <$> ask @Integer "- Chunk size : "
    from <-  ask @Int "- From which index : "
    to   <-  ask @Int "- To which index : "
    dispatchAdasOnChildAdresses' 
        collateralWalletName
        sourceAdaWalletName 
        investorWalletName
        source 
        from 
        to 
        chunkSize

dispatchAdasOnChildAdresses' ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m)
    => WalletName
    -> WalletName
    -> WalletName
    -> WalletUTxO
    -> Int
    -> Int
    -> Ada
    -> m ()
dispatchAdasOnChildAdresses' 
    collateralWalletName
    sourceAdaWalletName 
    investorWalletName 
    source@WalletUTxO {utxo = UTxO {value}} from to chunkSize  = do

    indexes <-  Prelude.take (to - from) . NEL.drop from  <$> fetchByWalletIndexedAddress investorWalletName
    let collateral     = CollateralAddressRef $ ChildAddressRef collateralWalletName 0
        fees           = FeeAddressRef $ ChildAddressRef sourceAdaWalletName 0
        adaUtxosNumber = fromIntegral $ Ada.fromValue value `div` chunkSize
        utxoPerIndex   = adaUtxosNumber `div` Prelude.length indexes

    printLn $ "nb utxos = " <> show adaUtxosNumber
    printLn $ "utxoPerIndex = " <> show utxoPerIndex

    buildAndSubmit
      (Unbalanced fees)
      (Just collateral)
      TxBuild
        { inputsFromWallet  = FromWallet source :| []
        , inputsFromScript  = Nothing
        , outputs = NEL.fromList $ txOUtForIndex utxoPerIndex chunkSize =<< indexes
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , metadataMaybe = Nothing}

txOUtForIndex :: Int -> Ada -> IndexedAddress -> [TxOut]
txOUtForIndex utxoPerIndex chunkSize IndexedAddress {..} =
    replicate utxoPerIndex
     ToWallet
        { address= address
        , value = Ada.toValue chunkSize
        , datumMaybe = Nothing }