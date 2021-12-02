{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}


module Tokenomia.Wallet.Collateral.Write
    ( createCollateral
    , createCollateral'
    ) where

import           Control.Monad.Reader


import           Data.Maybe
import           Data.List.NonEmpty
import           Control.Monad.Except

import           Ledger.Ada

import           Tokenomia.Common.Transacting
import           Tokenomia.Common.Environment

import           Tokenomia.Wallet.CLI as Wallet
import           Tokenomia.Wallet.LocalRepository hiding (fetchById)

import           Tokenomia.Common.Error
import           Tokenomia.Common.Shell.Console (printLn)
import           Prelude hiding (print)
import           Tokenomia.Wallet.Collateral.Read
import           Tokenomia.Wallet.Type as Wallet
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef 
import           Tokenomia.Wallet.ChildAddress.LocalRepository 

createCollateral :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m )
    => m ()
createCollateral = do
    fetchAll         >>= whenNullThrow    NoWalletRegistered
    >>= filterWalletsWithCollateral  >>= whenNothingThrow NoWalletWithoutCollateral 
    >>= \wallets -> do
            printLn "Select the wallet to receive the collateral"
            askToChooseAmongGivenWallets wallets 
    >>= createCollateral'. Wallet.name

createCollateral'
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
       => WalletName 
       -> m ()
createCollateral' walletName = do
    let firstAddress = ChildAddressRef walletName 0
    assertCollateralNotAlreadyCreated firstAddress
    ada <- selectBiggestStrictlyADAsNotCollateral firstAddress >>= whenNothingThrow NoADAInWallet
    ChildAddress {address = senderAddr} <- fetchById firstAddress
    buildAndSubmitWithoutCollateral
      (FeeAddressRef firstAddress )
      TxBuild
        { inputsFromWallet =  FromWallet ada :| []
        , inputsFromScript = Nothing
        , outputs = ToWallet senderAddr (adaValueOf 2.0) Nothing :| [] 
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , metadataMaybe = Nothing }

assertCollateralNotAlreadyCreated :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m ) 
    => ChildAddressRef 
    -> m ()
assertCollateralNotAlreadyCreated childAddressRef = fetchCollateral childAddressRef >>=  whenSomethingThrow (const AlreadyACollateral)



