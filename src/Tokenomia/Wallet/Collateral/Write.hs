{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Tokenomia.Wallet.Collateral.Write
    ( createCollateral
    , createCollateral'
    ) where

import           Control.Monad.Reader


import           Data.Maybe
import           Data.List.NonEmpty
import           Control.Monad.Except

import           Ledger.Ada

import           Tokenomia.Adapter.Cardano.CLI.Transaction
import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Tokenomia.Wallet.CLI as Wallet
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.UTxO


import           Tokenomia.Common.Error
import           Tokenomia.Common.Shell.Console (printLn)
import           Prelude hiding (print)
import           Tokenomia.Wallet.Collateral.Read

createCollateral :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m )
    => m ()
createCollateral = do
    query_registered_wallets         >>= whenNullThrow    NoWalletRegistered
    >>= filterWalletsWithCollateral  >>= whenNothingThrow NoWalletWithoutCollateral 
    >>= \wallets -> do
            printLn "Select the wallet to receive the collateral"
            askToChooseAmongGivenWallets wallets 
    >>= createCollateral'

createCollateral'
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError BuildingTxError m)
       => Wallet 
       -> m ()
createCollateral' senderWallet = do
    assertCollateralNotAlreadyCreated senderWallet
    ada <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral senderWallet >>= whenNothingThrow NoADAInWallet)
    submitCollateral
      TxBuild
        { wallet = senderWallet
        , txIns =  FromWallet ada :| []
        , txOuts = ToWallet (paymentAddress senderWallet) (adaValueOf 2.0) :| [] 
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , metadataMaybe = Nothing }

assertCollateralNotAlreadyCreated :: 
    ( MonadIO m
    , MonadReader Environment m
    , MonadError BuildingTxError m ) 
    => Wallet 
    -> m ()
assertCollateralNotAlreadyCreated wallet = fetchCollateral wallet >>=  whenSomethingThrow AlreadyACollateral



