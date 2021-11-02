{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Tokenomia.Wallet.Collateral
    ( createCollateral
    , fetchCollateral
    , fetchWalletsWithCollateral
    ) where

import           Control.Monad.Reader


import           Data.Maybe
import           Data.List.NonEmpty
import           Control.Monad.Except

import           Ledger.Ada

import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Adapter.Cardano.CLI.Transaction
import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Tokenomia.Wallet.CLI as Wallet
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import qualified Tokenomia.Adapter.Cardano.CLI.UTxO.Query as UTxOs

import           Tokenomia.Common.Error
import           Tokenomia.Common.Shell.Console (printLn)
import           Prelude hiding (print)


createCollateral
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError BuildingTxError m)
       => m ()
createCollateral = do
    query_registered_wallets         >>= whenNullThrow    NoWalletRegistered
    >>= filterWalletsWithCollateral  >>= whenNothingThrow NoWalletWithoutCollateral 
    >>= \wallets -> do
            printLn "Select the wallet to receive the collateral"
            askToChooseAmongGivenWallets wallets 
    >>= createCollateral'


filterWalletsWithCollateral 
  :: ( MonadIO m
     , MonadReader Environment m )
     => NonEmpty Wallet
     -> m (Maybe (NonEmpty Wallet))
filterWalletsWithCollateral xs = do 
    wallets <- (filterM (fmap isNothing . fetchCollateral ) . toList) xs
    (return . nonEmpty) wallets


createCollateral'
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError BuildingTxError m)
       => Wallet 
       -> m ()
createCollateral' senderWallet = do
    assertCollateralNotAlreadyCreated senderWallet
    utxoWithFees <- txOutRef <$> (selectBiggestStrictlyADAsNotCollateral senderWallet >>=  whenNothingThrow NoADAInWallet)
    submit'
      TxBuild
        { signingKeyPath = paymentSigningKeyPath senderWallet
        , txIns =  FromWallet utxoWithFees :| [] 
        , txOuts = ToWallet (paymentAddress senderWallet) (adaValueOf 2.0) :| [] 
        , changeAdress = paymentAddress $ senderWallet
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , metadataMaybe = Nothing 
        , collateral = utxoWithFees}

assertCollateralNotAlreadyCreated
    :: ( MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m) => Wallet -> m ()
assertCollateralNotAlreadyCreated wallet = fetchCollateral wallet >>=  whenSomethingThrow AlreadyACollateral


fetchWalletsWithCollateral 
  :: ( MonadIO m
     , MonadReader Environment m )
     => m [Wallet]
fetchWalletsWithCollateral = query_registered_wallets >>= filterM (fmap isJust . fetchCollateral )  


fetchCollateral
  :: ( MonadIO m
     , MonadReader Environment m )
     => Wallet
    -> m (Maybe UTxO)
fetchCollateral Wallet {..} =
    Prelude.filter containsCollateral <$> UTxOs.query paymentAddress
        >>= \case
            [] -> return Nothing
            (x:_)  -> (return . Just) x

