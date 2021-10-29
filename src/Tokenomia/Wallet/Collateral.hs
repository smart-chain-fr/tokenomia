{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Tokenomia.Wallet.Collateral
    ( createCollateral
    , fetchCollateral
    , fetchWalletsWithCollateral
    ) where

import Control.Monad.Reader

import Shh
    ( load,
      ExecReference(SearchPath) )

import           Data.List.NonEmpty (nonEmpty, NonEmpty, toList)

import           Data.Maybe
import qualified Data.Text as T

import           Control.Monad.Except

import           Ledger.Ada

import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Tokenomia.Adapter.Cardano.CLI.Transaction
import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Tokenomia.Wallet.CLI as Wallet
import           Tokenomia.Adapter.Cardano.CLI.Wallet
import qualified Tokenomia.Adapter.Cardano.CLI.UTxO.Query as UTxOs

import           Tokenomia.Common.Error


load SearchPath ["echo"]


createCollateral
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError BuildingTxError m)
       => m ()
createCollateral = do
    query_registered_wallets         >>= whenNullThrow    NoWalletRegistered
    >>= filterWalletsWithCollateral  >>= whenNothingThrow NoWalletWithoutCollateral 
    >>= \wallets -> do
            liftIO $ echo "Select the wallet to receive the collateral"
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
createCollateral' senderWallet@Wallet {paymentAddress = senderAddr,..} = do
    assertCollateralNotAlreadyCreated senderWallet
    utxoWithFees <- selectBiggestStrictlyADAsNotCollateral senderWallet >>=  whenNothingThrow NoADAInWallet
    submit paymentSigningKeyPath utxoWithFees
        [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees
        , "--tx-out" , senderAddr <> " " <>(T.unpack . toCLI) (adaValueOf 2.0)
        , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithFees
        , "--change-address"  , senderAddr]


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
    filter containsCollateral <$> UTxOs.query paymentAddress
        >>= \case
            [] -> return Nothing
            (x:_)  -> (return . Just) x

