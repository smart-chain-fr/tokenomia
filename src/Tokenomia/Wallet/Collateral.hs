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
    , getCollateral
    ) where

import Control.Monad.Reader

import Shh
    ( load,
      ExecReference(SearchPath) )

import           Data.List.NonEmpty (nonEmpty)
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
{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo"]




createCollateral
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError BuildingTxError m)
       => m ()
createCollateral = do
    allWallets <- query_registered_wallets
    walletWithoutCollateral <- nonEmpty <$> filterM (fmap isNothing . getCollateral ) allWallets
    case walletWithoutCollateral of
        Nothing  | null allWallets -> throwError NoWalletRegistered
        Nothing -> throwError NoWalletWithoutCollateral
        Just wallets -> do
              liftIO $ echo "Select the sender's wallet"
              askAmongGivenWallets wallets >>= createCollateral'


createCollateral'
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError BuildingTxError m)
       => Wallet 
       -> m ()
createCollateral' senderWallet@Wallet {paymentAddress = senderAddr,..} = do
    assertCollateralNotAlreadyCreated senderWallet
    utxoWithFees <- selectUTxOForFees senderWallet >>=  whenNothingThrow NoADAInWallet
    submit paymentSigningKeyPath utxoWithFees
        [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoWithFees
        , "--tx-out" , senderAddr <> " " <>(T.unpack . toCLI) (adaValueOf 2.0)
        , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoWithFees
        , "--change-address"  , senderAddr]


assertCollateralNotAlreadyCreated
    :: ( MonadIO m
        , MonadReader Environment m
        , MonadError BuildingTxError m) => Wallet -> m ()
assertCollateralNotAlreadyCreated wallet = getCollateral wallet >>=  whenSomethingThrow AlreadyACollateral

whenSomethingThrow :: MonadError e m => (a -> e) -> Maybe a  -> m ()
whenSomethingThrow toErr = maybe (pure ()) (throwError . toErr)

whenNothingThrow :: MonadError e m => e -> Maybe a ->  m a
whenNothingThrow err = maybe (throwError err) pure

getCollateral
  :: ( MonadIO m
     , MonadReader Environment m )
     => Wallet
    -> m (Maybe UTxO)
getCollateral Wallet {..} =
    filter containsCollateral <$> UTxOs.query paymentAddress
        >>= \case
            [] -> return Nothing
            (x:_)  -> (return . Just) x

