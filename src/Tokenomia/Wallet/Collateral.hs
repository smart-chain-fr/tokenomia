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
    , Error (..)) where

import Control.Monad.Reader

import Shh
    ( load,
      ExecReference(SearchPath) )

import           Tokenomia.Adapter.Cardano.CLI
import           Tokenomia.Wallet.CLI as Wallet
import qualified Data.Text as T
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import           Ledger.Ada
import           Control.Monad.Except

import          Data.List.NonEmpty (nonEmpty)
import          Data.Maybe
{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["echo"]


data Error = NoWalletRegistered
           | NoWalletWithoutCollateral
           | AlreadyACollateral UTxO
           | NoADAInWallet deriving Show

createCollateral
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError Error m)
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
       , MonadError Error m)
       => Wallet 
       -> m ()
createCollateral' senderWallet@Wallet {paymentAddress = senderAddr,..} = do
    assertCollateralNotAlreadyCreated senderWallet
    utxoForFees <- selectUTxOForFees senderWallet >>=  whenNothingThrow NoADAInWallet
    submitTx paymentSigningKeyPath
        [ "--tx-in"  , (T.unpack . toCLI . txOutRef) utxoForFees
        , "--tx-out" , senderAddr <> " " <>(T.unpack . toCLI) (adaValueOf 2.0)
        , "--tx-in-collateral", (T.unpack . toCLI . txOutRef) utxoForFees
        , "--change-address"  , senderAddr]


assertCollateralNotAlreadyCreated
    :: ( MonadIO m
        , MonadReader Environment m
        , MonadError Error m) => Wallet -> m ()
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
    filter containsCollateral <$> getUTxOs paymentAddress
        >>= \case
            [] -> return Nothing
            (x:_)  -> (return . Just) x

