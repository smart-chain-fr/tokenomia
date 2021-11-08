{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Tokenomia.Wallet.Collateral.Read
    ( fetchCollateral
    , fetchWalletsWithCollateral
    , filterWalletsWithCollateral
    ) where

import           Control.Monad.Reader


import           Data.Maybe
import           Data.List.NonEmpty


import           Tokenomia.Adapter.Cardano.CLI.UTxO

import           Tokenomia.Adapter.Cardano.CLI.Environment

import           Tokenomia.Adapter.Cardano.CLI.Wallet
import qualified Tokenomia.Adapter.Cardano.CLI.UTxO.Query as UTxOs

import           Prelude hiding (print)


filterWalletsWithCollateral 
  :: ( MonadIO m
     , MonadReader Environment m )
     => NonEmpty Wallet
     -> m (Maybe (NonEmpty Wallet))
filterWalletsWithCollateral xs = do 
    wallets <- (filterM (fmap isNothing . fetchCollateral ) . toList) xs
    (return . nonEmpty) wallets


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

