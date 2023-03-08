{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DisambiguateRecordFields #-}


module Tokenomia.Wallet.Collateral.Read
    ( fetchCollateral
    , fetchWalletsWithCollateral
    , filterWalletsWithCollateral
    ) where

import           Control.Monad.Reader


import           Data.Maybe
import           Data.List.NonEmpty


import           Tokenomia.Common.Value

import           Tokenomia.Common.Environment

import           Tokenomia.Wallet.LocalRepository
import           Tokenomia.Wallet.UTxO
import           Tokenomia.Wallet.WalletUTxO hiding ( value )
import           Tokenomia.Wallet.ChildAddress.ChainIndex
import           Prelude hiding (print)
import           Tokenomia.Wallet.ChildAddress.ChildAddressRef


filterWalletsWithCollateral
  :: ( MonadIO m
     , MonadReader Environment m )
     => NonEmpty Wallet
     -> m (Maybe (NonEmpty Wallet))
filterWalletsWithCollateral xs = do
    wallets <- (filterM (\Wallet {name} -> fmap isNothing . fetchCollateral $ ChildAddressRef name 0 ) . toList) xs
    (return . nonEmpty) wallets


fetchWalletsWithCollateral
  :: ( MonadIO m
     , MonadReader Environment m )
     => m [Wallet]
fetchWalletsWithCollateral = fetchAll >>= filterM (\Wallet {name} -> fmap isJust . fetchCollateral $ ChildAddressRef name 0 )


fetchCollateral
  :: ( MonadIO m
     , MonadReader Environment m )
     => ChildAddressRef
    -> m (Maybe WalletUTxO)
fetchCollateral childAddressRef =
    queryUTxOsFilterBy childAddressRef (containsCollateral . value . utxo)
    >>= \case
         [] -> return Nothing
         (x : _) -> (return . Just) x
