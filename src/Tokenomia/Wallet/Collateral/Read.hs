{-# LANGUAGE DisambiguateRecordFields                  #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE NamedFieldPuns                            #-}


module Tokenomia.Wallet.Collateral.Read
    ( fetchCollateral
    , fetchWalletsWithCollateral
    , filterWalletsWithCollateral
    ) where

import Control.Monad.Reader                            ( MonadIO, MonadReader, filterM )


import Data.List.NonEmpty                              ( NonEmpty, nonEmpty, toList )
import Data.Maybe                                      ( isJust, isNothing )


import Tokenomia.Common.Value                          ( containsCollateral )

import Tokenomia.Common.Environment                    ( Environment )

import Prelude hiding                                  ( print )
import Tokenomia.Wallet.ChildAddress.ChainIndex        ( queryUTxOsFilterBy )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef   ( ChildAddressRef(ChildAddressRef) )
import Tokenomia.Wallet.LocalRepository                ( fetchAll )
import Tokenomia.Wallet.Type                           ( Wallet(Wallet, name) )
import Tokenomia.Wallet.UTxO                           ( UTxO(value) )
import Tokenomia.Wallet.WalletUTxO                     ( WalletUTxO(utxo) )


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
