{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Wallet.ChildAddress.ChainIndex (
  queryUTxO,
  queryUTxOsFilterBy,
) where

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding as TLE (decodeUtf8)

import Control.Monad.Reader (MonadIO (..), MonadReader, asks)
import Shh.Internal

import Tokenomia.Common.Address
import Tokenomia.Common.Environment
import Tokenomia.Common.Serialise
import Tokenomia.Common.Value ()

import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.ChildAddress.LocalRepository
import Tokenomia.Wallet.WalletUTxO

load SearchPath ["cardano-cli"]

queryUTxO ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  ChildAddressRef ->
  m [WalletUTxO]
queryUTxO childAddressRef = do
  ChildAddress {address = Address address} <- fetchById childAddressRef
  magicN <- asks magicNumber
  utxos <- fromCLI . TL.toStrict . TLE.decodeUtf8 <$> liftIO (cardano_cli "query" "utxo" "--testnet-magic" magicN "--address" address |> capture)
  return $ WalletUTxO childAddressRef <$> utxos

queryUTxOsFilterBy ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  ChildAddressRef ->
  (WalletUTxO -> Bool) ->
  m [WalletUTxO]
queryUTxOsFilterBy childAddressRef f = fmap (filter f) (queryUTxO childAddressRef)
