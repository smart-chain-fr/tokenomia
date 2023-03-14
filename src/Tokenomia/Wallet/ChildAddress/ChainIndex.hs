{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}


{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tokenomia.Wallet.ChildAddress.ChainIndex
    ( queryUTxO
    , queryUTxOsFilterBy
    ) where


import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as TLE ( decodeUtf8 )

import           Control.Monad.Reader ( MonadReader, MonadIO(..), asks )
import Shh.Internal
    ( capture, load, (|>), ExecReference(SearchPath) )


import Tokenomia.Common.Address ( Address(Address) )
import Tokenomia.Common.Serialise ( FromCLI(fromCLI) )
import Tokenomia.Common.Environment ( Environment(magicNumber) )
import           Tokenomia.Common.Value ()

import Tokenomia.Wallet.WalletUTxO ( WalletUTxO(WalletUTxO) )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
    ( ChildAddressRef )
import Tokenomia.Wallet.ChildAddress.LocalRepository
    ( fetchById, ChildAddress(ChildAddress, address) )

load SearchPath ["cardano-cli"]

queryUTxO ::
  ( MonadIO m
  , MonadReader Environment m )
  => ChildAddressRef
  -> m [WalletUTxO]
queryUTxO childAddressRef = do
    ChildAddress {address = Address address} <- fetchById childAddressRef
    magicN <- asks magicNumber
    utxos <- fromCLI . TL.toStrict . TLE.decodeUtf8 <$> liftIO (cardano_cli "query" "utxo" "--testnet-magic" magicN "--address" address |> capture)
    return $ WalletUTxO childAddressRef <$> utxos


queryUTxOsFilterBy ::
    ( MonadIO m
    , MonadReader Environment m )
    => ChildAddressRef
    -> (WalletUTxO -> Bool)
    -> m [WalletUTxO]
queryUTxOsFilterBy childAddressRef f = fmap (filter f)  (queryUTxO childAddressRef)
