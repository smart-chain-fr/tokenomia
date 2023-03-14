{-# LANGUAGE ExtendedDefaultRules                      #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE FlexibleInstances                         #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE NamedFieldPuns                            #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE TemplateHaskell                           #-}
{-# LANGUAGE TupleSections                             #-}
{-# LANGUAGE TypeApplications                          #-}


{-# LANGUAGE ImportQualifiedPost                       #-}
{-# OPTIONS_GHC -Wno-orphans                           #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures           #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds             #-}

module Tokenomia.Wallet.ChildAddress.ChainIndex
    ( queryUTxO
    , queryUTxOsFilterBy
    ) where


import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding
    as TLE                                             ( decodeUtf8 )

import Control.Monad.Reader                            ( MonadIO(..), MonadReader, asks )
import Shh.Internal                                    ( ExecReference(SearchPath), capture, load, (|>) )


import Tokenomia.Common.Address                        ( Address(Address) )
import Tokenomia.Common.Environment                    ( Environment(magicNumber) )
import Tokenomia.Common.Serialise                      ( FromCLI(fromCLI) )
import Tokenomia.Common.Value                          ()

import Tokenomia.Wallet.ChildAddress.ChildAddressRef   ( ChildAddressRef )
import Tokenomia.Wallet.ChildAddress.LocalRepository   ( ChildAddress(ChildAddress, address), fetchById )
import Tokenomia.Wallet.WalletUTxO                     ( WalletUTxO(WalletUTxO) )

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
