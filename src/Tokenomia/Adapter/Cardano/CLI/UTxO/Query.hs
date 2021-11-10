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

module Tokenomia.Adapter.Cardano.CLI.UTxO.Query
    ( query
<<<<<<< HEAD
    , queryUTxOsFilterBy
=======
    , queryUTxOsContainingStrictlyADAs
>>>>>>> 12f4d10 ([ada] consolidation done)
    ) where


import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Encoding as TLE ( decodeUtf8 )

import           Control.Monad.Reader ( MonadReader, MonadIO(..), asks )
import           Shh.Internal

import           Tokenomia.Adapter.Cardano.CLI.Wallet
import           Tokenomia.Adapter.Cardano.CLI.Serialise
import           Tokenomia.Adapter.Cardano.CLI.Environment
import           Tokenomia.Adapter.Cardano.CLI.Value ()
import           Tokenomia.Adapter.Cardano.CLI.UTxO
import            Tokenomia.Adapter.Cardano.Types


load SearchPath ["cardano-cli"]

query :: 
  ( MonadIO m
  , MonadReader Environment m )
  => Address
  -> m [UTxO]
query (Address address) = do
    magicN <- asks magicNumber
    fromCLI . TL.toStrict . TLE.decodeUtf8 <$> liftIO (cardano_cli "query" "utxo" "--testnet-magic" magicN "--address" address |> capture)


queryUTxOsFilterBy ::
    ( MonadIO m
    , MonadReader Environment m )
    => Wallet
    -> (UTxO -> Bool)
    -> m [UTxO]
queryUTxOsFilterBy Wallet {..} f = filter f <$> query paymentAddress
