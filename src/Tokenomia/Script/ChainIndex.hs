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

module Tokenomia.Script.ChainIndex (
  queryUTxO,
) where

import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding as TLE (decodeUtf8)

import Control.Monad.Reader (MonadIO (..), MonadReader, asks)
import Shh.Internal

import Tokenomia.Common.Address
import Tokenomia.Common.Environment
import Tokenomia.Common.Serialise
import Tokenomia.Common.Value ()
import Tokenomia.Script.UTxO

load SearchPath ["cardano-cli"]

queryUTxO ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  Address ->
  m [ScriptUTxO]
queryUTxO (Address address) = do
  magicN <- asks magicNumber
  fromCLI . TL.toStrict . TLE.decodeUtf8 <$> liftIO (cardano_cli "query" "utxo" "--testnet-magic" magicN "--address" address |> capture)
