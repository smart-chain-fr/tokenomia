{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tokenomia.ICO.Funds.Validation.CardanoCLI.Datum (
  ExchangeDatum (..),
  mkExchangeDatum,
  datumFromJson,
) where

import Codec.Serialise.Class (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Prelude qualified as PlutusTx
import Prelude as Haskell

import Ledger.Slot
import Tokenomia.Wallet.ChildAddress.ChildAddressRef

import Cardano.Api qualified as Cardano
import Cardano.Api.Shelley qualified as Cardano

mkExchangeDatum ::
  Slot ->
  ChildAddressIndex ->
  ExchangeDatum
mkExchangeDatum (Slot slot) (ChildAddressIndex index) = ExchangeDatum (slot, index)

newtype ExchangeDatum = ExchangeDatum (PlutusTx.Integer, PlutusTx.Integer)
  deriving stock (Generic, Haskell.Show, Haskell.Eq)
  deriving newtype (Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
  deriving anyclass (Hashable, NFData, ToJSON, FromJSON)

PlutusTx.makeLift ''ExchangeDatum

datumFromJson :: Value -> (Slot, ChildAddressIndex)
datumFromJson v =
  case Cardano.scriptDataFromJson
    Cardano.ScriptDataJsonDetailedSchema
    v of
    Left e -> error $ "Unexpected Datum format " <> show e
    Right sData -> case (PlutusTx.fromData . Cardano.toPlutusData) sData of
      Nothing -> error $ "Unexpected Datum unserialisation " <> show v
      Just (ExchangeDatum (x, y)) -> (Slot x, ChildAddressIndex y)
