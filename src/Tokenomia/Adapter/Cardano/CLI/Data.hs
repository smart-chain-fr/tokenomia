{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tokenomia.Adapter.Cardano.CLI.Data (dataToJSONString) where

import Data.Aeson as Json

import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley
    ( fromPlutusData )
import qualified PlutusTx
import PlutusTx.IsData.Class ( ToData )
import Data.ByteString.Lazy.UTF8 as BLU ( toString ) 

dataToJSONString :: ToData a => a -> String
dataToJSONString =
  BLU.toString
    . Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData