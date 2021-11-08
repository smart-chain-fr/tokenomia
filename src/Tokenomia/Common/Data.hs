{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tokenomia.Common.Data (dataToJSONString) where

import Data.Aeson as Json ( encode )

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