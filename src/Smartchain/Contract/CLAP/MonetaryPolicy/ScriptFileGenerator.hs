{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Smartchain.Contract.CLAP.MonetaryPolicy.ScriptFileGenerator 
  (main) 
  where

import           Prelude

import qualified Data.Aeson as A
import qualified Data.Maybe as M
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise ( serialise )
import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..))
import           Ledger
import qualified Plutus.V1.Ledger.Api as Plutus
import           PlutusTx.Builtins.Internal ()
import           Smartchain.Contract.CLAP.MonetaryPolicy ( mkCLAPMonetaryPolicyScript )


main :: IO ()
main = do
  case Plutus.defaultCostModelParams of
        Just m ->
          let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptShortBS []
          in do print ("Log output" :: String) >> print logout
                case e of
                  Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                  Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
        Nothing -> error "defaultCostModelParams failed"
  result <- writeFileTextEnvelope "minting-policy-CLAP.plutus" Nothing plutusScriptV1
  case result of
    Left err -> print $ displayError err
    Right () -> return ()


scriptAsCbor :: LB.ByteString
scriptAsCbor 
  = (serialise . unMintingPolicyScript . mkCLAPMonetaryPolicyScript)  
        (TxOutRef (M.fromJust $ A.decode "{\"getTxId\" : \"95f644032e4e2f516ddee5ae9ceb8d467f53f630c4d4f481771cb56063adb244\"}") 0)

plutusScriptV1 :: PlutusScript PlutusScriptV1
plutusScriptV1 = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

scriptShortBS :: SBS.ShortByteString
scriptShortBS = SBS.toShort . LB.toStrict $ scriptAsCbor