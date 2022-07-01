{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
module Spec.Tokenomia.Common.Value (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Text.InterpolatedString.Perl6 (qc)

import Plutus.V1.Ledger.Value ( singleton )

import Ledger.Ada ( lovelaceValueOf )

import Tokenomia.Common.Value ()
import Tokenomia.Common.Serialise ( FromCLI(fromCLI) ) 

tests :: TestTree
tests = testGroup "Value" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "fromCLI cardano-cli Value" $
     fromCLI [qc|1379280 lovelace + 489999979900 bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c.434c4150 + TxOutDatumNone|]
      @?= lovelaceValueOf 1379280 <>  singleton "bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c" "CLAP" 489999979900]
