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
module Spec.Tokenomia.Adapter.Cardano.CLI.UTxO (tests) where

import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Text.InterpolatedString.Perl6 (qc)

import Ledger ( TxOutRef (..) )
import Plutus.V1.Ledger.Value ( singleton )

import Ledger.Ada ( lovelaceValueOf )

import Tokenomia.Adapter.Cardano.CLI.UTxO
    ( UTxO(UTxO, txOutRef, value) )
import Tokenomia.Adapter.Cardano.CLI.Serialise ( FromCLI(fromCLI) ) 

tests :: TestTree
tests = testGroup "UTxOs" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "fromCLI cardano-cli UTxOs" $
     fromCLI [qc|
             TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
372c9b77046abc5e1266285eb33db5c4c07c53f5f28d0a05e16a9a8151a82c96     0        994156452 lovelace + TxOutDatumHashNone
372c9b77046abc5e1266285eb33db5c4c07c53f5f28d0a05e16a9a8151a82c96     2        1344798 lovelace + 489999979900 bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c.CLAP + TxOutDatumHashNone
3f2c36bed71474b2f18642efeb4db3890c984304d2b723169264552d97446783     1        2000000 lovelace + TxOutDatumHashNone
7ab72a8b4fe1128de66e5d95577c0ba213033cbe7153061bf366a5d108c2bb13     0        994933314 lovelace + TxOutDatumHashNone

      |] @?=  [ UTxO {txOutRef = TxOutRef "372c9b77046abc5e1266285eb33db5c4c07c53f5f28d0a05e16a9a8151a82c96" 0
                            , value = lovelaceValueOf 994156452 } 
                   , UTxO {txOutRef = TxOutRef "372c9b77046abc5e1266285eb33db5c4c07c53f5f28d0a05e16a9a8151a82c96" 2
                            , value = lovelaceValueOf 1344798 <>  singleton "bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c" "CLAP" 489999979900 }
                   , UTxO {txOutRef = TxOutRef "3f2c36bed71474b2f18642efeb4db3890c984304d2b723169264552d97446783" 1
                            , value = lovelaceValueOf 2000000 }
                   , UTxO {txOutRef = TxOutRef "7ab72a8b4fe1128de66e5d95577c0ba213033cbe7153061bf366a5d108c2bb13" 0
                            , value = lovelaceValueOf 994933314 }]
  ]




