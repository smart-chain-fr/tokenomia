{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Tokenomia.Wallet.UTxO (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.InterpolatedString.Perl6 (qc)

import Ledger (TxOutRef (..))
import Ledger.Ada (lovelaceValueOf)
import Plutus.V1.Ledger.Value (singleton)
import Tokenomia.Common.Hash

import Tokenomia.Wallet.UTxO

import Tokenomia.Common.Serialise

tests :: TestTree
tests = testGroup "UTxOs" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ testCase "fromCLI cardano-cli UTxOs" $
        fromCLI
          [qc|
             TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
372c9b77046abc5e1266285eb33db5c4c07c53f5f28d0a05e16a9a8151a82c96     0        994156452 lovelace + TxOutDatumNone
372c9b77046abc5e1266285eb33db5c4c07c53f5f28d0a05e16a9a8151a82c96     2        1379280 lovelace + 489999979900 bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c.434c4150 + TxOutDatumNone
3f2c36bed71474b2f18642efeb4db3890c984304d2b723169264552d97446783     1        2000000 lovelace + TxOutDatumNone
7ab72a8b4fe1128de66e5d95577c0ba213033cbe7153061bf366a5d108c2bb13     0        994933314 lovelace + TxOutDatumNone
42e5d56fe31a9ee9bc83b6b88c2254952d9e477ca46e40dc985fe041feec50f2    10        6000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "faf44f2aa43aa67e7a8b7e8c24465515dcf86ed3780c70779e6ac13cd68f3060"

      |]
          @?= [ UTxO
                  { txOutRef = TxOutRef "372c9b77046abc5e1266285eb33db5c4c07c53f5f28d0a05e16a9a8151a82c96" 0
                  , value = lovelaceValueOf 994156452
                  , maybeDatumHash = Nothing
                  }
              , UTxO
                  { txOutRef = TxOutRef "372c9b77046abc5e1266285eb33db5c4c07c53f5f28d0a05e16a9a8151a82c96" 2
                  , value = lovelaceValueOf 1379280 <> singleton "bb71084cb088b07943080a6fd4dc42eb1196d12de663526b5cdf8c5c" "CLAP" 489999979900
                  , maybeDatumHash = Nothing
                  }
              , UTxO
                  { txOutRef = TxOutRef "3f2c36bed71474b2f18642efeb4db3890c984304d2b723169264552d97446783" 1
                  , value = lovelaceValueOf 2000000
                  , maybeDatumHash = Nothing
                  }
              , UTxO
                  { txOutRef = TxOutRef "7ab72a8b4fe1128de66e5d95577c0ba213033cbe7153061bf366a5d108c2bb13" 0
                  , value = lovelaceValueOf 994933314
                  , maybeDatumHash = Nothing
                  }
              , UTxO
                  { txOutRef = TxOutRef "42e5d56fe31a9ee9bc83b6b88c2254952d9e477ca46e40dc985fe041feec50f2" 10
                  , value = lovelaceValueOf 6000000
                  , maybeDatumHash = Just $ Hash "faf44f2aa43aa67e7a8b7e8c24465515dcf86ed3780c70779e6ac13cd68f3060"
                  }
              ]
    ]
