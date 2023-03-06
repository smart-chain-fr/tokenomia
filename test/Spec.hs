{-# LANGUAGE OverloadedStrings #-}
module Main(main) where


import qualified Spec.Tokenomia.Vesting.GenerateNative
import qualified Spec.Tokenomia.CardanoApi.FromPlutus.Value
import qualified Spec.Tokenomia.CardanoApi.Fees
import qualified Spec.Tokenomia.Common.Arbitrary.Builtins
import qualified Spec.Tokenomia.Common.Arbitrary.Utils
import qualified Spec.Tokenomia.Common.Data.List.Extra
import qualified Spec.Tokenomia.Common.Time
import qualified Spec.Tokenomia.Wallet.UTxO
import qualified Spec.Tokenomia.Common.Value
import qualified Spec.Tokenomia.Common.Parser.Address
import qualified Spec.Tokenomia.Vesting.Sendings
import Test.Tasty ( TestTree, defaultMain, testGroup )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "use cases"
    [ Spec.Tokenomia.Wallet.UTxO.tests
    , Spec.Tokenomia.Common.Value.tests
    , Spec.Tokenomia.Vesting.Sendings.tests
    , Spec.Tokenomia.CardanoApi.Fees.tests
    , Spec.Tokenomia.CardanoApi.FromPlutus.Value.tests
    , Spec.Tokenomia.Common.Arbitrary.Builtins.tests
    , Spec.Tokenomia.Common.Arbitrary.Utils.tests
    , Spec.Tokenomia.Common.Data.List.Extra.tests
    , Spec.Tokenomia.Common.Parser.Address.tests
    , Spec.Tokenomia.Common.Time.tests
    , Spec.Tokenomia.Vesting.GenerateNative.tests
    ]
