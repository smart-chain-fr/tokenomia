{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE OverloadedStrings                         #-}

module Main
    ( main
    ) where

import Spec.Tokenomia.CardanoApi.Fees qualified
import Spec.Tokenomia.CardanoApi.FromPlutus.Value qualified
import Spec.Tokenomia.Common.Arbitrary.Builtins qualified
import Spec.Tokenomia.Common.Arbitrary.Utils qualified
import Spec.Tokenomia.Common.Data.List.Extra qualified
import Spec.Tokenomia.Common.Parser.Address qualified
import Spec.Tokenomia.Common.Time qualified
import Spec.Tokenomia.Common.Value qualified
import Spec.Tokenomia.Vesting.GenerateNative qualified
import Spec.Tokenomia.Vesting.Sendings qualified
import Spec.Tokenomia.Wallet.UTxO qualified
import Test.Tasty                                      ( TestTree, defaultMain, testGroup )

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
