{-# LANGUAGE OverloadedStrings #-}
module Main(main) where


import qualified Spec.Tokenomia.Wallet.UTxO
import qualified Spec.Tokenomia.Common.Value
import qualified Spec.Tokenomia.ICO.Funds.Exchange.Plan
import qualified Spec.Tokenomia.ICO.Funds.Reception.Investor.Plan
import qualified Spec.Tokenomia.ICO.Funds.Reception.CardanoCLI.Plan
import Test.Tasty ( TestTree, defaultMain, testGroup )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "use cases" [
    Spec.Tokenomia.Wallet.UTxO.tests ,
    Spec.Tokenomia.Common.Value.tests,
    Spec.Tokenomia.ICO.Funds.Exchange.Plan.tests,
    Spec.Tokenomia.ICO.Funds.Reception.Investor.Plan.tests,
    Spec.Tokenomia.ICO.Funds.Reception.CardanoCLI.Plan.tests
    ]
