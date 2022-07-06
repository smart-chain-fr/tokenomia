{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Spec.Tokenomia.Common.Value qualified

-- import Spec.Tokenomia.ICO.Funds.Exchange.Plan qualified
-- import Spec.Tokenomia.ICO.Funds.Validation.CardanoCLI.Plan qualified
-- import Spec.Tokenomia.ICO.Funds.Validation.Investor.Plan qualified
import Spec.Tokenomia.Wallet.UTxO qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "use cases"
    [ Spec.Tokenomia.Wallet.UTxO.tests
    , Spec.Tokenomia.Common.Value.tests
    -- , Spec.Tokenomia.ICO.Funds.Exchange.Plan.tests
    -- , Spec.Tokenomia.ICO.Funds.Validation.Investor.Plan.tests
    -- , Spec.Tokenomia.ICO.Funds.Validation.CardanoCLI.Plan.tests
    ]
