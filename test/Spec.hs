{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

-- import qualified Spec.Tokenomia.Token.CLAPStyle.MonetaryPolicy 
-- import qualified Spec.Tokenomia.Vesting.Contract 
-- import qualified Spec.Tokenomia.Wallet.UTxO
-- import qualified Spec.Tokenomia.Common.Value
import qualified Spec.Tokenomia.ICO.Funds.Exchange.Plan
import qualified Spec.Tokenomia.ICO.Funds.Reception.Investor.Plan
import           Test.Tasty
import           Test.Tasty.Hedgehog       (HedgehogTestLimit (..))

main :: IO ()
main = defaultMain tests

-- | Number of successful tests for each hedgehog property.
--   The default is 100 but we use a smaller number here in order to speed up
--   the test suite.
--
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests = localOption limit $ testGroup "use cases" [
    -- Spec.Tokenomia.Token.CLAPStyle.MonetaryPolicy.tests,
    -- Spec.Tokenomia.Vesting.Contract.tests,
    -- Spec.Tokenomia.Wallet.UTxO.tests ,
    -- Spec.Tokenomia.Common.Value.tests,
    -- Spec.Tokenomia.ICO.Funds.Exchange.Plan.tests,
    Spec.Tokenomia.ICO.Funds.Reception.Investor.Plan.tests
    ]
