{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Spec.Tokenomia.Vesting.Sendings (tests, main) where

import Blockfrost.Types (
  Address (Address),
  AddressTransaction (AddressTransaction),
  Amount (AdaAmount),
  TransactionUtxos (TransactionUtxos),
  TxHash (TxHash),
  UtxoOutput (UtxoOutput),
 )
import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (IdentityT (IdentityT))
import Data.ByteString.Lazy qualified as ByteString
import Data.Either (isLeft)
import Data.Kind (Type)
import PlutusTx.Either (isRight)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCaseSteps)
import Tokenomia.Common.Error (TokenomiaError)
import Tokenomia.Vesting.Sendings (
  MonadRunBlockfrost (getAddressTransactions, getTxUtxos),
  Sendings,
  jsonToSendings,
  verifySendings',
 )
import Control.Monad.State (MonadState, modify, gets, evalStateT)

data BlockfrostMockData = BlockfrostMockData
  { addressTransactions :: [AddressTransaction]
  , transactionUtxos :: TransactionUtxos
  }

type TestState = ([[AddressTransaction]], [TransactionUtxos])

newtype FakeBlockfrost (m :: Type -> Type) (a :: Type) = FakeBlockfrost {runFakeBlockfrost :: m a}
  deriving (Functor, Applicative, Monad) via IdentityT m

deriving via (IdentityT m) instance (MonadState a m) => MonadState a (FakeBlockfrost m)
deriving via (IdentityT m) instance (MonadError e m) => MonadError e (FakeBlockfrost m)

instance (Monad m, MonadState TestState m, MonadError TokenomiaError m) => MonadRunBlockfrost (FakeBlockfrost m) where
  getAddressTransactions _ =  do
    at <- gets (head . fst)
    modify $ \(a,u) -> (tail a, u)
    pure at
  getTxUtxos _ = do
    us <- gets (head . snd)
    modify $ \(a,u) -> (a, tail u)
    pure us


testSendings :: MonadIO m => BlockfrostMockData -> Sendings -> m (Either TokenomiaError ())
testSendings bfmd s =
  runExceptT $ evalStateT (runFakeBlockfrost $ verifySendings' s) ([addressTransactions bfmd], [transactionUtxos bfmd])

sendingsFail :: IO (Either TokenomiaError ())
sendingsFail =
  runExceptT mySendings
    >>= either (return . Left) (testSendings failBlockfrostMockData)
  where
    failBlockfrostMockData =
      BlockfrostMockData
        { addressTransactions = []
        , transactionUtxos = TransactionUtxos txHash input output
        }
      where
        output = []
        input = []
        txHash = TxHash "75d39ec2fd731ea9ef284eac3ceaa8191cc70f97b95194c5ab5a4985792047fd"

sendingsPass :: IO (Either TokenomiaError ())
sendingsPass = runExceptT mySendings >>= either (return . Left) (testSendings passBlockfrostMockData)
  where
    passBlockfrostMockData =
      BlockfrostMockData
        { addressTransactions =
            [ AddressTransaction
                (TxHash "75d39ec2fd731ea9ef284eac3ceaa8191cc70f97b95194c5ab5a4985792047fd")
                0
                0
            ]
        , transactionUtxos = TransactionUtxos txHash input output
        }
      where
        output =
          [ UtxoOutput
              (Address "addr_test1qzu80eg7jesd2tryfk3z2ww7fz2s40wcmxxcd43ylh7efunflufrxedaepnz8zsfadnt5h92j6k673ue9rj5mzcwvp4saxlckq")
              [AdaAmount 10000000]
              Nothing
              0
          ]
        input = []
        txHash = TxHash "75d39ec2fd731ea9ef284eac3ceaa8191cc70f97b95194c5ab5a4985792047fd"

failCase :: TestTree
failCase = testCaseSteps "Missing transacions at address" $ \step -> do
  _ <- step "Read and parse sample sendings.json file"
  x <- runExceptT mySendings
  assertBool "Failed to parse json" (isRight x)

  step "Verify sendings"
  y <- sendingsFail
  assertBool "Verified invalid sendings" (isLeft y)

passCase :: TestTree
passCase = testCaseSteps "Pass case with single sendings Tx" $ \step -> do
  _ <- step "Read and parse sample sendings.json file"
  x <- runExceptT mySendings
  assertBool "Failed to parse json" (isRight x)

  step "Verify sendings"
  y <- sendingsPass
  assertBool "Failed to verify valid sendings" (isRight y)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Vesting Sendings"
    [failCase, passCase]

mySendings ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  ) =>
  m Sendings
mySendings = liftIO (ByteString.readFile "./test/Spec/Tokenomia/Vesting/sendings.json") >>= jsonToSendings
