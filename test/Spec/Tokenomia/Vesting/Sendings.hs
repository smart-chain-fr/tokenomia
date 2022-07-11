{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Spec.Tokenomia.Vesting.Sendings (tests, mySendings, main) where

import Data.ByteString.Lazy qualified as ByteString

import Test.Tasty (TestTree, testGroup)
import Tokenomia.Common.Error (TokenomiaError)
import Tokenomia.Vesting.Sendings (MonadRunBlockfrost (getAddressTransactions, getTxUtxos), Sendings, jsonToSendings, verifySendings')

import Data.Kind (Type)

import Control.Monad.Except (MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (IdentityT (IdentityT))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)

import Blockfrost.Types (
  Address (Address),
  AddressTransaction (AddressTransaction),
  Amount (AdaAmount),
  TransactionUtxos (TransactionUtxos),
  TxHash (TxHash),
  UtxoOutput (UtxoOutput),
 )

data BlockfrostMockData = BlockfrostMockData
  { addressTransactions :: [AddressTransaction]
  , transactionUtxos :: TransactionUtxos
  }

testBlockfrostMockData :: BlockfrostMockData
testBlockfrostMockData =
  BlockfrostMockData
    { addressTransactions = []
    , transactionUtxos = TransactionUtxos txHash input output
    }
  where
    output = undefined
    input = []
    txHash = TxHash "75d39ec2fd731ea9ef284eac3ceaa8191cc70f97b95194c5ab5a4985792047fd"

newtype FakeBlockfrost (m :: Type -> Type) (a :: Type) = FakeBlockfrost {runFakeBlockfrost :: m a}
  deriving (Functor, Applicative, Monad) via IdentityT m

deriving via (IdentityT m) instance (MonadReader BlockfrostMockData m) => MonadReader BlockfrostMockData (FakeBlockfrost m)
deriving via (IdentityT m) instance (MonadError TokenomiaError m) => MonadError TokenomiaError (FakeBlockfrost m)

instance (Monad m, MonadReader BlockfrostMockData m, MonadError TokenomiaError m) => MonadRunBlockfrost (FakeBlockfrost m) where
  getAddressTransactions _ = asks addressTransactions
  getTxUtxos _ = asks transactionUtxos

main :: IO ()
main = either print (const $ print @String "pass") =<< sendingsTestCase
  where
    {- sendingsTestCase = sendingsPass -}
    sendingsTestCase = sendingsFail

testSendings :: MonadIO m => BlockfrostMockData -> Sendings -> m (Either TokenomiaError ())
testSendings bfmd s =
  runExceptT $ runReaderT (runFakeBlockfrost $ verifySendings' s) bfmd

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

tests :: Sendings -> TestTree
tests _ =
  testGroup
    "Vesting Sendings"
    []

mySendings ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  ) =>
  m Sendings
mySendings = liftIO (ByteString.readFile "./test/Spec/Tokenomia/Vesting/sendings.json") >>= jsonToSendings
