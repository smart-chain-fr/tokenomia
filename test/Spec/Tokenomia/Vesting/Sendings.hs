{-# LANGUAGE DataKinds                                 #-}
{-# LANGUAGE DerivingVia                               #-}
{-# LANGUAGE ExplicitForAll                            #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE FlexibleInstances                         #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE KindSignatures                            #-}
{-# LANGUAGE MultiParamTypeClasses                     #-}
{-# LANGUAGE OverloadedStrings                         #-}
{-# LANGUAGE StandaloneDeriving                        #-}
{-# LANGUAGE UndecidableInstances                      #-}
{-# OPTIONS_GHC -Werror                                #-}
{-# OPTIONS_GHC -Wno-unused-top-binds                  #-}

module Spec.Tokenomia.Vesting.Sendings
    ( main
    , tests
    ) where

import Blockfrost.Client.Types                         ( BlockfrostError(BlockfrostNotFound) )
import Blockfrost.Types
    ( Address(Address)
    , AddressTransaction(AddressTransaction)
    , Amount(AdaAmount, AssetAmount)
    , TransactionUtxos(TransactionUtxos)
    , TxHash(TxHash)
    , UtxoOutput(UtxoOutput)
    )
import Control.Monad.Except                            ( MonadError, liftEither, runExceptT )
import Control.Monad.Identity                          ( Identity(runIdentity), IdentityT(IdentityT) )
import Control.Monad.IO.Class                          ( MonadIO(liftIO) )
import Control.Monad.State                             ( MonadState, evalStateT, gets )
import Data.ByteString.Lazy qualified as ByteString
import Data.Either                                     ( isLeft, isRight )
import Data.Either.Combinators                         ( maybeToRight )
import Data.Hex                                        ( hex )
import Data.Kind                                       ( Type )
import Data.List.NonEmpty                              ( NonEmpty )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map                                        ( Map )
import Data.Map qualified as Map
import Data.Map.NonEmpty qualified as NEMap
import Data.Maybe                                      ( fromJust )
import Data.String                                     ( IsString(fromString) )
import Data.Text                                       ( Text, pack )
import Ledger                                          ( Value )
import Ledger.Ada                                      ( lovelaceValueOf )
import Ledger.Value                                    ( assetClass, assetClassValue )
import Money                                           ( mkSomeDiscrete, scaleFromRational )
import Test.Tasty                                      ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit                                ( assertBool, testCase )
import Tokenomia.Common.Error                          ( TokenomiaError(BlockFrostError) )
import Tokenomia.Vesting.Sendings
    ( MonadRunBlockfrost(getAddressTransactions, getTxUtxos)
    , Sendings(Sendings)
    , jsonToSendings
    , verifySendings'
    )

data BlockfrostMockData = BlockfrostMockData
  { addressTransactions :: [AddressTransaction]
  , transactionUtxos :: TransactionUtxos
  }

type TestState = (Map Address [AddressTransaction], Map TxHash TransactionUtxos)

-- Suffecient data for a single test case:
-- 1. Input sendings
-- 2. Blockfrost mock response
type TestData = (Sendings, TestState)

newtype FakeBlockfrost (m :: Type -> Type) (a :: Type) = FakeBlockfrost {runFakeBlockfrost :: m a}
  deriving (Functor, Applicative, Monad) via IdentityT m

deriving via (IdentityT m) instance (MonadState a m) => MonadState a (FakeBlockfrost m)
deriving via (IdentityT m) instance (MonadError e m) => MonadError e (FakeBlockfrost m)

instance (Monad m, MonadState TestState m, MonadError TokenomiaError m) => MonadRunBlockfrost (FakeBlockfrost m) where
  getAddressTransactions addr = do
    atsMap <- gets fst
    liftEither $ maybeToRight (BlockFrostError BlockfrostNotFound) (atsMap Map.!? addr)
  getTxUtxos txh = do
    txMap <- gets snd
    liftEither $ maybeToRight (BlockFrostError BlockfrostNotFound) $ txMap Map.!? txh

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Vesting Sendings"
    [invalidInputTests, validTxHashTests, valueCheckTests]

mySendings ::
  forall (m :: Type -> Type).
  ( MonadIO m
  , MonadError TokenomiaError m
  ) =>
  m Sendings
mySendings = liftIO (ByteString.readFile "./test/Spec/Tokenomia/Vesting/sendings.json") >>= jsonToSendings

testBuilder :: TestData -> Either TokenomiaError ()
testBuilder (sendings, testState) =
  runIdentity . runExceptT $ evalStateT (runFakeBlockfrost $ verifySendings' sendings) testState

invalidInputTests :: TestTree
invalidInputTests =
  testGroup
    "Invalid Input Tests"
    [ testCase
        "Test whether malformed address fails verification"
        (assertBool ("Failed Left assertion on " <> show failTest) (isLeft failTest))
    , testCase
        "Empty value list fails verification"
        (assertBool ("Failed Left assertion on " <> show valueTest) (isLeft valueTest))
    ]
  where
    -- malformed address case
    failTest = testBuilder $ testDataBuilder "bad_address" sendingsTxs bfAddrTxMap bfUtxos

    -- has zero value tx case
    valueTest = testBuilder $ testDataBuilder sendingsAddress emptyValueSendingsTxs bfAddrTxMap bfUtxos

    sendingsAddress = testAddress
    bfAddr = [AddressTransaction (TxHash "abcd") 0 0 0, AddressTransaction (TxHash "wxyz") 0 0 0]
    sendingsTxs =
      NonEmpty.fromList
        [ (TxHash "abcd", lovelaceValueOf 10000000)
        , (TxHash "wxyz", lovelaceValueOf 10000000)
        ]
    emptyValueSendingsTxs =
      NonEmpty.fromList
        [ (TxHash "abcd", mempty)
        , (TxHash "wxyz", lovelaceValueOf 10000000)
        ]
    bfAddrTxMap = [(Address sendingsAddress, bfAddr)]
    bfUtxos = [(TxHash "abcd", abcdUtxos), (TxHash "wxyz", wxyzUtxos), (TxHash "ffff", abcdUtxos)]
    abcdUtxos = TransactionUtxos "abcd" [] outUtxos
    wxyzUtxos = TransactionUtxos "wxyz" [] outUtxos
    outUtxos =
      [ UtxoOutput
          (Address sendingsAddress)
          [AdaAmount 10000000]
          Nothing
          0
      ]

-- Missing Tx hash tests
validTxHashTests :: TestTree
validTxHashTests =
  testGroup
    "Test Missing Tx"
    [ testCase
        "Test whether missing TxHash in Sendings input fails"
        (assertBool ("Failed Left assertion on " <> show failTest) (isLeft failTest))
    , testCase
        "Test all valid TxHash in Sendings passes verification"
        (assertBool ("Failed Right assertion on " <> show passTest) (isRight passTest))
    ]
  where
    -- All request TxHashes exist
    passTest = testBuilder $ testDataBuilder sendingsAddress sendingsTxs (bfAddrTxMap goodBfAddr) bfUtxos
    goodBfAddr = [AddressTransaction (TxHash "abcd") 0 0 0, AddressTransaction (TxHash "wxyz") 0 0 0]

    -- Some TxHash doesn't exist
    failTest = testBuilder $ testDataBuilder sendingsAddress sendingsTxs (bfAddrTxMap badBfAddr) bfUtxos
    badBfAddr = tail goodBfAddr

    sendingsAddress = testAddress
    sendingsTxs =
      NonEmpty.fromList
        [ (TxHash "abcd", lovelaceValueOf 10000000)
        , (TxHash "wxyz", lovelaceValueOf 10000000)
        ]
    bfAddrTxMap ba = [(Address sendingsAddress, ba)]
    bfUtxos = [(TxHash "abcd", abcdUtxos), (TxHash "wxyz", wxyzUtxos), (TxHash "ffff", abcdUtxos)]
    abcdUtxos = TransactionUtxos "abcd" [] outUtxos
    wxyzUtxos = TransactionUtxos "wxyz" [] outUtxos
    outUtxos =
      [ UtxoOutput
          (Address sendingsAddress)
          [AdaAmount 10000000]
          Nothing
          0
      ]

-- Malformed address

-- Value check tests
valueCheckTests :: TestTree
valueCheckTests =
  testGroup
    "Test Value Checks"
    [ testCase
        "Test whether value mismatch between sendings and BF fails"
        (assertBool ("Failed Left assertion on " <> show failTest) (isLeft failTest))
    , testCase
        "Test whether valid values passes verification"
        (assertBool ("Failed Right assertion on " <> show passTest) (isRight passTest))
    ]
  where
    -- All request TxHashes exist
    passTest = testBuilder $ testDataBuilder sendingsAddress sendingsTxs bfAddrTxMap goodBfUtxos
    goodBfUtxos = [(TxHash "abcd", abcdUtxos), (TxHash "wxyz", wxyzUtxos)]

    -- Some TxHash doesn't exist
    failTest = testBuilder $ testDataBuilder sendingsAddress sendingsTxs bfAddrTxMap badBfUtxos
    badBfUtxos = [(TxHash "abcd", abcdUtxos), (TxHash "wxyz", abcdUtxos)]

    sendingsAddress = testAddress
    cs = replicate 56 'a' -- 28 because hex will turn it into 56 chars
    tk = "TOK"
    testToken =
      assetClassValue
        ( assetClass
            (fromString cs)
            (fromString tk)
        )
        50

    sendingsTxs =
      NonEmpty.fromList
        [ (TxHash "abcd", lovelaceValueOf 15000000 <> testToken)
        , (TxHash "wxyz", lovelaceValueOf 10000000)
        ]
    bfAddrTxMap =
      [
        ( Address sendingsAddress
        ,
          [ AddressTransaction (TxHash "abcd") 0 0 0
          , AddressTransaction (TxHash "wxyz") 0 0 0
          ]
        )
      ]
    abcdUtxos = TransactionUtxos "abcd" [] abcdOutUtxos
    wxyzUtxos = TransactionUtxos "wxyz" [] wxyzOutUtxos
    abcdOutUtxos =
      [ UtxoOutput
          (Address sendingsAddress)
          [ AdaAmount 15000000
          , AssetAmount $
              mkSomeDiscrete (pack cs <> pack (hex tk)) (fromJust $ scaleFromRational 1) 50
          ]
          Nothing
          0
      ]
    wxyzOutUtxos =
      [ UtxoOutput
          (Address sendingsAddress)
          [AdaAmount 10000000]
          Nothing
          0
      ]

testAddress :: Text
testAddress = "addr_test1qzu80eg7jesd2tryfk3z2ww7fz2s40wcmxxcd43ylh7efunflufrxedaepnz8zsfadnt5h92j6k673ue9rj5mzcwvp4saxlckq"

testDataBuilder ::
  Text ->
  NonEmpty (TxHash, Value) ->
  [(Address, [AddressTransaction])] ->
  [(TxHash, TransactionUtxos)] ->
  TestData
testDataBuilder sendingsAddress sendingsTxs bfAddrTxMap bfUtxos = testData
  where
    testData = (sendings, testState) :: TestData
    sendings = Sendings address txhValueMap
    address = Address sendingsAddress
    txhValueMap = NEMap.fromList sendingsTxs
    -- Stuff Blockfrost will "return"
    testState = (Map.fromList bfAddrTxMap, Map.fromList bfUtxos)
