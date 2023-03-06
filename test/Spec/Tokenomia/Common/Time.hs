{-# LANGUAGE ScopedTypeVariables            #-}

module Spec.Tokenomia.Common.Time
    ( tests
    ) where

import Data.Either.Combinators              ( fromRight' )

import Control.Monad.Except                 ( ExceptT, runExceptT )
import Control.Monad.Reader                 ( ReaderT, runReaderT )

import Test.QuickCheck.Monadic              ( PropertyM, monadicIO )
import Test.Tasty.QuickCheck                ( Property, Testable, testProperty )
import Test.Tasty                           ( TestTree, testGroup )

import Cardano.Api                          ( SlotNo(..) )

import Ouroboros.Consensus.BlockchainTime.WallClock.Types
    ( RelativeTime(..) )

import Tokenomia.CardanoApi.Arbitrary.Time  ( )
import Tokenomia.CardanoApi.Arbitrary.Slot  ( )
import Tokenomia.CardanoApi.Query           ( querySlotToWallclock', queryWallclockToSlot')

import Tokenomia.Common.Error               ( TokenomiaError(QueryFailure) )
import Tokenomia.Common.Environment         ( Environment(..), getTestnetEnvironmment )
import Tokenomia.Common.Environment.Query   ( evalQuery )

import Tokenomia.Common.Time
    ( slotAfterNextBeginRelativeTime
    , toNextBeginRelativeTime
    )


tests :: TestTree
tests = testGroup "Common.Time" [ properties ]

queryFailure :: Show a => a -> TokenomiaError
queryFailure = QueryFailure . show

runTest ::
     ( Testable a )
    => ReaderT Environment (ExceptT e (PropertyM IO)) a
    -> Property
runTest test =
    monadicIO $
        do
            env <- getTestnetEnvironmment 1
            fromRight' <$> runExceptT (runReaderT test env)

propertiesQueryWallclockToSlot' :: [TestTree]
propertiesQueryWallclockToSlot' =
    [ testProperty "preserve order"
        ( \(a :: RelativeTime) (b :: RelativeTime) ->
                runTest $
                    do
                        na <- evalQuery queryFailure queryWallclockToSlot' a
                        nb <- evalQuery queryFailure queryWallclockToSlot' b
                        pure $ elem (compare na nb) [EQ, compare a b]
        )
    ]

propertiesSlotAfterNextBeginRelativeTime :: [TestTree]
propertiesSlotAfterNextBeginRelativeTime  =
    [ testProperty "preserve order"
        ( \(a :: RelativeTime) (b :: RelativeTime) ->
                runTest $
                    do
                        na <- evalQuery queryFailure slotAfterNextBeginRelativeTime a
                        nb <- evalQuery queryFailure slotAfterNextBeginRelativeTime b
                        pure $ elem (compare na nb) [EQ, compare a b]
        )
    , testProperty "is after enclosing slot"
        ( \(a :: RelativeTime) ->
                runTest $
                    do
                        n  <- evalQuery queryFailure queryWallclockToSlot' a
                        na <- evalQuery queryFailure slotAfterNextBeginRelativeTime a
                        pure $ n <= na
        )
    ]

propertiesToNextBeginRelativeTime :: [TestTree]
propertiesToNextBeginRelativeTime =
    [ testProperty "is a future time"
        ( \(time :: RelativeTime) ->
                runTest $
                    do
                        begin <- evalQuery queryFailure toNextBeginRelativeTime time
                        pure $ time <= begin
        )
    , testProperty "is a slot starting time"
        ( \(time :: RelativeTime) ->
                runTest $
                    do
                        begin <- evalQuery queryFailure toNextBeginRelativeTime time
                        n <- evalQuery queryFailure queryWallclockToSlot' begin
                        t <- evalQuery queryFailure querySlotToWallclock' n
                        pure $ begin == t
        )
    , testProperty "is an identity of slots starting time"
        ( \(n :: SlotNo) ->
                runTest $
                    do
                        t <- evalQuery queryFailure querySlotToWallclock' n
                        begin <- evalQuery queryFailure toNextBeginRelativeTime t
                        pure $ begin == t
        )
    ]

properties :: TestTree
properties = testGroup "Properties"
    [ testGroup "queryWallclockToSlot'" propertiesQueryWallclockToSlot'
    , testGroup "slotAfterNextBeginRelativeTime" propertiesSlotAfterNextBeginRelativeTime
    , testGroup "toNextBeginRelativeTime" propertiesToNextBeginRelativeTime
    ]
