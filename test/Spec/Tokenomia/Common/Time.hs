{-# LANGUAGE ScopedTypeVariables            #-}

module Spec.Tokenomia.Common.Time
    ( tests
    ) where

import Data.Default                         ( def )
import Ledger                               ( POSIXTime, Slot(..) )
import Ledger.TimeSlot                      ( posixTimeToEnclosingSlot, slotToBeginPOSIXTime )

import Test.Tasty.QuickCheck                ( testProperty )
import Test.Tasty                           ( TestTree, testGroup )

import Tokenomia.Common.Arbitrary.POSIXTime ( )
import Tokenomia.Common.Arbitrary.Slot      ( )

import Tokenomia.Common.Time
    ( posixTimeToEnclosingSlotNo
    , slotAfterNextBeginPOSIXTime
    , toNextBeginPOSIXTime
    )


tests :: TestTree
tests = testGroup "Common.Time" [ properties ]

propertiesPosixTimeToEnclosingSlotNo :: [TestTree]
propertiesPosixTimeToEnclosingSlotNo =
    [ testProperty "preserve order"
        ( \(a :: POSIXTime) (b :: POSIXTime) ->
                let na = posixTimeToEnclosingSlotNo a
                    nb = posixTimeToEnclosingSlotNo b
                in
                    elem (compare na nb) [EQ, compare a b]
        )
    ]

propertiesSlotAfterNextBeginPOSIXTime :: [TestTree]
propertiesSlotAfterNextBeginPOSIXTime =
    [ testProperty "preserve order"
        ( \(a :: POSIXTime) (b :: POSIXTime) ->
                let na = slotAfterNextBeginPOSIXTime a
                    nb = slotAfterNextBeginPOSIXTime b
                in
                    elem (compare na nb) [EQ, compare a b]
        )
    , testProperty "is after enclosing slot"
        ( \(a :: POSIXTime) ->
                posixTimeToEnclosingSlot def a <= slotAfterNextBeginPOSIXTime a
        )
    ]

propertiesToNextBeginPOSIXTime :: [TestTree]
propertiesToNextBeginPOSIXTime =
    [ testProperty "is a future time"
        ( \(time :: POSIXTime) ->
                time <= toNextBeginPOSIXTime time
        )
    , testProperty "is a slot starting time"
        ( \(time :: POSIXTime) ->
                let begin = toNextBeginPOSIXTime time
                    n = posixTimeToEnclosingSlot def begin
                in
                    begin == slotToBeginPOSIXTime def n
        )
    , testProperty "is an identity of slots starting time"
        ( \(n :: Slot) ->
                let begin = slotToBeginPOSIXTime def n
                in
                    begin == toNextBeginPOSIXTime begin
        )
    ]

properties :: TestTree
properties = testGroup "Properties"
    [ testGroup "posixTimeToEnclosingSlotNo"    propertiesPosixTimeToEnclosingSlotNo
    , testGroup "slotAfterNextBeginPOSIXTime"   propertiesSlotAfterNextBeginPOSIXTime
    , testGroup "toNextBeginPOSIXTime"          propertiesToNextBeginPOSIXTime
    ]
