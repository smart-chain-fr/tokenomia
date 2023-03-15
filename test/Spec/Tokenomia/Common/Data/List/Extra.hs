{-# LANGUAGE ScopedTypeVariables                       #-}
{-# LANGUAGE TypeApplications                          #-}
{-# LANGUAGE ViewPatterns                              #-}

module Spec.Tokenomia.Common.Data.List.Extra
    ( tests
    ) where

import Data.Function                                   ( (&) )

import Test.QuickCheck.Function                        ( apply )
import Test.QuickCheck.Modifiers                       ( NonEmptyList(..) )
import Test.Tasty                                      ( TestTree, testGroup )
import Test.Tasty.QuickCheck                           ( testProperty )

import Tokenomia.Common.Data.List.Extra                ( mapLastWith, transpose )


tests :: TestTree
tests = testGroup "Common.Data.List.Extra" [ properties ]

propertiesMapLastWith :: [TestTree]
propertiesMapLastWith =
    [ testProperty "mapLastWith f g [] == []"
        ( \(apply -> f) (apply -> g) ->
                mapLastWith @Integer f g [] & null
        )
    , testProperty "mapLastWith id id == id"
        ( \(NonEmpty xs) ->
                mapLastWith @Integer id id xs == xs
        )
    , testProperty "mapLastWith f id == (map f init) ++ [last]"
        ( \(NonEmpty xs) (apply -> f) ->
                mapLastWith @Integer f id xs == map f (init xs) ++ [last xs]
        )
    , testProperty "mapLastWith id g == init ++ [g last]"
        ( \(NonEmpty xs) (apply -> g) ->
                mapLastWith @Integer id g xs == init xs ++ [g (last xs)]
        )
    , testProperty "mapLastWith f g == (map f init) ++ [g last]"
        ( \(NonEmpty xs) (apply -> f) (apply -> g) ->
                mapLastWith @Integer f g xs == map f (init xs) ++ [g (last xs)]
        )
    ]

propertiesTranspose :: [TestTree]
propertiesTranspose =
    [ testProperty "transpose [] == []"  $ transpose [] & null
    , testProperty "valid transpose length"
        ( \((NonEmpty xs) :: NonEmptyList [Integer]) ->
                length (transpose xs) == minimum (length <$> xs)
        )
    , testProperty "valid transpose elements length"
        ( \(xs :: [[Integer]]) ->
                all (== length xs) (length <$> transpose xs)
        )
    ]

properties :: TestTree
properties = testGroup "Properties"
    [ testGroup "mapLastWith"   propertiesMapLastWith
    , testGroup "transpose"     propertiesTranspose
    ]
