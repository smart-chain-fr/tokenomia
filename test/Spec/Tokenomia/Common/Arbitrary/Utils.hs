{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE TypeApplications               #-}

module Spec.Tokenomia.Common.Arbitrary.Utils
    ( tests
    ) where

import Data.Char                            ( toUpper, toLower )
import Test.Tasty.QuickCheck                ( testProperty, expectFailure )
import Test.Tasty                           ( TestTree, testGroup )

import Tokenomia.Common.Arbitrary.Utils     ( isIdentity, inBijection )


tests :: TestTree
tests = testGroup "Common.Arbitrary.Utils" [ properties ]

propertiesIsIdentity :: [TestTree]
propertiesIsIdentity =
    [ testProperty "id isIdentity" $
        isIdentity @Integer id
    ]

propertiesInBijection :: [TestTree]
propertiesInBijection =
    [ testProperty "inBijection id id" $
        inBijection @Integer id id
    , testProperty "inBijection (+1) (-1)" $
        inBijection @Integer (+1) (subtract 1)
    , testProperty "not inBijection toUpper toLower" $
        expectFailure $ inBijection toUpper toLower
    ]

properties :: TestTree
properties = testGroup "Properties"
    [ testGroup "isIdentity" propertiesIsIdentity
    , testGroup "inBijection" propertiesInBijection
    ]
