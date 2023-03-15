{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE ScopedTypeVariables                       #-}

module Spec.Tokenomia.Common.Arbitrary.Builtins
    ( tests
    ) where

import Data.ByteString qualified
    as ByteString                                      ( length )
import PlutusTx.Builtins.Internal                      ( BuiltinByteString(..) )

import Test.Tasty                                      ( TestTree, testGroup )
import Test.Tasty.QuickCheck                           ( getSize, shrink, testProperty )

import Test.QuickCheck.Modifiers                       ( Positive(..) )

import Tokenomia.Common.Arbitrary.Builtins             ( Hex(..), fromHexString, toHexString, vectorOfHexBytes )
import Tokenomia.Common.Arbitrary.Utils                ( inBijection )
import Tokenomia.Common.Data.ByteString                ( encode )
import Tokenomia.Common.Data.Convertible               ( convert )


tests :: TestTree
tests = testGroup "Common.Arbitrary.Builtins" [ properties ]

builtinByteStringLength :: BuiltinByteString -> Int
builtinByteStringLength = ByteString.length . encode . convert

propertiesHexString :: [TestTree]
propertiesHexString =
    [ testProperty "vectorOfHexBytes length"
        ( \(Positive n) ->
                do
                    xs <- vectorOfHexBytes n
                    pure $ 2 * n == length xs
        )
    , testProperty "arbitrary length"
        ( \(xs :: Hex String) ->
                do
                    n <- getSize
                    pure $ 2 * n >= (length .unHex  $ xs)
        )
    , testProperty "all shrinks length"
        ( \(xs :: Hex String) ->
                let shrinksLength = length . unHex <$> shrink xs
                in
                    do
                        n <- getSize
                        pure $ all (2 * (n - 1) >=) shrinksLength
        )
    ]

propertiesBuiltinByteString :: [TestTree]
propertiesBuiltinByteString =
    [ testProperty "inBijection fromHexString toHexString" $
        inBijection fromHexString toHexString
    , testProperty "arbitrary length"
        ( \(xs :: BuiltinByteString) ->
                do
                    n <- getSize
                    pure $ 2 * n >= builtinByteStringLength xs
        )
    , testProperty "all shrinks length"
        ( \(xs :: BuiltinByteString) ->
                let shrinksLength = builtinByteStringLength <$> shrink xs
                in
                    do
                        n <- getSize
                        pure $ all (2 * n >=) shrinksLength
        )
    ]

properties :: TestTree
properties = testGroup "Properties"
    [ testGroup "HexString" propertiesHexString
    , testGroup "BuiltinByteString" propertiesBuiltinByteString
    ]
