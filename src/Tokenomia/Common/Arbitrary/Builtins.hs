{-# LANGUAGE DerivingStrategies             #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# OPTIONS_GHC -Wno-orphans                #-}

module Tokenomia.Common.Arbitrary.Builtins
    ( Hex(..)
    , fromHexString
    , toHexString
    , vectorOfHexBytes
    ) where


import Data.List.Split                      ( chunksOf )

import PlutusTx.Builtins.Internal           ( BuiltinByteString(..) )

import Tokenomia.Common.Arbitrary.Utils     ( shrinkListStructure )
import Tokenomia.Common.Data.Convertible    ( convert )
import Tokenomia.Common.Data.ByteString     ( unsafeDecodeHex, encode )

import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty.QuickCheck
    ( Arbitrary
    , Gen
    , arbitrary
    , elements
    , listOf
    , shrink
    , vectorOf
    )


newtype Hex a
    =   Hex { unHex :: a }
    deriving stock ( Show, Eq )

instance Arbitrary (Hex String) where
    arbitrary =
        Hex . concat <$> listOf (vectorOf 2 arbitraryHexSymbol)

    shrink (Hex xs) =
        Hex . concat <$> shrinkListStructure (chunksOf 2 xs)

instance Arbitrary BuiltinByteString where
    arbitrary = fromHexString <$> arbitrary
    shrink x  = fromHexString <$> shrink (toHexString x)

arbitraryHexSymbol :: Gen Char
arbitraryHexSymbol = elements $ ['0'..'9'] ++ ['a' .. 'f']

fromHexString :: Hex String -> BuiltinByteString
fromHexString = convert . unsafeDecodeHex . convert . unHex

toHexString :: BuiltinByteString -> Hex String
toHexString = Hex . convert . encode . convert

vectorOfHexBytes :: Int -> Gen String
vectorOfHexBytes n =
    vectorOf (2 * n) arbitraryHexSymbol
