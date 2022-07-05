{-# OPTIONS_GHC -Wno-orphans              #-}

module Tokenomia.Common.Arbitrary.Builtins
    () where

import PlutusTx.Builtins.Internal
    ( BuiltinByteString(..) )

import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty.QuickCheck
    ( Arbitrary
    , arbitrary
    , resize
    , shrink
    )


instance Arbitrary BuiltinByteString where
    arbitrary = BuiltinByteString <$> resize 64 arbitrary
    shrink x
        |  x == mempty = mempty
        |  otherwise   = pure mempty
