module Spec.Tokenomia.Common.Parser.Address
    ( tests
    ) where

import Data.Either.Combinators              ( isRight )
import Data.Functor.Syntax                  ( (<$$>) )

import Test.QuickCheck.Monadic              ( monadicIO )
import Test.Tasty.QuickCheck                ( testProperty, withMaxSuccess )
import Test.Tasty                           ( TestTree, testGroup )

import Tokenomia.Common.Arbitrary.Wallet    ( PaymentAddress(..), generateAddresses )
import Tokenomia.Common.Data.Convertible    ( convert )
import Tokenomia.Common.Parser.Address      ( deserialiseCardanoAddress )


tests :: TestTree
tests = testGroup "Common.Parser.Address" [ properties ]

properties :: TestTree
properties = testGroup "Properties"
    [ testGroup "deserialiseCardanoAddress"
        [ testProperty "generated with cardano-address (testnet)" $
            withMaxSuccess 1 $ monadicIO $
                all isRight
                    <$>  deserialiseCardanoAddress . convert . unPaymentAddress
                    <$$> generateAddresses "testnet" [0..3]
        , testProperty "generated with cardano-address (mainnet)" $
            withMaxSuccess 1 $ monadicIO $
                all isRight
                    <$>  deserialiseCardanoAddress . convert . unPaymentAddress
                    <$$> generateAddresses "mainnet" [0..3]
        ]
    ]
