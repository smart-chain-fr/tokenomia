{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE KindSignatures               #-}

module Tokenomia.TokenDistribution.Parser.Address ( deserialiseCardanoAddress ) where

import Ledger.Address           ( Address(..) )

import Data.Text                ( Text, isPrefixOf )
import Data.Kind                ( Type )
import Data.Either.Combinators  ( mapLeft, maybeToRight )

import Plutus.Contract.CardanoAPI
    ( fromCardanoAddress )

import Cardano.Api
    ( AsType (AsAddressInEra, AsAlonzoEra, AsByronEra)
    , IsCardanoEra
    , deserialiseAddress 
    )

deserialiseAddressInEra
    :: forall (era :: Type). IsCardanoEra era
    => AsType era -> Text -> Either Text Address
deserialiseAddressInEra era address = do
    cardanoAddress <- maybeToRight "deserialisation failed" $
        deserialiseAddress (AsAddressInEra era) address
    mapLeft (const "not a cardano address") $
        fromCardanoAddress cardanoAddress

deserialiseCardanoAddress :: Text -> Either Text Address
deserialiseCardanoAddress address
    | "addr" `isPrefixOf` address = deserialiseAddressInEra AsAlonzoEra address
    | otherwise                   = deserialiseAddressInEra AsByronEra  address