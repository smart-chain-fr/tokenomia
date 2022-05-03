{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE KindSignatures               #-}

module Tokenomia.TokenDistribution.Parser.Address
    ( deserialiseCardanoAddress
    , serialiseCardanoAddress
    , unsafeSerialiseCardanoAddress
    ) where

import Ledger.Address           ( Address(..) )

import Data.Text                ( Text, isPrefixOf )
import Data.Kind                ( Type )
import Data.Either.Combinators  ( mapLeft, maybeToRight )

import PlutusCore.Pretty        ( Pretty(pretty) )

import Plutus.Contract.CardanoAPI
    ( ToCardanoError
    , fromCardanoAddress
    , toCardanoAddress
    )

import Cardano.Api
    ( AsType (AsAddressInEra, AsAlonzoEra, AsByronEra)
    , IsCardanoEra
    , NetworkId
    , AddressInEra
    , AlonzoEra
    , deserialiseAddress
    , serialiseAddress
    )

import Tokenomia.Common.Data.Convertible ( convert )


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

serialiseAlonzoAddress :: NetworkId -> Address -> Either Text (AddressInEra AlonzoEra)
serialiseAlonzoAddress networdId address =
    mapLeft showError $
        toCardanoAddress networdId address
  where
    showError :: ToCardanoError -> Text
    showError err =
           (convert . show . pretty $ err)
        <> (convert . show $ address)

serialiseCardanoAddress :: NetworkId -> Address -> Either Text Text
serialiseCardanoAddress networkId address =
    serialiseAddress <$> serialiseAlonzoAddress networkId address

unsafeSerialiseCardanoAddress :: NetworkId -> Address -> Text
unsafeSerialiseCardanoAddress networkId address =
    either (error . convert) id $
        serialiseCardanoAddress networkId address
