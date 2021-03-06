{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE KindSignatures               #-}

module Tokenomia.TokenDistribution.Parser.Address
    ( deserialiseCardanoAddress
    , serialiseCardanoAddress
    , unsafeSerialiseCardanoAddress
    ) where

import Ledger.Address           ( Address(..) )
import Ledger.Credential        ( Credential(..) )
import Ledger.Crypto            ( PubKeyHash(PubKeyHash) )

import Data.ByteArray           ( length )
import Data.Text                ( Text, isPrefixOf )
import Data.Kind                ( Type )
import Data.Either.Combinators  ( mapLeft, maybeToRight )

import PlutusCore.Pretty        ( Pretty(pretty) )

import PlutusTx.Prelude         ( fromBuiltin )

import Prelude           hiding ( length )

import Plutus.Contract.CardanoAPI
    ( ToCardanoError
    , fromCardanoAddress
    , toCardanoAddress
    )

import Cardano.Chain.Common     ( decodeAddressBase58 )

import Cardano.Api.Byron qualified as Bryon
    ( Address(ByronAddress) )

import Cardano.Api
    ( AsType(AsAddressInEra, AsAlonzoEra, AsByronEra)
    , IsCardanoEra
    , NetworkId
    , AddressInEra(AddressInEra)
    , AddressTypeInEra(ByronAddressInAnyEra)
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

serialiseByronAddress :: Address -> Either Text (AddressInEra AlonzoEra)
serialiseByronAddress (Address (PubKeyCredential (PubKeyHash bytes)) _) = do
    base58 <-
        mapLeft (convert . show) $
            decodeAddressBase58 $ convert $ fromBuiltin bytes
    pure $ AddressInEra ByronAddressInAnyEra (Bryon.ByronAddress base58)
serialiseByronAddress _ = Left "Invalid Byron address"

serialiseCardanoAddress :: NetworkId -> Address -> Either Text Text
serialiseCardanoAddress _ address@(Address (PubKeyCredential (PubKeyHash bytes)) _)
  | length bytes > 28 =
        serialiseAddress <$> serialiseByronAddress address
serialiseCardanoAddress networkId address =
        serialiseAddress <$> serialiseAlonzoAddress networkId address

unsafeSerialiseCardanoAddress :: NetworkId -> Address -> Text
unsafeSerialiseCardanoAddress networkId address =
    either (error . convert) id $
        serialiseCardanoAddress networkId address
