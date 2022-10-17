{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE RankNTypes                   #-}
{-# LANGUAGE KindSignatures               #-}

module Tokenomia.Common.Parser.Address
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
    , fromCardanoAddressInEra
    , toCardanoAddressInEra
    )

import Cardano.Chain.Common     ( decodeAddressBase58 )

import Cardano.Api.Byron qualified as Bryon
    ( Address(ByronAddress) )

import Cardano.Api
    ( AsType(AsAddressInEra, AsBabbageEra, AsByronEra)
    , IsCardanoEra
    , NetworkId
    , AddressInEra(AddressInEra)
    , AddressTypeInEra(ByronAddressInAnyEra)
    , BabbageEra
    , deserialiseAddress
    , serialiseAddress
    )

import Tokenomia.Common.Data.Convertible ( convert )


deserialiseAddressInEra
    :: forall (era :: Type). IsCardanoEra era
    => AsType era -> Text -> Either Text Address
deserialiseAddressInEra era address =
    maybeToRight "deserialisation failed" $ fromCardanoAddressInEra
        <$> deserialiseAddress (AsAddressInEra era) address

deserialiseCardanoAddress :: Text -> Either Text Address
deserialiseCardanoAddress address
    | "addr" `isPrefixOf` address = deserialiseAddressInEra AsBabbageEra address
    | otherwise                   = deserialiseAddressInEra AsByronEra  address

serialiseBabbageAddress :: NetworkId -> Address -> Either Text (AddressInEra BabbageEra)
serialiseBabbageAddress networdId address =
    mapLeft showError $
        toCardanoAddressInEra networdId address
  where
    showError :: ToCardanoError -> Text
    showError err =
           (convert . show . pretty $ err)
        <> (convert . show $ address)

serialiseByronAddress :: Address -> Either Text (AddressInEra BabbageEra)
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
        serialiseAddress <$> serialiseBabbageAddress networkId address

unsafeSerialiseCardanoAddress :: NetworkId -> Address -> Text
unsafeSerialiseCardanoAddress networkId address =
    either (error . convert) id $
        serialiseCardanoAddress networkId address
