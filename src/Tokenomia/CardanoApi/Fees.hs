{-# LANGUAGE DerivingStrategies                        #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE TypeFamilies                              #-}

module Tokenomia.CardanoApi.Fees
    ( HasDatumHash(..)
    , calculateDefaultMinimumUTxOFromAssetId
    , calculateDefaultMinimumUTxOFromValue
    , calculateMinimumUTxOFromAssetId
    , calculateMinimumUTxOFromValue
    , utxoEntrySize
    ) where

import Cardano.Api
    ( AssetId(..)
    , Lovelace(..)
    , ShelleyBasedEra(..)
    , Value
    , selectLovelace
    , valueFromList
    )

import Cardano.Api.Shelley
    ( ProtocolParameters(..)
    , calcMinimumDeposit
    , lovelaceToValue
    , toMaryValue
    )

import Cardano.Ledger.Val qualified
    as Value                                           ( size )

import Tokenomia.CardanoApi.PParams                    ( defaultCalculateMinimumUTxOParams )
import Tokenomia.CardanoApi.Value                      ( unLovelace )


data    HasDatumHash
    =   NoDatumHash
    |   WithDatumHash
    deriving stock (Show)

-- | Utxo entry size calculation
utxoEntrySize :: Value -> HasDatumHash -> Integer
utxoEntrySize value datum =
    utxoEntrySizeWithoutVal + Value.size (toMaryValue value) + datumHashSize datum
  where
    datumHashSize :: HasDatumHash -> Integer
    datumHashSize NoDatumHash = 0
    datumHashSize WithDatumHash = 10

    utxoEntrySizeWithoutVal :: Integer
    utxoEntrySizeWithoutVal = 27

-- | Calculate minimumUTxO simply from Value instead of TxOut
calculateMinimumUTxOFromValue ::
    forall era.
       ShelleyBasedEra era
    -> Value
    -> HasDatumHash
    -> ProtocolParameters
    -> Maybe Value
calculateMinimumUTxOFromValue era value datumHash ProtocolParameters{..} =
    lovelaceToValue <$>
        case era of
            ShelleyBasedEraShelley -> protocolParamMinUTxOValue
            ShelleyBasedEraAllegra -> calculateMinimumUTxOAllegraMary
            ShelleyBasedEraMary    -> calculateMinimumUTxOAllegraMary
            ShelleyBasedEraAlonzo  -> calculateMinimumUTxOAlonzo
  where
    calculateMinimumUTxOAllegraMary :: Maybe Lovelace
    calculateMinimumUTxOAllegraMary =
        calcMinimumDeposit value
            <$> protocolParamMinUTxOValue

    calculateMinimumUTxOAlonzo :: Maybe Lovelace
    calculateMinimumUTxOAlonzo =
        (Lovelace (utxoEntrySize value datumHash) *)
            <$> protocolParamUTxOCostPerWord

-- | Calculate minimumUTxO with default protocol parameters from a Value
calculateDefaultMinimumUTxOFromValue ::
    forall era.
       ShelleyBasedEra era -> Value -> HasDatumHash -> Maybe Value
calculateDefaultMinimumUTxOFromValue era value datumHash =
    calculateMinimumUTxOFromValue era value datumHash $
        defaultCalculateMinimumUTxOParams era

-- | Calculate minimumUTxO for a singleton Value from AssetId
calculateMinimumUTxOFromAssetId ::
    forall era.
       ShelleyBasedEra era
    -> AssetId
    -> ProtocolParameters
    -> Maybe Integer
calculateMinimumUTxOFromAssetId era assetId parameters =
    let value = valueFromList [(assetId, 1)]
    in
        unLovelace . selectLovelace <$>
            calculateMinimumUTxOFromValue era value NoDatumHash parameters

-- | Calculate minimumUTxO with default protocol parameters from AssetId
calculateDefaultMinimumUTxOFromAssetId ::
    forall era.
       ShelleyBasedEra era -> AssetId -> Maybe Integer
calculateDefaultMinimumUTxOFromAssetId era assetId =
    calculateMinimumUTxOFromAssetId era assetId $
        defaultCalculateMinimumUTxOParams era
