{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE TypeFamilies                   #-}

module Tokenomia.CardanoApi.PParams
    ( defaultCalculateMinimumUTxOParams
    , withCalculateMinimumUTxOParams
    ) where

import Cardano.Api                          ( ShelleyBasedEra(..), fromLedgerPParams )
import Cardano.Api.Shelley                  ( ProtocolParameters, ShelleyLedgerEra )
import Cardano.Ledger.Coin                  ( Coin (..) )
import Cardano.Ledger.Core                  ( PParams )
import Cardano.Ledger.Alonzo.PParams        ( PParams'(..) )
import Cardano.Ledger.Shelley.PParams       ( PParams'(..) )
import Data.Default                         ( def )


-- | Update the right parameters necessary to calculateMinimumUTxO
withCalculateMinimumUTxOParams ::
    forall era.
       Integer -> ShelleyBasedEra era -> PParams (ShelleyLedgerEra era) -> ProtocolParameters
withCalculateMinimumUTxOParams n era pparams =
    fromLedgerPParams era $ updateWith (Coin n) pparams
  where
    updateWith =
        case era of
            ShelleyBasedEraShelley -> (\x up -> up {_minUTxOValue = x})
            ShelleyBasedEraAllegra -> (\x up -> up {_minUTxOValue = x})
            ShelleyBasedEraMary    -> (\x up -> up {_minUTxOValue = x})
            ShelleyBasedEraAlonzo  -> (\x up -> up {_coinsPerUTxOWord = x})

-- | Default parameters necessary to calculateMinimumUTxO
defaultCalculateMinimumUTxOParams ::
    forall era.
       ShelleyBasedEra era -> ProtocolParameters
defaultCalculateMinimumUTxOParams era =
    case era of
        ShelleyBasedEraShelley -> withCalculateMinimumUTxOParams 1000000 era def
        ShelleyBasedEraAllegra -> withCalculateMinimumUTxOParams 1000000 era def
        ShelleyBasedEraMary    -> withCalculateMinimumUTxOParams 1000000 era def
        ShelleyBasedEraAlonzo  -> withCalculateMinimumUTxOParams   34482 era def
