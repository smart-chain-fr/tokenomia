{-# LANGUAGE FlexibleInstances            #-}
{-# LANGUAGE ImportQualifiedPost          #-}
{-# LANGUAGE OverloadedStrings            #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.Wallet.UTxO
    ( UTxO (..)
    ) where

import Prelude           hiding ( lines )

import Data.Attoparsec.Text     ( parseOnly )
import Data.Either.Combinators  ( rightToMaybe )
import Data.Maybe               ( mapMaybe )
import Data.Text                ( lines )

import Plutus.V1.Ledger.Api     ( DatumHash(..), Value, TxOutRef (..) )

import Tokenomia.Common.Hash                            ( Hash(..) )
import Tokenomia.Common.Serialise                       ( FromCLI(..) )
import Tokenomia.Common.Parser.Utxo qualified as Parser ( utxo )


data UTxO = UTxO
    { txOutRef :: TxOutRef
    , value :: Value
    , maybeDatumHash :: Maybe Hash
    } deriving (Show,Eq)

instance Ord UTxO where
    compare x y = compare (txOutRef x) (txOutRef y)

-- 7ab72a8b4fe1128de66e5d95577c0ba213033cbe7153061bf366a5d108c2bb13     0        994933314 lovelace + TxOutDatumHashNone
-- 42e5d56fe31a9ee9bc83b6b88c2254952d9e477ca46e40dc985fe041feec50f2    10        6000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "faf44f2aa43aa67e7a8b7e8c24465515dcf86ed3780c70779e6ac13cd68f3060"

toUTxO :: (TxOutRef, Value, Maybe DatumHash) -> UTxO
toUTxO (txOutRef, value, maybeDatumHash') =
    let maybeDatumHash = Hash . show <$> maybeDatumHash'
    in
        UTxO {..}

instance FromCLI [UTxO] where
    fromCLI input = mapMaybe
        (rightToMaybe . parseOnly (toUTxO <$> Parser.utxo))
        (drop 2 . lines $ input)
