{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tokenomia.ICO.Funds.Validation.CardanoCLI.Datum
    ( ExchangeDatum (..)
    , mkExchangeDatum
    , datumFromJson
    , ) where

import           Data.Aeson                
import           GHC.Generics              (Generic)
import Prelude   as Haskell
import           Data.Hashable                    (Hashable)
import           Control.DeepSeq                  (NFData)
import           Codec.Serialise.Class            (Serialise)
import qualified PlutusTx
import qualified PlutusTx.Prelude as PlutusTx

import Ledger.Slot
import Tokenomia.Wallet.ChildAddress.ChildAddressRef 

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano

mkExchangeDatum 
    :: Slot 
    -> ChildAddressIndex 
    -> ExchangeDatum
mkExchangeDatum (Slot slot) (ChildAddressIndex index) = ExchangeDatum (slot,index)

newtype ExchangeDatum = ExchangeDatum (PlutusTx.Integer,PlutusTx.Integer)
      deriving stock (Generic, Haskell.Show, Haskell.Eq)
      deriving newtype (Serialise, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
      deriving anyclass (Hashable, NFData, ToJSON, FromJSON)

PlutusTx.makeLift ''ExchangeDatum

datumFromJson :: Value -> (Slot,ChildAddressIndex )
datumFromJson v 
    = case Cardano.scriptDataFromJson 
        Cardano.ScriptDataJsonDetailedSchema v of 
            Left e -> error $ "Unexpected Datum format " <> show e
            Right sData -> case (PlutusTx.fromData . Cardano.toPlutusData) sData of
              Nothing -> error $ "Unexpected Datum unserialisation " <> show v
              Just (ExchangeDatum (x,y)) -> (Slot x, ChildAddressIndex y)
        
        