{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Exchange.Plan.Settings
    ( mkPlanSettings, PlanSettings (..) ) where
import           Prelude hiding (round,print)


import           Tokenomia.ICO.Round.Settings

import           Plutus.V1.Ledger.Value


mkPlanSettings :: RoundSettings  -> PlanSettings
mkPlanSettings RoundSettings {..} = Settings {..}


data PlanSettings 
    = Settings 
       { exchangeTokenId :: AssetClass
       , tokenRatePerLovelace :: RatePerLovelace } deriving (Show,Eq)
