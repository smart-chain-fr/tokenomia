{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tokenomia.ICO.Funds.Exchange.Plan.Settings (mkPlanSettings, PlanSettings (..)) where

import Prelude hiding (print, round)

import Tokenomia.ICO.Round.Settings

import Plutus.V1.Ledger.Value

mkPlanSettings :: RoundSettings -> PlanSettings
mkPlanSettings RoundSettings {..} = Settings {..}

data PlanSettings = Settings
  { exchangeTokenId :: AssetClass
  , tokenRatePerLovelace :: RatePerLovelace
  }
  deriving stock (Show, Eq)
