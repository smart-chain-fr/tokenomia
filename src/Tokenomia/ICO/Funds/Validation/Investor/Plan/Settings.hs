{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tokenomia.ICO.Funds.Validation.Investor.Plan.Settings (mkPlanSettings, PlanSettings (..)) where

import Prelude hiding (print, round)

import Ledger (Slot (..))
import Plutus.V1.Ledger.Ada
import Plutus.V1.Ledger.Interval
import Tokenomia.ICO.Round.Settings

mkPlanSettings :: RoundSettings -> PlanSettings
mkPlanSettings RoundSettings {..} = Settings {..}

data PlanSettings = Settings
  { timeRange :: Interval Slot
  , minimumAdaPerFund :: Ada
  , maximumAdaPerAddress :: Ada
  }
  deriving stock (Show, Eq)
