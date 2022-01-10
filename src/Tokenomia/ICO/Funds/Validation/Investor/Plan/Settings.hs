{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Validation.Investor.Plan.Settings
    ( mkPlanSettings, PlanSettings (..) ) where
import           Prelude hiding (round,print)


import           Tokenomia.ICO.RoundSettings
import           Plutus.V1.Ledger.Ada
import           Ledger ( Slot(..) )
import           Plutus.V1.Ledger.Interval


mkPlanSettings :: RoundSettings  -> PlanSettings
mkPlanSettings RoundSettings {..} = Settings {..}


data PlanSettings 
    = Settings 
       { timeRange :: Interval Slot
       , minimumAdaPerFund :: Ada
       , maximumAdaPerAddress :: Ada } deriving (Show,Eq)
