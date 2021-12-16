{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
module Tokenomia.ICO.Funds.Reception.CardanoCLI.Plan
    ( plan
    , Plan (..)
    , getTxBalance) where
import           Prelude hiding (round,print)



import           Tokenomia.ICO.Funds.Reception.CardanoCLI.Command
import           Tokenomia.ICO.RoundSettings


import qualified Data.Set.NonEmpty as NES
import           Tokenomia.Common.Transacting




plan ::  Maybe Fees -> NES.NESet Command  ->  Plan Command
plan Nothing commands = Plan {feesMaybe = Nothing,..}


data Plan command 
    = Plan 
        { feesMaybe :: Maybe Fees
        , commands :: NES.NESet command }
    

instance (Show command) =>  Show (Plan command) where 
    show Plan {..} =
           "\n|| PLAN || " 
        <> "\n| Fees = " <> show feesMaybe
        <> "\n| Commands = " <> show commands 


getTxBalance :: RoundAddresses -> Plan a -> TxBalance
getTxBalance  _ Plan {feesMaybe = Just fees} = Balanced fees
getTxBalance  roundAddresses Plan {feesMaybe = Nothing} = Unbalanced $ getFees roundAddresses
