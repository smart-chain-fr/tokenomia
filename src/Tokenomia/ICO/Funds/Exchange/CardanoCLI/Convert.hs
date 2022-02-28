{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tokenomia.ICO.Funds.Exchange.CardanoCLI.Convert
    ( convertToRoundSpecificPlan) where

import           Prelude hiding (round,print,map)
import           Control.Monad.Reader hiding (ask)
import           Tokenomia.Common.Environment
import           Tokenomia.ICO.Funds.Exchange.Command as RoundAgnostic
import           Tokenomia.ICO.Funds.Exchange.CardanoCLI.Command as RoundSpecific
import Tokenomia.Common.Datum
import Tokenomia.ICO.Round.Settings
import           Tokenomia.ICO.Funds.Validation.CardanoCLI.Datum
import           Tokenomia.ICO.Funds.Exchange.Plan
import qualified Data.Set.NonEmpty as NES 

convertToRoundSpecificPlan
    :: (  MonadIO m
       ,  MonadReader Environment m)
    => RoundSettings
    -> Plan RoundAgnostic.Command
    -> m (Plan RoundSpecific.Command)
convertToRoundSpecificPlan settings Plan {..} = do 
    roundSpecificCommands <- NES.fromList <$> mapM (convertCommand settings) (NES.toList commands)
    return Plan {commands = roundSpecificCommands,..}

convertCommand
    :: (  MonadIO m
       ,  MonadReader Environment m)
    => RoundSettings
    -> RoundAgnostic.Command
    -> m RoundSpecific.Command
convertCommand RoundSettings { ..} planCommand =
    case (planCommand,nextRoundMaybe) of
        (RoundAgnostic.RejectBecauseTokensSoldOut   {..},Nothing )-> do
            return RoundSpecific.RefundBecauseTokensSoldOut {refundAmount = rejectAmount, ..}
        (RoundAgnostic.RejectBecauseTokensSoldOut   {..}, Just NextRound {exchangeAddress = nextRoundExchangeAddress} ) -> do
            datumFile <- registerDatum $ mkExchangeDatum receivedAt index
            return RoundSpecific.MoveToNextRoundBecauseTokensSoldOut {moveAmount = rejectAmount,..}
        (RoundAgnostic.ExchangeAndPartiallyReject {..}, Nothing )-> do
            return RoundSpecific.ExchangeAndPartiallyRefund {refundAmount = rejectAmount,..}
        (RoundAgnostic.ExchangeAndPartiallyReject {..}, Just NextRound {exchangeAddress = nextRoundExchangeAddress} ) -> do
            datumFile <- registerDatum $ mkExchangeDatum receivedAt index 
            return RoundSpecific.ExchangeAndPartiallyMoveToNextRound { moveAmount = rejectAmount, ..}
        (RoundAgnostic.Exchange {..}, _) -> do
            return RoundSpecific.Exchange{..}

