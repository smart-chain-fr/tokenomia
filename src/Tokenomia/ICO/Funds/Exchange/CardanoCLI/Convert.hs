{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tokenomia.ICO.Funds.Exchange.CardanoCLI.Convert (convertToRoundSpecificPlan) where

import Control.Monad.Reader hiding (ask)
import Data.Set.NonEmpty qualified as NES
import Tokenomia.Common.Datum
import Tokenomia.Common.Environment
import Tokenomia.ICO.Funds.Exchange.CardanoCLI.Command as RoundSpecific
import Tokenomia.ICO.Funds.Exchange.Command as RoundAgnostic
import Tokenomia.ICO.Funds.Exchange.Plan
import Tokenomia.ICO.Funds.Validation.CardanoCLI.Datum
import Tokenomia.ICO.Round.Settings
import Prelude hiding (map, print, round)

convertToRoundSpecificPlan ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  RoundSettings ->
  Plan RoundAgnostic.Command ->
  m (Plan RoundSpecific.Command)
convertToRoundSpecificPlan settings Plan {..} = do
  roundSpecificCommands <- NES.fromList <$> mapM (convertCommand settings) (NES.toList commands)
  return Plan {commands = roundSpecificCommands, ..}

convertCommand ::
  ( MonadIO m
  , MonadReader Environment m
  ) =>
  RoundSettings ->
  RoundAgnostic.Command ->
  m RoundSpecific.Command
convertCommand RoundSettings {..} planCommand =
  case (planCommand, nextRoundMaybe) of
    (RoundAgnostic.RejectBecauseTokensSoldOut {..}, Nothing) -> do
      return RoundSpecific.RefundBecauseTokensSoldOut {refundAmount = rejectAmount, ..}
    (RoundAgnostic.RejectBecauseTokensSoldOut {..}, Just NextRound {exchangeAddress = nextRoundExchangeAddress}) -> do
      datumFile <- registerDatum $ mkExchangeDatum receivedAt index
      return RoundSpecific.MoveToNextRoundBecauseTokensSoldOut {moveAmount = rejectAmount, ..}
    (RoundAgnostic.ExchangeAndPartiallyReject {..}, Nothing) -> do
      return RoundSpecific.ExchangeAndPartiallyRefund {refundAmount = rejectAmount, ..}
    (RoundAgnostic.ExchangeAndPartiallyReject {..}, Just NextRound {exchangeAddress = nextRoundExchangeAddress}) -> do
      datumFile <- registerDatum $ mkExchangeDatum receivedAt index
      return RoundSpecific.ExchangeAndPartiallyMoveToNextRound {moveAmount = rejectAmount, ..}
    (RoundAgnostic.Exchange {..}, _) -> do
      return RoundSpecific.Exchange {..}
