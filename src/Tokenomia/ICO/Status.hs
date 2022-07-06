{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tokenomia.ICO.Status (
  displayStatus,
) where

import Control.Monad.Except
import Control.Monad.Reader
import Prelude hiding (print, round)

import Data.List.NonEmpty as NEL
import Tokenomia.Common.Shell.Console (printLn)

import Tokenomia.Common.Environment
import Tokenomia.Common.Error
import Tokenomia.Common.Token
import Tokenomia.ICO.Funds.Exchange.ReceivedFunds
import Tokenomia.ICO.Funds.Exchange.Tokens
import Tokenomia.ICO.Round.Settings

displayStatus ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  RoundSettings ->
  m ()
displayStatus round = do
  printLn $ show round

  tokensMaybe <- fetchTokens round

  printLn "--------------------------------------"
  printLn $ "|| Remaining Tokens : " <> showRemainingTokens round tokensMaybe

  printLn "--------------------------------------"
  printLn "|| Funds on Exchange Addresses ||"

  fetchRawReceivedFundsByTx round
    >>= authentifyTxsAsComingFromRoundWallet round
    >>= discardRejectedTxs
    >>= \funds -> do
      mapM_ (printLn . show) (NEL.sort funds)
      printLn "--------------------------------------"
      printLn $ "Nb Funds On Exchange > " <> show (NEL.length funds)
      printLn "--------------------------------------"

showRemainingTokens :: RoundSettings -> Maybe ExchangeToken -> String
showRemainingTokens _ Nothing = "No tokens available for exchange"
showRemainingTokens RoundSettings {..} (Just ExchangeToken {token = Token {..}}) =
  show amount <> " Tokens (~ " <> show (ceiling @_ @Integer $ fromIntegral amount / (tokenRatePerLovelace * 1_000_000)) <> " ADAs )"
