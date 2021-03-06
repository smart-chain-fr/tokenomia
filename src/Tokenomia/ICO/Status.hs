{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Tokenomia.ICO.Status
    ( displayStatus
    ) where

import Prelude hiding (round,print)
import           Control.Monad.Reader
import           Control.Monad.Except

import           Data.List.NonEmpty as NEL
import           Tokenomia.Common.Shell.Console (printLn)

import           Tokenomia.Common.Environment
import           Tokenomia.Common.Error
import           Tokenomia.ICO.Round.Settings
import           Tokenomia.ICO.Funds.Exchange.ReceivedFunds
import           Tokenomia.ICO.Funds.Exchange.Tokens
import           Tokenomia.Common.Token

displayStatus
    :: ( MonadIO m
       , MonadReader Environment m
       , MonadError TokenomiaError m)
    => RoundSettings
    -> m ()
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
showRemainingTokens RoundSettings {..} (Just ExchangeToken {token = Token {..}})
    = show amount <> " Tokens (~ " <> show (ceiling @_ @Integer $ fromIntegral amount / (tokenRatePerLovelace * 1_000_000) ) <> " ADAs )"



