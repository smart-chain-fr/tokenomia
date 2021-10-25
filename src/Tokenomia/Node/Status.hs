{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Tokenomia.Node.Status
  ( displayStatus)
  where

import Data.Time.Clock.POSIX 

import Control.Monad.Reader
import Shh

import Ledger hiding (getPOSIXTime)

import Tokenomia.Adapter.Cardano.CLI.Environment 
import Tokenomia.Adapter.Cardano.CLI.Node 


load SearchPath ["echo"]

displayStatus
  ::( MonadIO m
    , MonadReader Environment m)
  =>  m ()
displayStatus = do
    syncSlot@(Slot synSlotAsInt) <- getCurrentSlotSynced
    syncTime  <- toPosixTime syncSlot 
    localTime <- liftIO getPOSIXTime
    liftIO $ echo $ "\t-Last Slot Synced : " <> show synSlotAsInt <> " (Δ " <> show (localTime - syncTime)  <> " s)"
    liftIO  $ echo $ "\t-Local Time : " <> formatISO8601 localTime
    liftIO $ echo  $ "\t-Last Sync  : " <> formatISO8601 syncTime
