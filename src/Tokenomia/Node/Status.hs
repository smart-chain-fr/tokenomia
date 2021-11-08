{-# LANGUAGE FlexibleContexts #-}


module Tokenomia.Node.Status
  ( displayStatus)
  where

import Data.Time.Clock.POSIX 

import Control.Monad.Reader

import Ledger hiding (getPOSIXTime)
import Prelude hiding (print)

import Tokenomia.Common.Environment 
import Tokenomia.Common.Node 
import Tokenomia.Common.Shell.Console (printLn)

displayStatus
  ::( MonadIO m
    , MonadReader Environment m)
  =>  m ()
displayStatus = do
    syncSlot@(Slot synSlotAsInt) <- getCurrentSlotSynced
    syncTime  <- toPosixTime syncSlot 
    localTime <- liftIO getPOSIXTime
    printLn $ "\t-Last Slot Synced : " <> show synSlotAsInt <> " (Δ " <> show (localTime - syncTime)  <> " s)"
    printLn $ "\t-Local Time : " <> formatISO8601 localTime
    printLn $ "\t-Last Sync  : " <> formatISO8601 syncTime
