{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}


module Tokenomia.Node.Status
  ( displayStatus,translateSlotToTime,translateTimeToSlot)
  where

import Data.Time.Clock.POSIX

import Control.Monad.Reader

import Ledger hiding (getPOSIXTime)
import Prelude hiding (print)

import Tokenomia.Common.Environment
import Tokenomia.Common.Node
import Tokenomia.Common.Shell.Console
import Tokenomia.Common.Shell.InteractiveMenu as I
import Data.Time.ISO8601 (parseISO8601)
import Data.Maybe
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


translateSlotToTime
  ::( MonadIO m
    , MonadReader Environment m)
  =>  m ()
translateSlotToTime = do
    syncSlot@(Slot synSlotAsInt) <- getCurrentSlotSynced
    syncTime  <- toPosixTime syncSlot
    localTime <- liftIO getPOSIXTime
    printLn $ "\t-Last Slot Synced : " <> show synSlotAsInt <> " (Δ " <> show (localTime - syncTime)  <> " s)"
    printLn $ "\t-Local Time : " <> formatISO8601 localTime
    printLn $ "\t-Last Sync  : " <> formatISO8601 syncTime
    slot  <- Slot <$> I.ask @Integer "> Given Slot  :"
    slotInPosixTime  <- toPosixTime slot
    printLn $ "\t-Slot in time  : " <> formatISO8601 slotInPosixTime

translateTimeToSlot
  ::( MonadIO m
    , MonadReader Environment m)
  =>  m ()
translateTimeToSlot = do
    syncSlot@(Slot synSlotAsInt) <- getCurrentSlotSynced
    syncTime  <- toPosixTime syncSlot
    localTime <- liftIO getPOSIXTime
    printLn $ "\t-Last Slot Synced : " <> show synSlotAsInt <> " (Δ " <> show (localTime - syncTime)  <> " s)"
    printLn $ "\t-Local Time : " <> formatISO8601 localTime
    printLn $ "\t-Last Sync  : " <> formatISO8601 syncTime
    givenTime <- fromJust . parseISO8601 <$> I.askStringFilterM "> Given Time (ISO8601)  :" (return . isJust . parseISO8601 )
    Slot translatedSlot  <- toSlot givenTime
    slotInPosixTime  <- toPosixTime (Slot translatedSlot)
    printLn $ "\t-Slot : " <> show translatedSlot <> "(" <> formatISO8601 slotInPosixTime <>  ")"