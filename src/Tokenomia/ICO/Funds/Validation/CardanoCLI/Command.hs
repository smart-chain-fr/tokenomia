{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tokenomia.ICO.Funds.Validation.CardanoCLI.Command (Command (..)) where

import Prelude hiding (print, round)

import Ledger (Slot (..))
import Plutus.V1.Ledger.Ada

import Tokenomia.Wallet.WalletUTxO

import Tokenomia.Common.Address
import Tokenomia.ICO.Balanceable
import Tokenomia.Wallet.ChildAddress.ChildAddressRef qualified as C

data Command
  = SendOnExchangeAddressAndPartiallyRefund
      { source :: WalletUTxO
      , exchangeAddress :: Address
      , refundAddress :: Address
      , adasToSendOnExchange :: Ada
      , datum :: FilePath
      , adasToRefund :: Ada
      , receivedAt :: Slot
      }
  | SendOnExchangeAddressAndPartiallyMoveToNextRound
      { source :: WalletUTxO
      , exchangeAddress :: Address
      , nextRoundExchangeAddress :: Address
      , adasToSendOnExchange :: Ada
      , datum :: FilePath
      , adasToMove :: Ada
      , receivedAt :: Slot
      }
  | Refund
      { source :: WalletUTxO
      , refundAddress :: Address
      , adasToRefund :: Ada
      , receivedAt :: Slot
      }
  | MoveToNextRound
      { source :: WalletUTxO
      , nextRoundExchangeAddress :: Address
      , datum :: FilePath
      , adasToMove :: Ada
      , receivedAt :: Slot
      }
  | SendOnExchangeAddress
      { source :: WalletUTxO
      , exchangeAddress :: Address
      , adasToSendOnExchange :: Ada
      , datum :: FilePath
      , receivedAt :: Slot
      }
  deriving stock (Eq)

instance Ord Command where
  compare x y = case compare ((childAddressRef . source) x) ((childAddressRef . source) y) of
    LT -> LT
    EQ -> case compare (receivedAt x) (receivedAt y) of
      LT -> LT
      EQ -> compare (source x) (source y)
      GT -> GT
    GT -> GT

instance AdaBalanceable Command where
  adaBalance Refund {..} = getAdas source - adasToRefund
  adaBalance MoveToNextRound {..} = getAdas source - adasToMove
  adaBalance SendOnExchangeAddressAndPartiallyRefund {..} = getAdas source - adasToSendOnExchange - adasToRefund
  adaBalance SendOnExchangeAddressAndPartiallyMoveToNextRound {..} = getAdas source - adasToSendOnExchange - adasToMove
  adaBalance SendOnExchangeAddress {..} = getAdas source - adasToSendOnExchange

instance Show Command where
  show Refund {..} =
    "\n Command : Refund "
      <> "\n   | received at : "
      <> show (getSlot receivedAt)
      <> "\n   | source  : "
      <> show (getAdas source)
      <> "\n   | amount  : "
      <> show adasToRefund
      <> "\n   | index  : "
      <> show (C.index . childAddressRef $ source)
      <> "\n   | refundAddress : "
      <> show refundAddress
  show MoveToNextRound {..} =
    "\n Command : MoveToNextRound "
      <> "\n   | received at : "
      <> show (getSlot receivedAt)
      <> "\n   | source  : "
      <> show (getAdas source)
      <> "\n   | amount  : "
      <> show adasToMove
      <> "\n   | nextRoundExchangeAddress : "
      <> show nextRoundExchangeAddress
      <> "\n   | index  : "
      <> show (C.index . childAddressRef $ source)
      <> "\n   | datum       : "
      <> show datum
  show SendOnExchangeAddressAndPartiallyRefund {..} =
    "\n Command : SendOnExchangeAddressAndPartiallyRefund "
      <> "\n   | received at : "
      <> show (getSlot receivedAt)
      <> "\n   | source  : "
      <> show (getAdas source)
      <> "\n   | adasToRefund  : "
      <> show adasToRefund
      <> "\n   | adasToSendOnExchange   : "
      <> show adasToSendOnExchange
      <> "\n   | exchangeAddress       : "
      <> show exchangeAddress
      <> "\n   | refundAddress : "
      <> show refundAddress
      <> "\n   | index  : "
      <> show (C.index . childAddressRef $ source)
      <> "\n   | datum       : "
      <> show datum
  show SendOnExchangeAddressAndPartiallyMoveToNextRound {..} =
    "\n Command : SendOnExchangeAddressAndPartiallyRefund "
      <> "\n   | received at : "
      <> show (getSlot receivedAt)
      <> "\n   | source  : "
      <> show (getAdas source)
      <> "\n   | adasToMove  : "
      <> show adasToMove
      <> "\n   | adasToSendOnExchange   : "
      <> show adasToSendOnExchange
      <> "\n   | exchangeAddress       : "
      <> show exchangeAddress
      <> "\n   | nextRoundExchangeAddress : "
      <> show nextRoundExchangeAddress
      <> "\n   | index  : "
      <> show (C.index . childAddressRef $ source)
      <> "\n   | datum       : "
      <> show datum
  show SendOnExchangeAddress {..} =
    "\n Command : SendOnExchangeAddress "
      <> "\n   | received at : "
      <> show (getSlot receivedAt)
      <> "\n   | source      : "
      <> show (getAdas source)
      <> "\n   | adasToSendOnExchange   : "
      <> show adasToSendOnExchange
      <> "\n   | exchangeAddress       : "
      <> show exchangeAddress
      <> "\n   | index  : "
      <> show (C.index . childAddressRef $ source)
      <> "\n   | datum       : "
      <> show datum
