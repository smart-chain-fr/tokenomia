{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.ICO.Funds.Exchange.CardanoCLI.Command (Command (..)) where

import Ledger (Slot (..))
import Plutus.V1.Ledger.Ada

import Tokenomia.Wallet.WalletUTxO

import Tokenomia.Common.Address
import Tokenomia.Common.Token
import Tokenomia.ICO.Balanceable
import Tokenomia.Wallet.ChildAddress.ChildAddressRef qualified as C
import Prelude hiding (print, round)

data Command
  = RefundBecauseTokensSoldOut
      { paybackAddress :: Address
      , source :: WalletUTxO
      , refundAmount :: Ada
      , receivedAt :: Slot
      }
  | MoveToNextRoundBecauseTokensSoldOut
      { source :: WalletUTxO
      , nextRoundExchangeAddress :: Address
      , datumFile :: FilePath
      , moveAmount :: Ada
      , receivedAt :: Slot
      }
  | ExchangeAndPartiallyRefund
      { paybackAddress :: Address
      , source :: WalletUTxO
      , collectedAmount :: Ada
      , refundAmount :: Ada
      , tokens :: Token
      , receivedAt :: Slot
      }
  | ExchangeAndPartiallyMoveToNextRound
      { source :: WalletUTxO
      , nextRoundExchangeAddress :: Address
      , paybackAddress :: Address
      , collectedAmount :: Ada
      , moveAmount :: Ada
      , tokens :: Token
      , datumFile :: FilePath
      , receivedAt :: Slot
      }
  | Exchange
      { paybackAddress :: Address
      , source :: WalletUTxO
      , collectedAmount :: Ada
      , tokens :: Token
      , receivedAt :: Slot
      }
  deriving stock (Eq)

instance Ord Command where
  compare x y = case compare (receivedAt x) (receivedAt y) of
    LT -> LT
    EQ -> compare (source x) (source y)
    GT -> GT

instance Show Command where
  show RefundBecauseTokensSoldOut {..} =
    "\n Command : RefundBecauseTokensSoldOut "
      <> "\n   | received at : "
      <> show (getSlot receivedAt)
      <> "\n   | source  : "
      <> show (getAdas source)
      <> "\n   | refund  : "
      <> show refundAmount
      <> "\n   | paybackAddress  : "
      <> show paybackAddress
      <> "\n   | index  : "
      <> show (C.index . childAddressRef $ source)
  show MoveToNextRoundBecauseTokensSoldOut {..} =
    "\n Command : MoveToNextRoundBecauseTokensSoldOut "
      <> "\n   | received at : "
      <> show (getSlot receivedAt)
      <> "\n   | source      : "
      <> show (getAdas source)
      <> "\n   | move        : "
      <> show moveAmount
      <> "\n   | datum       : "
      <> show datumFile
      <> "\n   | nextRoundExchangeAddress  : "
      <> show nextRoundExchangeAddress
      <> "\n   | index  : "
      <> show (C.index . childAddressRef $ source)
  show ExchangeAndPartiallyRefund {..} =
    "\n Command : ExchangeAndPartiallyRefund "
      <> "\n   | received at : "
      <> show (getSlot receivedAt)
      <> "\n   | source      : "
      <> show (getAdas source)
      <> "\n   | refund      : "
      <> show refundAmount
      <> "\n   | collected   : "
      <> show collectedAmount
      <> "\n   | token       : "
      <> show tokens
      <> "\n   | paybackAddress  : "
      <> show paybackAddress
      <> "\n   | index  : "
      <> show (C.index . childAddressRef $ source)
  show ExchangeAndPartiallyMoveToNextRound {..} =
    "\n Command : ExchangeAndPartiallyMoveToNextRound "
      <> "\n   | received at : "
      <> show (getSlot receivedAt)
      <> "\n   | source      : "
      <> show (getAdas source)
      <> "\n   | move        : "
      <> show moveAmount
      <> "\n   | collected   : "
      <> show collectedAmount
      <> "\n   | token       : "
      <> show tokens
      <> "\n   | paybackAddress  : "
      <> show paybackAddress
      <> "\n   | nextRoundExchangeAddress  : "
      <> show nextRoundExchangeAddress
      <> "\n   | datum       : "
      <> show datumFile
      <> "\n   | index  : "
      <> show (C.index . childAddressRef $ source)
  show Exchange {..} =
    "\n Command : Exchange "
      <> "\n   | received at : "
      <> show (getSlot receivedAt)
      <> "\n   | source      : "
      <> show (getAdas source)
      <> "\n   | collected   : "
      <> show collectedAmount
      <> "\n   | token       : "
      <> show tokens
      <> "\n   | paybackAddress  : "
      <> show paybackAddress
      <> "\n   | index  : "
      <> show (C.index . childAddressRef $ source)

instance AdaBalanceable Command where
  adaBalance RefundBecauseTokensSoldOut {..} = getAdas source - refundAmount
  adaBalance MoveToNextRoundBecauseTokensSoldOut {..} = getAdas source - moveAmount
  adaBalance Exchange {tokens = Token {minimumAdaRequired}, ..} = getAdas source - collectedAmount - minimumAdaRequired
  adaBalance ExchangeAndPartiallyRefund {tokens = Token {minimumAdaRequired}, ..} = getAdas source - collectedAmount - minimumAdaRequired - refundAmount
  adaBalance ExchangeAndPartiallyMoveToNextRound {tokens = Token {minimumAdaRequired}, ..} = getAdas source - collectedAmount - minimumAdaRequired - moveAmount

instance TokenBalanceable Command where
  tokenBalance = getTokenAmount

getTokenAmount :: Command -> Integer
getTokenAmount RefundBecauseTokensSoldOut {} = 0
getTokenAmount MoveToNextRoundBecauseTokensSoldOut {} = 0
getTokenAmount Exchange {tokens = Token {..}} = amount
getTokenAmount ExchangeAndPartiallyRefund {tokens = Token {..}} = amount
getTokenAmount ExchangeAndPartiallyMoveToNextRound {tokens = Token {..}} = amount
