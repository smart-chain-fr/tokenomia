{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.ICO.Funds.Exchange.Plan (
  mkPlan,
  mkPlan',
  mkPlanSettings,
  State (..),
  Plan (..),
  getTxBalance,
  IOOnTokenAddress (..),
) where

import Prelude hiding (print, round)

import Data.List.NonEmpty as NEL
import Data.Set.Ordered
import Ledger.Ada
import Tokenomia.ICO.Funds.Exchange.ReceivedFunds

import Data.Set.NonEmpty qualified as NES
import Data.Set.Ordered qualified as SO
import Tokenomia.Common.Token
import Tokenomia.ICO.Funds.Exchange.Command
import Tokenomia.ICO.Funds.Exchange.Tokens

import Data.Set.NonEmpty (NESet)
import Tokenomia.Common.Transacting
import Tokenomia.ICO.Funds.Exchange.Plan.Settings
import Tokenomia.ICO.Round.Settings

import Tokenomia.ICO.Balanceable

mkPlan ::
  PlanSettings ->
  Ada ->
  Maybe Fees ->
  Maybe ExchangeToken ->
  NESet AuthentifiedFunds ->
  Plan Command
mkPlan a b c d e = snd $ mkPlan' a b c d e

mkPlan' ::
  PlanSettings ->
  Ada ->
  Maybe Fees ->
  Maybe ExchangeToken ->
  NESet AuthentifiedFunds ->
  (State, Plan Command)
mkPlan' settings minimumAdaRequiredOnUtxoWithToken feesMaybe exchangeTokenMaybe allReceivedFunds =
  let (quotFees, remFees) = getQuotRem feesMaybe allReceivedFunds
      s@State {commands} =
        foldr
          transition
          State
            { commands = empty
            , quotientFeesPerFund = quotFees
            , remainderFeesPerFund = remFees
            , totalCommands = NES.size allReceivedFunds
            , ..
            }
          (NES.toDescList allReceivedFunds)
   in ( s
      , Plan
          { feesMaybe = feesMaybe
          , ioOnTokenAddress = mkIOOnTokenAddress exchangeTokenMaybe commands
          , commands = (NES.fromList . NEL.fromList . SO.toAscList) commands
          }
      )

getQuotRem ::
  Maybe Fees ->
  NESet AuthentifiedFunds ->
  (Fees, Fees)
getQuotRem Nothing _ = (0, 0)
getQuotRem (Just totalFees) xs = totalFees `quotRem` (fromIntegral . NES.size) xs

data State = State
  { settings :: !PlanSettings
  , minimumAdaRequiredOnUtxoWithToken :: Ada
  , feesMaybe :: !(Maybe Fees)
  , quotientFeesPerFund :: !Fees
  , remainderFeesPerFund :: !Fees
  , totalCommands :: !Int
  , exchangeTokenMaybe :: !(Maybe ExchangeToken)
  , commands :: !(OSet Command)
  }
  deriving stock (Show)

transition :: AuthentifiedFunds -> State -> State
transition AuthentifiedFunds {..} State {exchangeTokenMaybe = exchangeTokenMaybe@Nothing, ..} =
  appendCommand $
    RejectBecauseTokensSoldOut
      { rejectAmount = adas - feesPerCommand
      , ..
      }
  where
    appendCommand command = State {commands = commands |> command, ..}
    feesPerCommand = quotientFeesPerFund + addRemainderFeesPerFundIfLastCommand
    addRemainderFeesPerFundIfLastCommand = if totalCommands == size commands + 1 then remainderFeesPerFund else 0
transition
  AuthentifiedFunds {..}
  State
    { exchangeTokenMaybe = exchangeTokenMaybe@(Just ExchangeToken {token = Token {amount = exchangeTokenAmount, minimumAdaRequired = adasOnExchangeToken}})
    , settings = settings@Settings {..}
    , ..
    }
    | tokenSoldOutWithPreviousFunds =
      appendCommand $ RejectBecauseTokensSoldOut {rejectAmount = adas - feesPerCommand, ..}
    | tokenSoldOutWithIncomingFund && refundIsUnderMinimum =
      appendCommand
        ExchangeAndPartiallyReject
          { collectedAmount = collectedAmmountWhenSoldoutWithIncomingFund - minimumAdaRequiredOnUtxoWithToken
          , rejectAmount = rejectAmountWhenSoldOutWithIncomingFund + minimumAdaRequiredOnUtxoWithToken
          , tokens =
              Token
                { assetClass = exchangeTokenId
                , amount = availableTokenAmount
                , minimumAdaRequired = minimumAdaRequiredOnUtxoWithToken
                }
          , ..
          }
    | tokenSoldOutWithIncomingFund && collectIsUnderMinimum =
      appendCommand
        ExchangeAndPartiallyReject
          { collectedAmount = collectedAmmountWhenSoldoutWithIncomingFund + minimumAdaRequiredOnUtxoWithToken
          , rejectAmount = rejectAmountWhenSoldOutWithIncomingFund - minimumAdaRequiredOnUtxoWithToken
          , tokens =
              Token
                { assetClass = exchangeTokenId
                , amount = availableTokenAmount
                , minimumAdaRequired = minimumAdaRequiredOnUtxoWithToken
                }
          , ..
          }
    | tokenSoldOutWithIncomingFund =
      appendCommand
        ExchangeAndPartiallyReject
          { collectedAmount = collectedAmmountWhenSoldoutWithIncomingFund
          , rejectAmount = rejectAmountWhenSoldOutWithIncomingFund
          , tokens =
              Token
                { assetClass = exchangeTokenId
                , amount = availableTokenAmount
                , minimumAdaRequired = minimumAdaRequiredOnUtxoWithToken
                }
          , ..
          }
    | otherwise =
      appendCommand $
        Exchange
          { collectedAmount = adas - feesPerCommand - minimumAdaRequiredOnUtxoWithToken -- (899957104-1379280)/8562147
          , tokens =
              Token
                { assetClass = exchangeTokenId
                , amount = tokenAmountCurrentFund
                , minimumAdaRequired = minimumAdaRequiredOnUtxoWithToken
                }
          , ..
          }
    where
      appendCommand command = State {commands = commands |> command, ..}
      tokenAmountCurrentFund = floor (tokenRatePerLovelace * (fromIntegral adas - fromIntegral feesPerCommand))
      tokenSoldOutWithPreviousFunds = getTokensSum commands >= exchangeTokenAmount
      tokenSoldOutWithIncomingFund = getTokensSum commands + tokenAmountCurrentFund >= exchangeTokenAmount
      availableTokenAmount = exchangeTokenAmount - getTokensSum commands
      rejectAmountWhenSoldOutWithIncomingFund = adas - feesPerCommand - ceiling (fromIntegral availableTokenAmount / tokenRatePerLovelace)
      feesPerCommand = quotientFeesPerFund + addRemainderFeesPerFundIfLastCommand
      addRemainderFeesPerFundIfLastCommand = if totalCommands == size commands + 1 then remainderFeesPerFund else 0
      collectedAmmountWhenSoldoutWithIncomingFund = adas + adasOnExchangeToken - rejectAmountWhenSoldOutWithIncomingFund - minimumAdaRequiredOnUtxoWithToken - feesPerCommand
      refundIsUnderMinimum = rejectAmountWhenSoldOutWithIncomingFund < minimumAdaRequiredOnUtxoWithToken
      collectIsUnderMinimum = collectedAmmountWhenSoldoutWithIncomingFund < minimumAdaRequiredOnUtxoWithToken

mkIOOnTokenAddress :: Maybe ExchangeToken -> OSet Command -> Maybe IOOnTokenAddress
mkIOOnTokenAddress Nothing _ = Nothing
mkIOOnTokenAddress (Just source@ExchangeToken {token = Token {assetClass, amount = sourceAmount, ..}}) commands =
  let tokenExchangedAmount = getTokensSum commands
   in case sourceAmount - tokenExchangedAmount of
        tokenRemaining | tokenRemaining > 0 -> Just IOOnTokenAddress {remainingTokensMaybe = Just Token {assetClass = assetClass, amount = tokenRemaining, ..}, ..}
        _ -> Just IOOnTokenAddress {remainingTokensMaybe = Nothing, ..}

data IOOnTokenAddress = IOOnTokenAddress
  { source :: ExchangeToken
  , remainingTokensMaybe :: Maybe Token
  }
  deriving stock (Show)

data Plan command = Plan
  { feesMaybe :: Maybe Fees
  , ioOnTokenAddress :: Maybe IOOnTokenAddress
  , commands :: NES.NESet command
  }

instance (Show command) => Show (Plan command) where
  show Plan {..} =
    "\n|| PLAN || "
      <> "\n| Fees = "
      <> show feesMaybe
      <> "\n| IO On tokenAddress = "
      <> show ioOnTokenAddress

instance AdaBalanceable (Plan Command) where
  adaBalance Plan {..} = adaBalance commands + adaBalance ioOnTokenAddress - adaBalance feesMaybe

instance TokenBalanceable (Plan Command) where
  tokenBalance Plan {..} = tokenBalance commands - tokenBalance ioOnTokenAddress

instance AdaBalanceable IOOnTokenAddress where
  adaBalance IOOnTokenAddress {..} = adaBalance source - adaBalance remainingTokensMaybe

instance TokenBalanceable IOOnTokenAddress where
  tokenBalance IOOnTokenAddress {..} = tokenBalance source - tokenBalance remainingTokensMaybe

getTxBalance :: RoundAddresses -> Plan a -> TxBalance
getTxBalance _ Plan {feesMaybe = Just fees} = Balanced fees
getTxBalance roundAddresses Plan {feesMaybe = Nothing} = Unbalanced $ getFees roundAddresses
