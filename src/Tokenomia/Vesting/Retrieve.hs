{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tokenomia.Vesting.Retrieve (retrieveFunds) where

import Prelude hiding ((+), (-))

import Data.List.NonEmpty

import Control.Monad.Except
import Control.Monad.Reader

import PlutusTx.Prelude (AdditiveSemigroup ((+)))

import Tokenomia.Script.LocalRepository qualified as Script
import Tokenomia.Script.UTxO qualified as Script

import Tokenomia.Common.Datum qualified as Script
import Tokenomia.Common.Environment
import Tokenomia.Common.Node
import Tokenomia.Common.Transacting
import Tokenomia.Wallet.LocalRepository hiding (fetchById)

import Tokenomia.Common.Error
import Tokenomia.Vesting.Repository
import Tokenomia.Wallet.CLI
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.ChildAddress.LocalRepository
import Tokenomia.Wallet.Type

retrieveFunds ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  m ()
retrieveFunds = do
  WalletWithVestedFunds {wallet = Wallet {name}, ..} <- selectWalletWithVestedFunds >>= whenNothingThrow NoVestingInProgress
  selectVesting vestedFunds >>= retrieveFunds' name

retrieveFunds' ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  WalletName ->
  Vesting ->
  m ()
retrieveFunds'
  walletName
  ( Vesting
      VestingContext
        { tranches =
          ( TrancheContext {valueVested = v1}
            , TrancheContext {valueVested = v2}
            )
        , ..
        }
      VestingState {tranches = (s1, s2), ..}
    ) = do
    let firstChildAddress = ChildAddressRef walletName 0
    adas <- selectBiggestStrictlyADAsNotCollateral firstChildAddress >>= whenNothingThrow NoADAsOnChildAddress
    currentSlot <- getCurrentSlotSynced
    voidDataFilePath <- Script.registerDatum ()
    datumVoidHash <- Script.getDataHash ()
    ChildAddress {address = walletAddress0} <- fetchById firstChildAddress
    let inputsFromWallet = FromWallet adas :| []
        inputsFromScript =
          nonEmpty $
            fmap
              ( \Script.ScriptUTxO {..} ->
                  FromScript
                    { script = Script.offChain scriptLocation
                    , datum = voidDataFilePath
                    , redeemer = voidDataFilePath
                    , utxoRef = txOutRef
                    }
              )
              scriptUTxOs

        validitySlotRangeMaybe = Just (ValiditySlotRange currentSlot (currentSlot + 100))
        tokenSupplyChangesMaybe = Nothing
        metadataMaybe = Nothing

        outputsEither = case (s1, s2) of
          (Available, Available) ->
            let tokensThatCanBeVested = v1 + v2
             in Right $ ToWallet walletAddress0 tokensThatCanBeVested Nothing :| []
          (Available, Locked) ->
            let tokensThatCanBeVested = v1
                remainingTokensOnScript = v2
             in Right $
                  ToWallet walletAddress0 tokensThatCanBeVested Nothing
                    :| [ ToScript
                          { address = Script.onChain scriptLocation
                          , value = remainingTokensOnScript
                          , datumHash = datumVoidHash
                          }
                       ]
          (Locked, Available) ->
            let remainingTokensOnScript = v1
                tokensThatCanBeVested = v2
             in Right $
                  ToWallet walletAddress0 tokensThatCanBeVested Nothing
                    :| [ ToScript
                          { address = Script.onChain scriptLocation
                          , value = remainingTokensOnScript
                          , datumHash = datumVoidHash
                          }
                       ]
          (Retrieved, Available) ->
            let tokensThatCanBeVested = v2
             in Right $ ToWallet walletAddress0 tokensThatCanBeVested Nothing :| []
          (Available, Retrieved) ->
            let tokensThatCanBeVested = v1
             in Right $ ToWallet walletAddress0 tokensThatCanBeVested Nothing :| []
          (Retrieved, Locked) -> Left NoFundsToBeRetrieved
          (Locked, Retrieved) -> Left NoFundsToBeRetrieved
          (Retrieved, Retrieved) -> Left FundAlreadyRetrieved
          (Locked, Locked) -> Left AllFundsLocked

    outputs <- liftEither outputsEither
    buildAndSubmit
      (Unbalanced (FeeAddressRef firstChildAddress))
      (Just $ CollateralAddressRef firstChildAddress)
      TxBuild {..}
