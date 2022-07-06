{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Tokenomia.Token.Transfer (
  transfer,
  transfer',
) where

import PlutusTx.Prelude (AdditiveGroup ((-)), AdditiveSemigroup ((+)))
import Prelude hiding ((+), (-))

import Control.Monad.Except
import Control.Monad.Reader hiding (ask)
import Data.List.NonEmpty

import Ledger.Value
import Tokenomia.Common.Environment

import Ledger.Ada
import Tokenomia.Common.Address
import Tokenomia.Common.Error
import Tokenomia.Common.Shell.Console (printLn)
import Tokenomia.Common.Shell.InteractiveMenu (ask, askString, askStringLeaveBlankOption)
import Tokenomia.Common.Token
import Tokenomia.Common.Transacting
import Tokenomia.Common.Value
import Tokenomia.Wallet.CLI
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
import Tokenomia.Wallet.ChildAddress.LocalRepository
import Tokenomia.Wallet.Collateral.Read
import Tokenomia.Wallet.Type
import Tokenomia.Wallet.UTxO as UTxO
import Tokenomia.Wallet.WalletUTxO

transfer ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  m ()
transfer = do
  Wallet {name = sourceTokenWalletName} <-
    fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
      >>= \wallets -> do
        printLn "Select the wallet containing the tokens: "
        askToChooseAmongGivenWallets wallets
  Wallet {name = feesWalletName} <-
    fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
      >>= \wallets -> do
        printLn "Select the wallet containing funds : "
        askToChooseAmongGivenWallets wallets
  utxoWithToken <- askUTxOFilterBy (containingOneToken . UTxO.value . utxo) (ChildAddressRef sourceTokenWalletName 0) >>= whenNothingThrow NoUTxOWithOnlyOneToken
  amount <- ask @Integer "- Amount of Token to transfer : "
  receiverAddr <- Address <$> askString "- Receiver address : "
  labelMaybe <- askStringLeaveBlankOption "- Add label to your transaction (leave blank if no) : "

  transfer' feesWalletName sourceTokenWalletName receiverAddr utxoWithToken amount labelMaybe

type MetadataLabel = String

transfer' ::
  ( MonadIO m
  , MonadReader Environment m
  , MonadError TokenomiaError m
  ) =>
  WalletName ->
  WalletName ->
  Address ->
  WalletUTxO ->
  Integer ->
  Maybe MetadataLabel ->
  m ()
transfer' feesWalletName sourceTokenWalletName receiverAddr utxoWithToken amount labelMaybe = do
  let firstChildAddressSourceToken = ChildAddressRef sourceTokenWalletName 0
      firstChildAddressFees = ChildAddressRef feesWalletName 0
  metadataMaybe <- mapM (fmap Metadata . createMetadataFile) labelMaybe
  ChildAddress {address = senderWalletChildAddress} <- fetchById firstChildAddressSourceToken
  let (tokenPolicyHash, tokenNameSelected, totalAmount) = getTokenFrom . UTxO.value . utxo $ utxoWithToken
      tokenId = singleton tokenPolicyHash tokenNameSelected
      valueToTransfer = tokenId amount + toValue getMinimumUTxOAdaRequired
      change = tokenId (totalAmount - amount) + toValue getMinimumUTxOAdaRequired

  buildAndSubmit
    (Unbalanced (FeeAddressRef firstChildAddressFees))
    (Just $ CollateralAddressRef firstChildAddressFees)
    TxBuild
      { inputsFromWallet = FromWallet utxoWithToken :| []
      , inputsFromScript = Nothing
      , outputs =
          ToWallet receiverAddr valueToTransfer Nothing
            :| [ToWallet senderWalletChildAddress change Nothing]
      , validitySlotRangeMaybe = Nothing
      , tokenSupplyChangesMaybe = Nothing
      , ..
      }
