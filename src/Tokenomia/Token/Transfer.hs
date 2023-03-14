{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Tokenomia.Token.Transfer
    ( transfer
    , transfer'
    ) where

import           Prelude hiding ((+),(-))
import           PlutusTx.Prelude  (AdditiveSemigroup((+)),AdditiveGroup((-)))

import Data.List.NonEmpty ( NonEmpty((:|)) )
import Control.Monad.Reader ( MonadIO, MonadReader )
import Control.Monad.Except ( MonadError )

import Ledger.Value ( singleton )
import Tokenomia.Common.Environment ( Environment )

import Ledger.Ada ( toValue )
import Tokenomia.Wallet.UTxO as UTxO ( UTxO(value) )
import Tokenomia.Wallet.WalletUTxO ( WalletUTxO(utxo) )
import Tokenomia.Common.Transacting
    ( buildAndSubmit,
      createMetadataFile,
      Metadata(Metadata),
      TxBalance(Unbalanced),
      TxBuild(..),
      TxInFromWallet(FromWallet),
      TxOut(ToWallet) )
import Tokenomia.Common.Error
    ( whenNothingThrow,
      whenNullThrow,
      TokenomiaError(..) )
import Tokenomia.Wallet.Collateral.Read
    ( fetchWalletsWithCollateral )
import Tokenomia.Wallet.CLI
    ( askToChooseAmongGivenWallets, askUTxOFilterBy )
import           Tokenomia.Common.Shell.Console (printLn)
import           Tokenomia.Common.Shell.InteractiveMenu  (ask,askString, askStringLeaveBlankOption)
import Tokenomia.Common.Value ( containingOneToken, getTokenFrom )
import Tokenomia.Wallet.ChildAddress.ChildAddressRef
    ( ChildAddressRef(..),
      CollateralAddressRef(..),
      FeeAddressRef(..) )
import Tokenomia.Wallet.Type ( Wallet(..), WalletName )
import Tokenomia.Wallet.ChildAddress.LocalRepository
    ( fetchById,
      ChildAddress(..) )
import Tokenomia.Common.Address ( Address(..) )
import Tokenomia.Common.Token ( getMinimumUTxOAdaRequired )


transfer ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m )
    => m ()
transfer = do
    Wallet {name = sourceTokenWalletName } <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
        >>= \wallets -> do
            printLn "Select the wallet containing the tokens: "
            askToChooseAmongGivenWallets wallets
    Wallet {name = feesWalletName } <- fetchWalletsWithCollateral >>= whenNullThrow NoWalletWithCollateral
        >>= \wallets -> do
            printLn "Select the wallet containing funds : "
            askToChooseAmongGivenWallets wallets
    utxoWithToken <- askUTxOFilterBy (containingOneToken . UTxO.value . utxo) (ChildAddressRef sourceTokenWalletName 0) >>= whenNothingThrow NoUTxOWithOnlyOneToken
    amount <- ask @Integer                  "- Amount of Token to transfer : "
    receiverAddr <- Address <$> askString   "- Receiver address : "
    labelMaybe <- askStringLeaveBlankOption "- Add label to your transaction (leave blank if no) : "

    transfer' feesWalletName  sourceTokenWalletName receiverAddr utxoWithToken  amount labelMaybe

type MetadataLabel = String

transfer' ::
    (  MonadIO m
    , MonadReader Environment m
    , MonadError TokenomiaError m )
    => WalletName
    -> WalletName
    -> Address
    -> WalletUTxO
    -> Integer
    -> Maybe MetadataLabel
    -> m ()
transfer' feesWalletName sourceTokenWalletName receiverAddr utxoWithToken amount labelMaybe = do
    let firstChildAddressSourceToken = ChildAddressRef sourceTokenWalletName 0
        firstChildAddressFees = ChildAddressRef feesWalletName 0
    metadataMaybe <- mapM (fmap Metadata . createMetadataFile)  labelMaybe
    ChildAddress {address = senderWalletChildAddress} <- fetchById firstChildAddressSourceToken
    let (tokenPolicyHash,tokenNameSelected,totalAmount) = getTokenFrom . UTxO.value  . utxo $ utxoWithToken
        tokenId = singleton tokenPolicyHash tokenNameSelected
        valueToTransfer = tokenId amount + toValue getMinimumUTxOAdaRequired
        change = tokenId (totalAmount - amount) + toValue getMinimumUTxOAdaRequired

    buildAndSubmit
      (Unbalanced (FeeAddressRef firstChildAddressFees))
      (Just $ CollateralAddressRef firstChildAddressFees)
      TxBuild
        { inputsFromWallet =  FromWallet utxoWithToken :| []
        , inputsFromScript = Nothing
        , outputs = ToWallet receiverAddr valueToTransfer Nothing
                :| [ToWallet senderWalletChildAddress change Nothing]
        , validitySlotRangeMaybe = Nothing
        , tokenSupplyChangesMaybe = Nothing
        , ..}
