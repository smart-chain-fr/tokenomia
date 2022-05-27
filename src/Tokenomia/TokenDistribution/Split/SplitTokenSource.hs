{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.Split.SplitTokenSource
    ( splitTokenSource
    ) where

import Prelude           hiding ( repeat, zipWith3 )

import Control.Monad.Reader     ( MonadIO, MonadReader )
import Control.Monad.Except     ( MonadError )

import Data.List.NonEmpty       ( NonEmpty((:|)), fromList, repeat )
import Data.Maybe               ( fromJust )

import Ledger.Ada               ( lovelaceValueOf )
import Ledger.Value
    ( AssetClass
    , Value
    , assetClassValue
    , assetClassValueOf
    )

import Tokenomia.Common.AssetClass  ( adaAssetClass )
import Tokenomia.Common.Error       ( TokenomiaError )
import Tokenomia.Common.Environment ( Environment )
import Tokenomia.Wallet.WalletUTxO  ( WalletUTxO, value )

import Tokenomia.Common.Transacting
    ( TxInFromWallet(..)
    , TxOut(ToWallet)
    , TxBuild(..)
    , TxBalance(..)
    , Metadata(..)
    , buildAndSubmit
    )

import Tokenomia.Common.Data.List.NonEmpty          ( prependMaybe, singleton, zipWith3 )

import Tokenomia.TokenDistribution.CLI.Parameters   ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution     ( Distribution(..), Recipient(..) )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository
    ( fetchAddressByWalletAtIndex
    , fetchAddressesByWalletWithNonZeroIndex
    )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( defaultFeeAddressRef, defaultCollateralAddressRef )


-- Let ε be the value corresponding to the minimum ada needed to create an UTxO.
--
-- When splitting a token source into multiple pieces,
-- we need to cover multiple ε to create output UTxOs
-- containing the token for each batch,
-- unless the assets to distribute are ada.
--
-- Note that an unbalanced build is used here, meaning the extra ada value
-- already contained in the token source is given back
-- to another exchange address on the ada wallet.
-- This allows provisionning the token with a value possibly not stricly ε.
--
-- Splitting the token source may require a change output on the token wallet
-- in case `provisionnedToken > requiredToken` to get back spare token.
--
-- When the assets to distribute are ada, the case `remainingAda < ε` will fail,
-- but this is tolerated here because the process will ultimately fail anyway
-- due to the lack of ada to cover fees.

splitTokenSource ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletUTxO -> Parameters -> NonEmpty Distribution -> m ()
splitTokenSource source parameters distributions = do
    let assetClass = getAssetClass distributions

    splitTokenSourceTxBuild source parameters distributions assetClass >>=
        buildAndSubmit
            (Unbalanced $ defaultFeeAddressRef $ adaWallet parameters)
            (Just $ defaultCollateralAddressRef $ collateralWallet parameters)
  where
    getAssetClass :: NonEmpty Distribution -> AssetClass
    getAssetClass (Distribution{..} :| _) = assetClass

splitTokenSourceTxBuild ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletUTxO -> Parameters -> NonEmpty Distribution -> AssetClass -> m TxBuild
splitTokenSourceTxBuild source Parameters{..} distributions assetClass = do
    change  <- splitTokenSourceChange
    outputs <- splitTokenSourceOutputs
    return TxBuild
        { inputsFromScript          = Nothing
        , inputsFromWallet          = singleton $ FromWallet source
        , outputs                   = prependMaybe change outputs
        , validitySlotRangeMaybe    = Nothing
        , metadataMaybe             = Metadata <$> metadataFilePath
        , tokenSupplyChangesMaybe   = Nothing
        }
  where
    splitTokenSourceChange ::
        ( MonadIO m
        , MonadReader Environment m
        )
        => m (Maybe TxOut)
    splitTokenSourceChange = do
        changeAddress <- fromJust <$> fetchAddressByWalletAtIndex 0 tokenWallet

        let provisionnedToken   = assetClassValueOf (value source) assetClass
            requiredToken       = sum $ amountSum <$> distributions
            remainingToken      = provisionnedToken - requiredToken

        pure $ if remainingToken <= 0
            then Nothing
            else Just $ ToWallet changeAddress (tokenValue remainingToken) Nothing

    splitTokenSourceOutputs ::
        ( MonadIO m
        , MonadReader Environment m
        , MonadError  TokenomiaError m
        )
        => m (NonEmpty TxOut)
    splitTokenSourceOutputs = do
        addresses <- fromList <$> fetchAddressesByWalletWithNonZeroIndex tokenWallet
        let values = tokenSum <$> distributions
        return $ zipWith3 ToWallet addresses values (repeat Nothing)

    ε :: Value
    ε = lovelaceValueOf minLovelacesPerUtxo

    addε :: Value -> Value
    addε
        | distributeAda = id
        | otherwise     = (ε <>)

    distributeAda :: Bool
    distributeAda =
        assetClass == adaAssetClass

    amountSum :: Distribution -> Integer
    amountSum distribution = sum $ amount <$> recipients distribution

    tokenSum :: Distribution -> Value
    tokenSum distribution =
        tokenValue $ amountSum distribution

    tokenValue :: Integer -> Value
    tokenValue amount =
        addε $ assetClassValue assetClass amount
