{-# LANGUAGE FlexibleContexts             #-}
{-# LANGUAGE RecordWildCards              #-}

module Tokenomia.TokenDistribution.Split.SplitTokenSource
    ( splitTokenSource
    ) where

import Prelude           hiding ( repeat, zipWith3 )

import Control.Monad.Reader     ( MonadIO, MonadReader )
import Control.Monad.Except     ( MonadError )

import Data.List.NonEmpty       ( NonEmpty((:|)), (<|), fromList, repeat )
import Data.Maybe               ( fromJust )

import Ledger.Ada               ( lovelaceValueOf )
import Ledger.Value
    ( AssetClass
    , Value
    , assetClassValue
    , assetClassValueOf
    )

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

import Tokenomia.Common.Data.List.NonEmpty          ( singleton, zipWith3 )

import Tokenomia.TokenDistribution.CLI.Parameters   ( Parameters(..) )
import Tokenomia.TokenDistribution.Distribution     ( Distribution(..), Recipient(..) )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.LocalRepository
    ( fetchAddressByWalletAtIndex
    , fetchAddressesByWalletWithNonZeroIndex
    )

import Tokenomia.TokenDistribution.Wallet.ChildAddress.ChildAddressRef
    ( defaultFeeAddressRef, defaultCollateralAddressRef )


splitTokenSource ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletUTxO -> Parameters -> NonEmpty Distribution -> m ()
splitTokenSource source parameters distributions = do
    splitTokenSourceTxBuild source parameters distributions >>=
        buildAndSubmit
            (Unbalanced $ defaultFeeAddressRef $ adaWallet parameters)
            (Just $ defaultCollateralAddressRef $ collateralWallet parameters)

splitTokenSourceTxBuild ::
    ( MonadIO m
    , MonadReader Environment m
    , MonadError  TokenomiaError m
    )
    => WalletUTxO -> Parameters -> NonEmpty Distribution -> m TxBuild
splitTokenSourceTxBuild source Parameters{..} distributions = do
    change  <- splitTokenSourceChange
    outputs <- splitTokenSourceOutputs
    return TxBuild
        { inputsFromScript          = Nothing
        , inputsFromWallet          = singleton $ FromWallet source
        , outputs                   = change <| outputs
        , validitySlotRangeMaybe    = Nothing
        , metadataMaybe             = Metadata <$> metadataFilePath
        , tokenSupplyChangesMaybe   = Nothing
        }
  where
    splitTokenSourceChange ::
        ( MonadIO m
        , MonadReader Environment m
        )
        => m TxOut
    splitTokenSourceChange = do
        changeAddress <- fromJust <$> fetchAddressByWalletAtIndex 0 tokenWallet

        let assetClass          = getAssetClass distributions

            provisionnedToken   = assetClassValueOf (value source) assetClass
            requiredToken       = sum $ amountSum <$> distributions
            remainingToken      = provisionnedToken - requiredToken

            remainingTokenValue = tokenValue assetClass remainingToken

        return $ ToWallet changeAddress remainingTokenValue Nothing

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

    getAssetClass :: NonEmpty Distribution -> AssetClass
    getAssetClass (Distribution{..} :| _) = assetClass

    amountSum :: Distribution -> Integer
    amountSum Distribution{..} = sum $ amount <$> recipients

    tokenSum :: Distribution -> Value
    tokenSum distribution =
        tokenValue
            (assetClass distribution)
            (amountSum  distribution)

    tokenValue :: AssetClass -> Integer -> Value
    tokenValue assetClass amount =
            assetClassValue assetClass amount
        <>  ε

    ε :: Value
    ε = lovelaceValueOf minLovelacesPerUtxo
