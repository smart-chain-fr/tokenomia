{-# LANGUAGE FlexibleContexts             #-}

module Tokenomia.TokenDistribution.Wallet.ChildAddress.ChainIndex
    ( fetchProvisionedUTxO
    ) where

import Control.Monad.Reader     ( MonadIO, MonadReader )

import Tokenomia.Common.Environment ( Environment )
import Tokenomia.Wallet.WalletUTxO  ( WalletUTxO )

import Tokenomia.Wallet.ChildAddress.ChildAddressRef
    ( ChildAddressRef(..) )

import Tokenomia.Wallet.ChildAddress.ChainIndex
    ( queryUTxO )

fetchProvisionedUTxO ::
    ( MonadIO m
    , MonadReader Environment m
    )
    => ChildAddressRef -> m WalletUTxO
fetchProvisionedUTxO childAddressRef =
    head <$> queryUTxO childAddressRef
