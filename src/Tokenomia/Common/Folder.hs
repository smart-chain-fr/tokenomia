{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}


module Tokenomia.Common.Folder
    ( getFolderPath
    , getRootCLIFolder
    , Folder (..)
    ) where

import Control.Monad.Reader ( MonadIO(..), MonadReader, asks )
import           System.Environment (getEnv)
import Shh.Internal ( load, ExecReference(SearchPath) )

import Tokenomia.Common.Environment
    ( Environment(Mainnet, Testnet) )





{-# ANN module "HLINT: ignore Use camelCase" #-}

load SearchPath ["mkdir","cardano-cli" ]

data Folder
    = Transactions
    | Wallets
    | Parameters
    | MonetaryPolicies
    | Validators
    | Datum
    | TMP

getFolderPath :: (MonadIO m, MonadReader Environment m) => Folder -> m FilePath
getFolderPath folder
    =  getFolderPath'
            $ case folder of
                Transactions -> "transactions"
                Wallets ->  "wallets"
                Parameters -> "parameters"
                MonetaryPolicies -> "monetary-policies"
                Validators -> "validators"
                Datum -> "datums"
                TMP -> "tmp"


getFolderPath' :: (MonadIO m, MonadReader Environment m) => String -> m FilePath
getFolderPath' s = do
    a <- ( <> "/"<> s<>"/") <$> getRootCLIFolder
    liftIO $ mkdir "-p" a
    return a

getRootCLIFolder :: (MonadIO m, MonadReader Environment m) => m FilePath
getRootCLIFolder = do
    environmentFolder <- asks (\case
                                Testnet {} -> "testnet"
                                Mainnet {} -> "mainnet")
    a <- ( <> "/.tokenomia-cli/" <> environmentFolder) <$> (liftIO . getEnv) "HOME"
    liftIO $ mkdir "-p" a
    return a
