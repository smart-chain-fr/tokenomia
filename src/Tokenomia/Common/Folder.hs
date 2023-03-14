{-# LANGUAGE ExtendedDefaultRules                      #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE FlexibleInstances                         #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE NamedFieldPuns                            #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE TemplateHaskell                           #-}
{-# LANGUAGE TupleSections                             #-}
{-# LANGUAGE TypeApplications                          #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures           #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds             #-}


module Tokenomia.Common.Folder
    ( Folder(..)
    , getFolderPath
    , getRootCLIFolder
    ) where

import Control.Monad.Reader                            ( MonadIO(..), MonadReader, asks )
import Shh.Internal                                    ( ExecReference(SearchPath), load )
import System.Environment                              ( getEnv )

import Tokenomia.Common.Environment                    ( Environment(Mainnet, Testnet) )





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
