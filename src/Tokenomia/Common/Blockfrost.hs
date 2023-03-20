{-# LANGUAGE DuplicateRecordFields                     #-}
{-# LANGUAGE ExtendedDefaultRules                      #-}
{-# LANGUAGE FlexibleContexts                          #-}
{-# LANGUAGE FlexibleInstances                         #-}
{-# LANGUAGE ImportQualifiedPost                       #-}
{-# LANGUAGE LambdaCase                                #-}
{-# LANGUAGE RankNTypes                                #-}
{-# LANGUAGE RecordWildCards                           #-}
{-# LANGUAGE ScopedTypeVariables                       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns               #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures           #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds             #-}

module Tokenomia.Common.Blockfrost
    ( projectFromEnv''
    ) where


import Blockfrost.Client qualified as B

import Prelude hiding                                  ( head )
import Tokenomia.Common.Environment                    ( Environment(..), TokenomiaNetwork(..), networkMagicNumber )

import Control.Monad.Except                            ( MonadError(throwError) )
import Control.Monad.Reader                            ( MonadIO(..), MonadReader, asks )
import Tokenomia.Common.Error                          ( TokenomiaError(NetworkNotSupported) )

projectFromEnv''
    :: ( MonadIO m
       , MonadReader Environment m, MonadError TokenomiaError m) => m B.Project
projectFromEnv'' = do
    environmentPath <- asks
        (\case
            Mainnet {}
                -> Right "BLOCKFROST_TOKEN_MAINNET_PATH"
            Testnet {..} | magicNumber == networkMagicNumber PreprodNetwork
                -> Right "BLOCKFROST_TOKEN_PREPROD_PATH"
            Testnet {..} | magicNumber == networkMagicNumber TestnetNetwork
                -> Left "Blockfrost does not support the legacy Testnet anymore"
        )
    case environmentPath of
        Left err -> throwError (NetworkNotSupported err)
        Right env -> liftIO $ B.projectFromEnv' env
