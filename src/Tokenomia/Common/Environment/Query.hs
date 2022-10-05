{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE RecordWildCards                #-}

module Tokenomia.Common.Environment.Query
    ( evalQuery
    , evalQueryWithSystemStart
    ) where

import Data.Bifunctor                       ( first )

import Control.Monad.Except                 ( MonadError, liftEither )
import Control.Monad.Reader                 ( MonadReader, asks )
import Control.Monad.Trans.Except           ( ExceptT, runExceptT )

import Cardano.Api.Shelley                  ( LocalNodeConnectInfo, CardanoMode )
import Cardano.Slotting.Time                ( SystemStart )

import Tokenomia.Common.Environment         ( Environment(..) )


-- | Lift an ExceptT into a MonadError
evalExceptT ::
     ( MonadError e m )
    => (a -> e)
    -> ExceptT a m b
    -> m b
evalExceptT err x =
    runExceptT x >>= liftEither . first err

-- | Run a query that can fail in a MonadError, with a LocalNodeConnectInfo argument
evalQuery ::
     ( MonadError e m
     , MonadReader Environment m
     )
    => (a -> e)
    -> (c -> LocalNodeConnectInfo CardanoMode -> ExceptT a m b)
    -> c
    -> m b
evalQuery err query args =
    asks localNodeConnectInfo
        >>= evalExceptT err . query args

-- | Run a query that can fail in a MonadError, with an additional SystemStart argument
evalQueryWithSystemStart ::
     ( MonadError e m
     , MonadReader Environment m
     )
    => (a -> e)
    -> (SystemStart -> c -> LocalNodeConnectInfo CardanoMode -> ExceptT a m b)
    -> c
    -> m b
evalQueryWithSystemStart err query args =
    do
        systemStart <- asks systemStart'
        evalQuery err (uncurry query) (systemStart, args)
