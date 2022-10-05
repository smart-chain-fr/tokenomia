{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE KindSignatures                 #-}
{-# LANGUAGE RecordWildCards                #-}
{-# LANGUAGE ScopedTypeVariables            #-}

module Tokenomia.Common.Environment.Query
    ( evalQuery
    , evalQueryWithSystemStart
    ) where

import Data.Bifunctor                       ( first )
import Data.Kind                            ( Type )

import Control.Monad.Except                 ( MonadError, liftEither )
import Control.Monad.Reader                 ( MonadReader, asks )
import Control.Monad.Trans.Except           ( ExceptT, runExceptT )

import Cardano.Api.Shelley                  ( LocalNodeConnectInfo, CardanoMode )
import Cardano.Slotting.Time                ( SystemStart )

import Tokenomia.Common.Environment         ( Environment(..) )


-- | Lift an ExceptT into a MonadError
evalExceptT ::
    forall (m :: Type -> Type) (a :: Type) (b :: Type) (e :: Type).
     ( MonadError e m )
    => (a -> e)
    -> ExceptT a m b
    -> m b
evalExceptT err x =
    runExceptT x >>= liftEither . first err

-- | Run a query that can fail in a MonadError, with a LocalNodeConnectInfo argument
evalQuery ::
    forall (m :: Type -> Type) (a :: Type) (b :: Type)  (c :: Type) (e :: Type).
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
    forall (m :: Type -> Type) (a :: Type) (b :: Type) (c :: Type) (e :: Type).
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
