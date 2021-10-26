{-# LANGUAGE LambdaCase #-}

module Tokenomia.Common.Error
    ( whenNullThrow
    , whenSomethingThrow
    , whenNothingThrow) where


import           Control.Monad.Except
import           Data.List.NonEmpty (nonEmpty, NonEmpty)

whenNullThrow :: MonadError e m => e -> [a]  -> m (NonEmpty a)
whenNullThrow err = 
    (\case 
      Nothing -> throwError err
      Just xs -> return xs) . nonEmpty 
    


whenSomethingThrow :: MonadError e m => (a -> e) -> Maybe a  -> m ()
whenSomethingThrow toErr = maybe (pure ()) (throwError . toErr)

whenNothingThrow :: MonadError e m => e -> Maybe a ->  m a
whenNothingThrow err = maybe (throwError err) pure


