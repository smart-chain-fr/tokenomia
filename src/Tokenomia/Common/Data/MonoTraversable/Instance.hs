{-# LANGUAGE TypeFamilies                 #-}
{-# LANGUAGE DerivingVia                  #-}

module Tokenomia.Common.Data.MonoTraversable.Instance
    ( Wrap(..)
    ) where

--
-- This module provides default instance for MonoFunctor and MonoZip to derive
-- with the via strategy. It is necessary to introduce the `Wrap` newtype
-- because there is no instance of MonoZip for the Identity functor.
--

import Control.Lens             ( both )
import Control.Lens.Setter      ( over )

import Data.Functor.Identity    ( Identity(..) )

import Data.MonoTraversable     ( MonoFunctor, Element )
import Data.Containers          ( MonoZip, ozipWith, ozip, ounzip )


type instance Element (Wrap a) = a

newtype Wrap a
    =   Wrap { unWrap :: a }
    deriving ( MonoFunctor ) via Identity (Wrap a)

instance (Monoid a) => MonoZip (Wrap a) where
    ozipWith f a b = Wrap $ f (unWrap a) (unWrap b)
    ozip a b = [(unWrap a, unWrap b)]
    ounzip = over both (Wrap . mconcat) . unzip
