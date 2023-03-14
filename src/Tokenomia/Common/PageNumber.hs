{-# LANGUAGE DerivingVia                               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving                #-}

module Tokenomia.Common.PageNumber
    ( PageNumber(..)
    ) where


newtype PageNumber = PageNumber Int
    deriving stock (Eq, Ord, Show)
    deriving newtype (Num, Enum, Real, Integral)
