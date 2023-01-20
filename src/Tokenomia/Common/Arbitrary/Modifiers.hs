{-# LANGUAGE DerivingStrategies             #-}

module Tokenomia.Common.Arbitrary.Modifiers
    ( Restricted(..)
    ) where


newtype Restricted a
    =   Restricted { getRestricted :: a }
    deriving stock ( Show, Eq )
