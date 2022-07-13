{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tokenomia.Common.Address (Address (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)

newtype Address = Address String
  deriving stock (Eq, Ord)
  deriving (Show, IsString) via String
  deriving newtype (ToJSON, FromJSON)
