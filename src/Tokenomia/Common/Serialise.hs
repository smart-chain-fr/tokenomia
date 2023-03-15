module Tokenomia.Common.Serialise
    ( FromCLI(..)
    , ToCLI(..)
    ) where

import Data.Text                                       ( Text )

class FromCLI a where
   fromCLI :: Text -> a

class ToCLI a where
   toCLI :: a -> Text
