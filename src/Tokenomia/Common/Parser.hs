module Tokenomia.Common.Parser
    ( unsafeParseOnly
    ) where

import           Data.Text              ( Text )
import           Data.Attoparsec.Text   ( Parser, parseOnly )


unsafeParseOnly :: Parser a -> Text -> a
unsafeParseOnly parser text =
    either error id $ parseOnly parser text
