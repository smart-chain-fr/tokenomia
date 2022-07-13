module Tokenomia.Common.Parser (
  unsafeParseOnly,
) where

import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text (Text)

unsafeParseOnly :: Parser a -> Text -> a
unsafeParseOnly parser text =
  either error id $ parseOnly parser text
