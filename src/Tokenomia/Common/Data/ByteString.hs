module Tokenomia.Common.Data.ByteString
    ( unsafeDecodeHex
    , encode
    ) where

import Data.ByteString                      ( ByteString )
import Data.ByteString.Base16               ( decode, encode )


unsafeDecodeHex :: ByteString -> ByteString
unsafeDecodeHex bs = either error id $ decode bs
