{-# LANGUAGE ImportQualifiedPost #-}

module Tokenomia.Common.Data.ByteString (
  unsafeDecodeHex,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16 (decode)

unsafeDecodeHex :: ByteString -> ByteString
unsafeDecodeHex bs = either error id $ Base16.decode bs
