{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE FunctionalDependencies     #-}

module Tokenomia.Common.Data.Convertible             ( Convertible(..) ) where

import Data.Text                    qualified as TS  ( Text, pack, unpack )
import Data.Text.Lazy               qualified as TL  ( Text, pack, unpack, fromStrict, toStrict )

import Data.ByteString              qualified as BS  ( ByteString )
import Data.ByteString.Lazy         qualified as BL  ( ByteString, fromStrict, toStrict )

import Data.Text.Encoding           qualified as TSE ( encodeUtf8, decodeUtf8 )
import Data.Text.Lazy.Encoding      qualified as TLE ( encodeUtf8, decodeUtf8 )

import Data.ByteString.UTF8         qualified as BSU ( fromString, toString )
import Data.ByteString.Lazy.UTF8    qualified as BLU ( fromString, toString )

import PlutusTx.Prelude             qualified as P   ( BuiltinByteString, fromBuiltin, toBuiltin )


class Convertible a b where
    convert :: a -> b


instance Convertible String TS.Text where
    convert = TS.pack
instance Convertible String TL.Text where
    convert = TL.pack
instance Convertible String BS.ByteString where
    convert = BSU.fromString
instance Convertible String BL.ByteString where
    convert = BLU.fromString
instance Convertible String P.BuiltinByteString where
    convert = P.toBuiltin @BS.ByteString . convert


instance Convertible TS.Text String where
    convert = TS.unpack
instance Convertible TS.Text TL.Text where
    convert = TL.fromStrict
instance Convertible TS.Text BS.ByteString where
    convert = TSE.encodeUtf8
instance Convertible TS.Text BL.ByteString where
    convert = convert @TL.Text . convert
instance Convertible TS.Text P.BuiltinByteString where
    convert = P.toBuiltin @BS.ByteString . convert


instance Convertible TL.Text String where
    convert = TL.unpack
instance Convertible TL.Text TS.Text where
    convert = TL.toStrict
instance Convertible TL.Text BS.ByteString where
    convert = convert @TS.Text. convert
instance Convertible TL.Text BL.ByteString where
    convert = TLE.encodeUtf8
instance Convertible TL.Text P.BuiltinByteString where
    convert = P.toBuiltin @BS.ByteString . convert


instance Convertible BS.ByteString String where
    convert = BSU.toString
instance Convertible BS.ByteString TS.Text where
    convert = TSE.decodeUtf8
instance Convertible BS.ByteString TL.Text where
    convert =  convert @TS.Text . convert
instance Convertible BS.ByteString BL.ByteString where
    convert = BL.fromStrict
instance Convertible BS.ByteString P.BuiltinByteString where
    convert = P.toBuiltin


instance Convertible BL.ByteString String where
    convert = BLU.toString
instance Convertible BL.ByteString TS.Text where
    convert =  convert @TL.Text . convert
instance Convertible BL.ByteString TL.Text where
    convert = TLE.decodeUtf8
instance Convertible BL.ByteString BS.ByteString where
    convert = BL.toStrict
instance Convertible BL.ByteString P.BuiltinByteString where
    convert = P.toBuiltin @BS.ByteString . convert


instance Convertible P.BuiltinByteString String where
    convert = convert . P.fromBuiltin
instance Convertible P.BuiltinByteString TS.Text where
    convert = convert . P.fromBuiltin
instance Convertible P.BuiltinByteString TL.Text where
    convert = convert . P.fromBuiltin
instance Convertible P.BuiltinByteString BS.ByteString where
    convert = P.fromBuiltin
instance Convertible P.BuiltinByteString BL.ByteString where
    convert = convert . P.fromBuiltin