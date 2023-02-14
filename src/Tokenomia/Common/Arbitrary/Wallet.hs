{-# LANGUAGE DerivingStrategies             #-}
{-# LANGUAGE ImportQualifiedPost            #-}
{-# LANGUAGE KindSignatures                 #-}
{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# OPTIONS_GHC -Wno-unused-top-binds       #-}

module Tokenomia.Common.Arbitrary.Wallet
    ( Mnemonics(..)
    , RootPrivateKey(..)
    , StakePrivateKey(..)
    , StakePublicKey(..)
    , PaymentPrivateKey(..)
    , PaymentPublicKey(..)
    , PaymentAddress(..)

    , generateMnemonics
    , getRootPrivateKey
    , getStakePrivateKey
    , getStakePublicKey
    , getPaymentPrivateKey
    , getPaymentPublicKey
    , getPaymentAddress
    , getPaymentAddressDelegated

    , generateKeysForAddressDerivation
    , deriveAddress
    , deriveAddresses
    , generateAddresses
    ) where

import Control.Monad.IO.Class               ( MonadIO(..) )
import Data.Kind                            ( Type )
import Data.ByteString.Lazy                 ( ByteString )
import Data.ByteString.Lazy.Char8 qualified
    as ByteString                           ( unwords )

import Shh.Internal
    ( ExecReference(..)
    , load
    , captureTrim
    , captureWords
    , (|>)
    , (>>>)
    )


load SearchPath
    [ "cardano-address"
    ]

missingExecutables :: IO [FilePath]

newtype Mnemonics
    =   Mnemonics
    { unMnemonics :: [ByteString] }
    deriving stock (Show)

newtype RootPrivateKey
    =   RootPrivateKey
    { unRootPrivateKey :: ByteString }
    deriving stock (Show)

newtype StakePrivateKey
    =   StakePrivateKey
    { unStakePrivateKey :: ByteString }
    deriving stock (Show)

newtype StakePublicKey
    =   StakePublicKey
    { unStakePublicKey :: ByteString }
    deriving stock (Show)

newtype PaymentPrivateKey
    =   PaymentPrivateKey
    { unPaymentPrivateKey :: ByteString }
    deriving stock (Show)

newtype PaymentPublicKey
    =   PaymentPublicKey
    { unPaymentPublicKey :: ByteString }
    deriving stock (Show)

newtype PaymentAddress
    =   PaymentAddress
    { unPaymentAddress :: ByteString }
    deriving stock (Show)

generateMnemonics ::
    forall (m :: Type -> Type).
     ( MonadIO m )
    => Integer -> m Mnemonics
generateMnemonics size =
    Mnemonics <$> liftIO
        ( cardano_address "recovery-phrase" "generate" "--size" size
            |> captureWords
        )

getRootPrivateKey ::
    forall (m :: Type -> Type).
     ( MonadIO m )
    => Mnemonics -> m RootPrivateKey
getRootPrivateKey (Mnemonics mnemonics) =
    RootPrivateKey <$> liftIO
        ( (ByteString.unwords mnemonics) >>> cardano_address "key" "from-recovery-phrase" "Shelley"
            |> captureTrim
        )

getStakePrivateKey ::
    forall (m :: Type -> Type).
     ( MonadIO m )
    => RootPrivateKey -> m StakePrivateKey
getStakePrivateKey (RootPrivateKey rootPrivateKey) =
    StakePrivateKey <$> liftIO
        ( rootPrivateKey >>> cardano_address "key" "child" "1852H/1815H/0H/2/0"
            |> captureTrim
        )

getStakePublicKey ::
    forall (m :: Type -> Type).
     ( MonadIO m )
    => StakePrivateKey-> m StakePublicKey
getStakePublicKey (StakePrivateKey stakePrivateKey) =
    StakePublicKey <$> liftIO
        ( stakePrivateKey >>> cardano_address "key" "public" "--with-chain-code"
            |> captureTrim
        )

getPaymentPrivateKey ::
    forall (m :: Type -> Type).
     ( MonadIO m )
    => RootPrivateKey -> Integer -> m PaymentPrivateKey
getPaymentPrivateKey (RootPrivateKey rootPrivateKey) index =
    PaymentPrivateKey <$> liftIO
        ( rootPrivateKey >>> cardano_address "key" "child" ("1852H/1815H/0H/0/" <> show index)
            |> captureTrim
        )

getPaymentPublicKey ::
    forall (m :: Type -> Type).
     ( MonadIO m )
    => PaymentPrivateKey -> m PaymentPublicKey
getPaymentPublicKey (PaymentPrivateKey paymentPrivateKey) =
    PaymentPublicKey <$> liftIO
        ( paymentPrivateKey >>> cardano_address "key" "public" "--with-chain-code"
            |> captureTrim
        )

getPaymentAddress ::
    forall (m :: Type -> Type).
     ( MonadIO m )
    => String -> PaymentPublicKey -> m PaymentAddress
getPaymentAddress network (PaymentPublicKey paymentPublicKey) =
    PaymentAddress <$> liftIO
        ( paymentPublicKey >>> cardano_address "address" "payment" "--network-tag" network
            |> captureTrim
        )

getPaymentAddressDelegated ::
    forall (m :: Type -> Type).
     ( MonadIO m )
    => StakePublicKey -> PaymentAddress -> m PaymentAddress
getPaymentAddressDelegated (StakePublicKey stakePublicKey) (PaymentAddress paymentAddress) =
    PaymentAddress <$> liftIO
        ( paymentAddress >>> cardano_address "address" "delegation" stakePublicKey
            |> captureTrim
        )

generateKeysForAddressDerivation ::
    forall (m :: Type -> Type).
     ( MonadIO m )
    => m (RootPrivateKey, StakePublicKey)
generateKeysForAddressDerivation =
    do
        rootPrivateKey <- generateMnemonics 24 >>= getRootPrivateKey
        stakePublicKey <- getStakePrivateKey rootPrivateKey >>= getStakePublicKey
        pure (rootPrivateKey, stakePublicKey)

deriveAddress ::
    forall (m :: Type -> Type).
     ( MonadIO m )
    => String -> RootPrivateKey -> StakePublicKey -> Integer -> m PaymentAddress
deriveAddress network rootPrivateKey stakePublicKey index =
    getPaymentPrivateKey rootPrivateKey index
        >>= getPaymentPublicKey
        >>= getPaymentAddress network
        >>= getPaymentAddressDelegated stakePublicKey

deriveAddresses ::
    forall (m :: Type -> Type) (t :: Type -> Type).
    ( MonadIO m
    , Traversable t
    )
    => String -> RootPrivateKey -> StakePublicKey -> t Integer -> m (t PaymentAddress)
deriveAddresses network rootPrivateKey stakePublicKey =
    traverse $ deriveAddress network rootPrivateKey stakePublicKey

generateAddresses ::
    forall (m :: Type -> Type) (t :: Type -> Type).
    ( MonadIO m
    , Traversable t
    )
    => String -> t Integer -> m (t PaymentAddress)
generateAddresses network xs =
    generateKeysForAddressDerivation
        >>= \keys -> uncurry (deriveAddresses network) keys xs
