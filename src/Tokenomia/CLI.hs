{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Tokenomia.CLI (main) where

import Shh 
import Data.Function ((&))
import  qualified Tokenomia.Token.CLAPStyle.Mint.CLI as Token
import  qualified Tokenomia.Transfer.CLI as Transfer
import Data.List.NonEmpty as NonEmpty ( NonEmpty, fromList )
import Tokenomia.Adapter.Cardano.CLI
import Byline.Menu
    ( runBylineT,
      text,
      askWithMenuRepeatedly,
      menu,
      menuSuffix,
      Stylized,
      ToStylizedText(..),
      Menu )
import Data.Text ( Text )
import Data.Maybe ( fromJust )
import Control.Monad.Reader 
import Byline.Internal.Stylized ()
import qualified Tokenomia.Wallet.CLI as Wallet
import Control.Monad.Catch ( MonadMask )

load SearchPath ["echo","cardano-cli"]

main ::  IO ()
main = do 
    echo "#############################"
    echo "#   Welcome to Tokenomia    #"
    echo "#############################"
    echo ""
    echo "FYI >> you'll operate over the Testnet Network"
    echo ""
    runReaderT recursiveMenu (Testnet {magicNumber = 1097911063}) 

    echo "#############################"
    echo "#   End of Tokenomia        #"
    echo "#############################"

recursiveMenu :: (MonadMask m,MonadIO m,MonadReader Environment m) =>  m()
recursiveMenu = do
  liftIO $ echo "----------------------"
  liftIO $ echo "  Select an action"
  liftIO $ echo "----------------------"
  r <- showActionMenu     
  case r of
      WalletList -> Wallet.list
      WalletAdd  -> Wallet.createAndRegister
      WalletRemove  -> Wallet.remove
      TokenMint  -> Token.mint
      TokenBurn  ->  liftIO $ echo "TODO"
      Transfer   -> Transfer.run 
  recursiveMenu


showActionMenu :: (MonadMask m,MonadIO m) =>  m Action
showActionMenu = fmap fromJust (runBylineT $ askWithMenuRepeatedly menuConfig prompt onError)

menuConfig :: Menu Action
menuConfig = menu actions & menuSuffix "- "

prompt :: Stylized Text
prompt = text "> please choose an action (provide the index) : "

onError :: Stylized Text
onError = text "> please choose an action (provide the index) : "

actions :: NonEmpty Action
actions = NonEmpty.fromList [
    WalletList,
    WalletAdd,
    WalletRemove,
    TokenMint,
    TokenBurn,
    Transfer
    ]

data Action
  = WalletList
  | WalletAdd
  | WalletRemove
  | TokenMint
  | TokenBurn
  | Transfer
  deriving (Show)


instance ToStylizedText Action where
  toStylizedText item = case item of
    WalletList   -> "[Wallet] - List Registered Ones" 
    WalletAdd    -> "[Wallet] - Add "
    WalletRemove -> "[Wallet] - Remove"
    TokenMint    -> "[Token]  - Mint (Fix Total Supply | one-time Minting and open Burning Policy )"
    TokenBurn    -> "[Token]  - Burn (TODO)"
    Transfer     -> "Transfer "
