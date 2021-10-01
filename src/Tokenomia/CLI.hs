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

import Data.Function ((&))
import Data.Text ( Text )
import Data.Maybe ( fromJust )
import Tokenomia.Common.Shell.InteractiveMenu (askSelect)

import Control.Monad.Reader 
import Control.Monad.Catch ( MonadMask )

import Data.List.NonEmpty as NonEmpty ( NonEmpty, fromList )

import Shh 

import Byline.Internal.Stylized ()
import Byline.Menu
    ( runBylineT,
      text,
      askWithMenuRepeatedly,
      menu,
      menuSuffix,
      Stylized,
      ToStylizedText(..),
      Menu )

import           Tokenomia.Adapter.Cardano.CLI
import qualified Tokenomia.Wallet.CLI as Wallet
import qualified Tokenomia.Token.CLAPStyle.Mint as Token
import qualified Tokenomia.Token.CLAPStyle.Burn as Token
import qualified Tokenomia.Token.Transfer as Token

load SearchPath ["echo","cardano-cli","clear"]

main ::  IO ()
main = do 
    clear
    echo "#############################"
    echo "#   Welcome to Tokenomia    #"
    echo "#############################"
    echo ""
    echo "FYI >> you'll operate over the Testnet Network (magic number = 1097911063)"
    echo ""
    waitAndClear
    runReaderT recursiveMenu (Testnet {magicNumber = 1097911063}) 

    echo "#############################"
    echo "#   End of Tokenomia        #"
    echo "#############################"

waitAndClear :: IO()
waitAndClear = do 
   _ <- echo "-n" "> press enter to continue..." >>  getLine
   clear

recursiveMenu :: (MonadMask m,MonadIO m,MonadReader Environment m) =>  m()
recursiveMenu = do
  liftIO $ echo "----------------------"
  liftIO $ echo "  Select an action"
  liftIO $ echo "----------------------"
  r <- showActionMenu     
  case r of
      WalletList    -> Wallet.list
      WalletAdd     -> Wallet.createAndRegister
      WalletRemove  -> Wallet.remove
      TokenMint     -> Token.mint
      TokenBurn     -> Token.burn
      TokenTransfer -> Token.transfer
  liftIO waitAndClear         
  recursiveMenu


showActionMenu :: (MonadMask m,MonadIO m) =>  m Action
showActionMenu = askSelect

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
    TokenTransfer
    ]

data Action
  = WalletList
  | WalletAdd
  | WalletRemove
  | TokenMint
  | TokenBurn
  | TokenTransfer
  deriving (Show)


instance ToStylizedText Action where
  toStylizedText item = case item of
    WalletList    -> "[Wallet] - List Registered Ones" 
    WalletAdd     -> "[Wallet] - Add "
    WalletRemove  -> "[Wallet] - Remove"
    TokenMint     -> "[Token]  - Mint with CLAP type policy (Fix Total Supply | one-time Minting and open Burning Policy )"
    TokenBurn     -> "[Token]  - Burn Tokens with CLAP type policy"
    TokenTransfer -> "[Token]  - Transfer "
