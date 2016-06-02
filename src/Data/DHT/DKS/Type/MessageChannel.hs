{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Data.DHT.DKS.Type.MessageChannel
  where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Exception (SomeException)
import Data.Either (Either(Right))
import Data.Function ((.))
import Data.Functor (Functor(fmap))
import Data.Typeable (Typeable)
import System.IO (IO)

import Data.DHT.DKS.Type.Hash (DksHash)
import Data.DHT.DKS.Type.Message (DksMessage)


class DksMessageChannel a where
    registerReceiveMessage
        :: a
        -> DksHash
        -> (DksMessage -> IO (Either SomeException ()))
        -> IO ()

    sendMessage :: a -> DksMessage -> IO (Either SomeException ())

data SomeDksMessageChannel =
    forall a. DksMessageChannel a => SomeDksMessageChannel a
  deriving Typeable

instance DksMessageChannel SomeDksMessageChannel where
    registerReceiveMessage (SomeDksMessageChannel a) = registerReceiveMessage a
    sendMessage (SomeDksMessageChannel a) = sendMessage a

data StmMessageChannel = StmMessageChannel
    { _registerInHandler
        :: DksHash
        -> (DksMessage -> IO (Either SomeException ()))
        -> IO ()
    , _outChan :: TChan DksMessage
    }
  deriving Typeable

instance DksMessageChannel StmMessageChannel where
    registerReceiveMessage StmMessageChannel{_registerInHandler = reg} = reg
    sendMessage StmMessageChannel{_outChan = chan} =
        fmap Right . atomically . writeTChan chan
