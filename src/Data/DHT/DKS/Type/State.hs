{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, NoImplicitPrelude
--
-- TODO
module Data.DHT.DKS.Type.State
  where

import Control.Monad (Monad(return))
import Data.Functor ((<$>))
import Data.IORef (IORef, newIORef)
import Data.Typeable (Typeable)
import Text.Show (Show(showsPrec), showString)
import System.IO (IO)

import Data.Default.Class (Default(def))

import Data.DHT.DKS.StateMachine (Signal, StateMachine)


data DksMessage = DksMessage
  deriving (Show, Typeable)

data DksState = DksState
    { _stateMachine  :: IORef StateMachine
    , _signalHandler :: !(Signal -> IO ())
    , _readMessage   :: !(IO DksMessage)
    , _writeMessage  :: !(DksMessage -> IO ())
    , _disconnect    :: !(IO ())
    }
  deriving (Typeable)

instance Show DksState where
    showsPrec _ _ = showString "{DhtHandle{implementation = DKS}}"

newDksState :: (Signal -> IO ()) -> IO DksState
newDksState handler = mkDksState <$> newIORef def
  where
    mkDksState sm = DksState
        { _stateMachine = sm
        , _signalHandler = handler
        , _readMessage = return DksMessage
        , _writeMessage = \_ -> return ()
        , _disconnect = return ()
        }
