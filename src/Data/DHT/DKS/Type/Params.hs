{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár; 2015-2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- TODO
module Data.DHT.DKS.Type.Params
  where

import Control.Applicative (pure)
import Control.Concurrent (ThreadId, forkIO)
import qualified Control.Concurrent as Concurrent (yield)
import Data.Bool (Bool(False))
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO (IO)

import Data.Default.Class (Default(def))
import System.Lumberjack.Backend (SomeLoggingBackend)

import Data.DHT.DKS.Type.EVar (EVarIO)
import Data.DHT.DKS.Type.Hash (DksHash)


data DksParams = DksParams
    { _runThread :: !(IO () -> IO ThreadId)
    , _yield :: IO ()
    , _logging :: !SomeLoggingBackend

    , _discovery :: !(EVarIO (Maybe DksHash))
    -- ^ Discover an entry node to which join request can be sent.

    , _singleton :: !Bool
    -- ^ Force self join, '_discovery' is not even executed.

--  , _initStorage :: EVarIO SomeStorage
    }
  deriving (Generic, Typeable)

instance Default DksParams where
    def = DksParams
        { _runThread = forkIO
        , _yield = Concurrent.yield
        , _logging = def  -- No logging.
        , _discovery = pure (Right Nothing)
        , _singleton = False
        }
