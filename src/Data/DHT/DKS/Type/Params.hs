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
    ( DksParams(..)
    , discovery
    , logging
    , singleton
    , defaultDiscovery
    )
  where

import Control.Applicative (pure)
import Control.Concurrent (ThreadId, forkIO)
import qualified Control.Concurrent as Concurrent (yield)
import Data.Bool (Bool(False, True))
import Data.Either (Either(Right))
import Data.Maybe (Maybe(Nothing))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO (IO)

import Data.Default.Class (Default(def))
import Data.Monoid.Endo (E)
import System.Lumberjack.Backend (SomeLoggingBackend)

import Data.DHT.DKS.Type.EVar (EVarIO)
import Data.DHT.DKS.Type.Hash (DksHash)


-- | Parameters that modify behaviour of a DKS node.
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

-- |
-- @
-- def = 'DksParams'
--     { 'discovery' = 'defaultDiscovery'
--     , 'singleton' = False
--     , 'logging' = 'def'  -- No logging.
--     }
-- @
instance Default DksParams where
    def = DksParams
        { _runThread = forkIO
        , _yield = Concurrent.yield
        , _logging = def  -- No logging.
        , _discovery = defaultDiscovery
        , _singleton = False
        }
    {-# INLINE def #-}

-- | Always fails to discover any DHT network entry node by always returning
-- 'Nothing'. See also 'discovery'.
defaultDiscovery :: EVarIO (Maybe DksHash)
defaultDiscovery = pure (Right Nothing)
{-# INLINE defaultDiscovery #-}

-- | Provide a way how DKS node can discover \"entry\" node, i.e. node to which
-- Join Request can be sent. If the discovery function returns 'Nothing', then
-- DKS node will self-join, i.e. it will create a DHT network with only one
-- node, itself.
--
-- Default implementation of discovery function ('defaultDiscovery') always
-- returns 'Nothing'.
discovery :: EVarIO (Maybe DksHash) -> E DksParams
discovery d params = params{_discovery = d}
{-# INLINE discovery #-}

-- | Node won't try to discover \"entry node\" for joining a DHT network,
-- instead it will self-join, i.e. it will create a new DHT network with only
-- one node, itself.
--
-- By default DKS node will try to discover a DKS network.
singleton :: E DksParams
singleton params = params{_singleton = True}
{-# INLINE singleton #-}

-- | Use provided logging backend, by default there is no logging.
logging :: SomeLoggingBackend -> E DksParams
logging lb params = params{_logging = lb}
{-# INLINE logging #-}
