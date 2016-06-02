{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Data.DHT.DKS.Type.Storage
  where

import Prelude (Num((+)))

import Data.Data ({-Data, -}Typeable)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (Maybe)
import Data.Word (Word64)
import GHC.Generics (Generic)
--import Text.Show (Show) -- TODO
import System.IO (IO)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (empty, insert, lookup)

import Data.Default.Class (Default(def))
import Data.DHT.Type.Encoding (Encoding)

import Data.DHT.DKS.Type.Hash (DksHash)


newtype DksStorage = DksStorage (IORef DksStorage_)
  deriving (Generic, Typeable)

newDksStorage :: IO DksStorage
newDksStorage = DksStorage <$> newIORef def

lookup :: DksStorage -> DksHash -> IO (Maybe Encoding)
lookup (DksStorage ref) k = (`lookup'` k) <$> readIORef ref

insert :: DksHash -> Encoding -> DksStorage -> IO ()
insert k v (DksStorage ref) = atomicModifyIORef' ref $ (, ()) <$> insert' k v

-- {{{ DksStorage_ -- Pure storage --------------------------------------------

data DksStorage_ = DksStorage_
    { _objectCounter :: {-# UNPACK #-} !Word64
    -- ^ Number of objects (key, value) pairs is there in 'HashMap' in
    -- '_storage'.

    , _usedMemory :: {-# UNPACK #-} !Word64
    -- ^ Approximation of how much memory 'HashMap' in '_storage' uses.

    , _storage :: HashMap DksHash Encoding
    }
  deriving ({-Data, -}Generic, {-Show, -}Typeable)
    -- Data and Show can be derived only if DataValue has instances for them.

instance Default DksStorage_ where
    def = DksStorage_
        { _objectCounter = 0
        , _usedMemory = 0
        , _storage = HashMap.empty
        }

lookup' :: DksStorage_ -> DksHash -> Maybe Encoding
lookup' DksStorage_{_storage} k = HashMap.lookup k _storage

insert' :: DksHash -> Encoding -> DksStorage_ -> DksStorage_
insert' k v DksStorage_{_objectCounter, _usedMemory, _storage} = DksStorage_
    { _objectCounter = _objectCounter + 1
    , _usedMemory = _usedMemory -- TODO: + memoryUsage k + memoryUsage v
    , _storage = HashMap.insert k v _storage
    }

-- }}} DksStorage_ -- Pure storage --------------------------------------------
