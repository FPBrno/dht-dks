{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NamedFieldPuns,
--               NoImplicitPrelude, TupleSections
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
import Data.DHT.Type.Key (DhtKey)
import Data.DHT.Type.Value (DhtValue)


newtype DksStorage = DksStorage (IORef DksStorage_)
  deriving (Generic, Typeable)

newDksStorage :: IO DksStorage
newDksStorage = DksStorage <$> newIORef def

lookup :: DksStorage -> DhtKey -> IO (Maybe DhtValue)
lookup (DksStorage ref) k = (`lookup'` k) <$> readIORef ref

insert :: DhtKey -> DhtValue -> DksStorage -> IO ()
insert k v (DksStorage ref) = atomicModifyIORef' ref $ (, ()) <$> insert' k v

-- {{{ DksStorage_ -- Pure storage --------------------------------------------

data DksStorage_ = DksStorage_
    { _objectCounter :: {-# UNPACK #-} !Word64
    -- ^ Number of objects (key, value) pairs is there in 'HashMap' in
    -- '_storage'.

    , _usedMemory :: {-# UNPACK #-} !Word64
    -- ^ Approximation of how much memory 'HashMap' in '_storage' uses.

    , _storage :: HashMap DhtKey DhtValue
    }
  deriving ({-Data, -}Generic, {-Show, -}Typeable)
    -- Data and Show can be derived only if DataValue has instances for them.

instance Default DksStorage_ where
    def = DksStorage_
        { _objectCounter = 0
        , _usedMemory = 0
        , _storage = HashMap.empty
        }

lookup' :: DksStorage_ -> DhtKey -> Maybe DhtValue
lookup' DksStorage_{_storage} k = HashMap.lookup k _storage

insert' :: DhtKey -> DhtValue -> DksStorage_ -> DksStorage_
insert' k v DksStorage_{_objectCounter, _usedMemory, _storage} = DksStorage_
    { _objectCounter = _objectCounter + 1
    , _usedMemory = _usedMemory -- TODO: + memoryUsage k + memoryUsage v
    , _storage = HashMap.insert k v _storage
    }

-- }}} DksStorage_ -- Pure storage --------------------------------------------
