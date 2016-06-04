{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár; 2015-2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Data.DHT.DKS
    (
    -- * Create DKS Instance
      newDks

    -- ** DKS Parameters
    , DksParams
    )
  where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar)
import Control.Exception (SomeException)
import Control.Monad (Monad((>>=)), void)
import Data.Either (Either)
import Data.Function (($), (.), const, id)
import Data.Functor ((<$), (<$>))
import Data.Maybe (Maybe(Just))
import System.IO (IO)

import Data.DHT.Type.Handle
    ( DhtHandle(DhtHandle)
    , DhtHandle'(DhtHandle', hash, insert, join, leave, lookup, state)
    )
import Data.DHT.Type.Key (DhtKey)
import Data.DHT.Type.Result (DhtResult, DhtResultVar(DhtResultVar))
import Data.DHT.Type.Encoding (Encoding)

import Data.DHT.DKS.Internal
    ( DksHandle
    , dksInsert
    , dksJoin
    , dksLeave
    , dksLookup
    , newDksHandle
    )
import Data.DHT.DKS.Type.Hash (DksHash)
import Data.DHT.DKS.Type.MessageChannel (DksMessageChannel)
import Data.DHT.DKS.Type.Params (DksParams)


newDks :: DksMessageChannel chan => chan -> DksParams -> DksHash -> IO DhtHandle
newDks chan params node = mkDhtHandle <$> newDksHandle chan params node
  where
    mkDhtHandle s = DhtHandle DhtHandle'
        { state  = s
        , hash   = const id
        , join   = joinImpl
        , leave  = leaveImpl
        , lookup = lookupImpl
        , insert = insertImpl
        }

withDhtResult :: (MVar (Either SomeException a) -> IO ()) -> DhtResult IO a
withDhtResult f = newEmptyMVar >>= \v -> DhtResultVar v <$ f v

joinImpl :: DksHandle -> DhtResult IO ()
joinImpl h = withDhtResult $ \v -> dksJoin (done v . void) h

leaveImpl :: DksHandle -> DhtResult IO ()
leaveImpl h = withDhtResult $ \v -> dksLeave (Just (done v)) h

lookupImpl :: DksHandle -> DhtKey -> DhtResult IO Encoding
lookupImpl h k = withDhtResult $ \v ->
    dksLookup (done v) h k

insertImpl :: DksHandle -> DhtKey -> Encoding -> DhtResult IO ()
insertImpl s k e = withDhtResult $ \v ->
    dksInsert (Just (done v)) s k e

done :: MVar (Either SomeException a) -> Either SomeException a -> IO ()
done v = putMVar v
