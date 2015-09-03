{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  NamedFieldPuns, NoImplicitPrelude
--
-- TODO
module Data.DHT.DKS
    (
    -- * Create DKS Instance
      newDks

    -- ** DKS Parameters
    , DksParams
    , singleton
    )
  where

import Prelude (undefined)

import Control.Monad (Monad((>>)))
import Data.Bool (Bool)
import Data.Function (($))
import Data.Functor ((<$>))
import System.IO (IO, print)

import Data.DHT.Type.Handle
    ( DhtHandle(DhtHandle)
    , DhtHandle'(DhtHandle', insert, join, leave, lookup, state)
    )
import Data.DHT.Type.Key (DhtKey)
import Data.DHT.Type.Result (DhtResult, result_)
import Data.DHT.Type.Value (DhtValue)

import Data.DHT.DKS.StateMachine
    ( Event(EventJoinRequest, EventLeaveRequest, EventSelfJoinDone)
    , Signal
    , stepIORef
    )
import Data.DHT.DKS.Type.Params (DksParams(_singleton), singleton)
import Data.DHT.DKS.Type.State
    ( DksState(DksState, _signalHandler, _stateMachine)
    , newDksState
    )


newDks :: DksParams -> IO DhtHandle
newDks params = mkDhtHandle <$> newDksState dksSignalHandler
  where
    mkDhtHandle s = DhtHandle DhtHandle'
        { state  = s
        , join   = joinImpl (_singleton params)
        , leave  = leaveImpl
        , lookup = lookupImpl
        , insert = insertImpl
        }

dksSignalHandler :: Signal -> IO ()
dksSignalHandler = print

stepDksState :: DksState -> Event -> IO ()
stepDksState DksState{_stateMachine, _signalHandler} event =
    stepIORef _stateMachine event _signalHandler

joinImpl :: Bool -> DksState -> DhtResult IO ()
joinImpl doSelfJoin dksState = do
    stepDksState dksState
        $ if doSelfJoin
            then EventSelfJoinDone
            else EventJoinRequest
    -- TODO: Should this block until we reach StateInside?
    result_

leaveImpl :: DksState -> DhtResult IO ()
leaveImpl dksState = stepDksState dksState EventLeaveRequest >> result_

lookupImpl :: DksState -> DhtKey -> DhtResult IO DhtValue
lookupImpl = undefined

insertImpl :: DksState -> DhtKey -> DhtValue -> DhtResult IO ()
insertImpl = undefined
