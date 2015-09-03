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
module Data.DHT.DKS.StateMachine
  where

import Prelude (Bounded, Enum)

import Control.Monad (Monad((>>=)))
import Data.Data (Data, Typeable)
import Data.Bool (otherwise)
import Data.Eq (Eq((==)))
import Data.Function ((.))
import Data.IORef (IORef, atomicModifyIORef')
import Data.Functor (Functor, (<$>))
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show(showsPrec), shows, showString)

import Data.Default.Class (Default(def))


-- {{{ State and Event --------------------------------------------------------

data State
    = StateInitialized
    | StateJoinRequest
    | StateJoining
    | StateInside
    | StateLeaveRequest
    | StateLeaving
    | StatePredecessorLeaveRequest
    | StatePredecessorLeaving
  deriving (Bounded, Data, Enum, Eq, Generic, Show, Typeable)

-- | @'def' = 'StateInitialized'@
instance Default State where
    def = StateInitialized

data Event
    = EventSelfJoinDone
    | EventJoinRequest
    | EventJoinRetry
    | EventJoinPoint
    | EventJoinDone
    | EventLeaveRequest
    | EventLeaveRetry
    | EventGrantLeave
    | EventPredecessorLeaveRequest
    | EventPredecessorLeavePoint
    | EventPredecessorLeaveDone
    | EventReset
  deriving (Bounded, Data, Enum, Eq, Generic, Show, Typeable)

-- }}} State and Event --------------------------------------------------------

-- {{{ Signal -----------------------------------------------------------------

-- | Signal indicates either successful ('Success') or unsuccessful ('Failure')
-- transition of state machine. Both, 'Success' and 'Failure', contain
-- 'SignalInfo' that describes what transition was made or which transition was
-- unsuccessful.
--
-- While this data type could be written as:
--
-- @
-- newtype Signal = (SignalStatus, 'SignalInfo')
-- @
--
-- Written in a way in which it forces processing of both cases to get to
-- 'SignalInfo'.
data Signal
    = Success SignalInfo
    | Failure SignalInfo
  deriving (Data, Eq, Generic, Show, Typeable)

-- | Describes successful or unsuccessful transition of states caused by event.
data SignalInfo = SignalInfo
    { _cause :: Event
    -- ^ Event that caused transition or failure.
    , _stateTransitionFrom :: State
    -- ^ 'State' where 'StateMachine' was before receiving '_cause' 'Event'.
    , _stateTransitionTo :: State
    -- ^ 'State' where 'StateMachine' ended after receiving '_cause' 'Event'.
    -- If transition was unsuccessful, then this will be the same 'State' as in
    -- '_stateTransitionFrom'.
    }
  deriving (Data, Eq, Generic, Show, Typeable)

-- }}} Signal -----------------------------------------------------------------

-- {{{ StateMachine -----------------------------------------------------------

data StateMachine = StateMachine
    { _currentState :: State
    -- ^ 'State' in which this instance of 'State' machine currently is.

    , _transitionFunction :: State -> Event -> (State, Signal)
    -- ^ Transition function takes current 'State', 'Event' that 'StateMachine'
    -- received and produces new 'State' and 'Signal' that is to be processed
    -- by caller.
    --
    -- If State transition fails, then 'Signal' indicates this and new 'State'
    -- is the same as current 'State'.
    }
  deriving (Generic, Typeable)

instance Show StateMachine where
    showsPrec _ (StateMachine{_currentState}) =
        showString "{StateMachine{currentState = "
        . shows _currentState
        . showString "}}"

instance Default StateMachine where
    def = StateMachine
        { _currentState = def
        , _transitionFunction = go
        }
      where
        go :: State -> Event -> (State, Signal)
        go cur event = case event of
            EventSelfJoinDone ->
                StateInitialized ~> StateInside

            EventJoinRequest ->
                StateInitialized ~> StateJoinRequest

            EventJoinRetry ->
                StateJoinRequest ~> StateJoinRequest

            EventJoinPoint ->
                StateJoinRequest ~> StateJoining

            EventJoinDone ->
                StateJoining ~> StateInside

            EventLeaveRequest ->
                StateInside ~> StateLeaveRequest

            EventLeaveRetry ->
                StateLeaveRequest ~> StateInside

            EventGrantLeave ->
                StateLeaveRequest ~> StateLeaving

            EventPredecessorLeaveRequest ->
                StateInside ~> StatePredecessorLeaveRequest

            EventPredecessorLeavePoint ->
                StatePredecessorLeaveRequest ~> StatePredecessorLeaving

            EventPredecessorLeaveDone ->
                StatePredecessorLeaving ~> StateInside

            EventReset -> signal StateInitialized Success
          where
            (~>) :: State -> State -> (State, Signal)
            s ~> next
              | cur == s  = signal next Success
              | otherwise = signal cur  Failure

            signal :: State -> (SignalInfo -> Signal) -> (State, Signal)
            signal state f = (state, f (SignalInfo event cur state))

-- | Make 'StateMachine' receive specified 'Event' and process its result using
-- provided function. It produces new 'StateMachine', which may have
-- transitioned in to new state, and result of 'Signal' handler.
stepping
    :: Functor f
    => StateMachine
    -- ^ Initial 'StateMachine'.
    -> Event
    -- ^ 'Event' intended for 'StateMachine' to receive.
    -> (Signal -> f a)
    -- ^ Function that handles 'Signal' produced by 'StateMachine' after it
    -- receives specified 'Event'.
    -> f (StateMachine, a)
    -- ^ Possibly new state machine and result of 'Signal' handler.
stepping sm event signalHandler = (sm',) <$> signalHandler signal
  where
    (sm', signal) = step event sm

-- | Make 'StateMachine' receive specified 'Event' and process its result using
-- provided function. It produces new 'StateMachine', which may have
-- transitioned in to new state, and 'Signal' it produced while transitioning.
step
    :: Event
    -- ^ 'Event' intended for 'StateMachine' to receive.
    -> StateMachine
    -- ^ Initial 'StateMachine'.
    -> (StateMachine, Signal)
step event sm@StateMachine{_transitionFunction} =
    (sm{_currentState = nextState}, signal)
  where
    (nextState, signal) = _transitionFunction (_currentState sm) event

stepIORef
    :: IORef StateMachine
    -> Event
    -- ^ 'Event' intended for 'StateMachine' to receive.
    -> (Signal -> IO r)
    -- ^ Function that handles 'Signal' produced by 'StateMachine' after it
    -- receives specified 'Event'.
    -> IO r
stepIORef refSm event signalHandler =
    atomicModifyIORef' refSm (step event) >>= signalHandler

-- }}} StateMachine -----------------------------------------------------------
