{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár, 2015-2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Data.DHT.DKS.Type.State
    ( State(..)
    , Event(..)
    , Signal(..)
    , SignalInfo(..)
    , DksState(..)
    , stepDksState
    )
  where

import Prelude (Bounded, Enum)

import Data.Bool (Bool(False), otherwise)
import Data.Data (Data, Typeable)
import Data.Eq (Eq((==)))
import Data.Maybe (Maybe(Nothing))
import GHC.Generics (Generic)
import Text.Show (Show)

import Data.Default.Class (Default(def))
import Data.Monoid.Endo (E)

import Data.DHT.DKS.Type.Hash (DksHash)


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
    | EventProcessingJoinRequest
    | EventNewSuccessor
    | EventNewSuccessorAck
    | EventSelfLeaveDone
    | EventLeaveRequest
    | EventLeaveRetry
    | EventGrantLeave
    | EventLeaveDone
    | EventUpdateSuccessor
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
    = Success !SignalInfo
    | Failure !SignalInfo
  deriving (Data, Eq, Generic, Show, Typeable)

-- | Describes successful or unsuccessful transition of states caused by event.
data SignalInfo = SignalInfo
    { _cause :: !Event
    -- ^ Event that caused transition or failure.

    , _stateTransitionFrom :: !State
    -- ^ 'State' where 'DksState' was before receiving '_cause' 'Event'.

    , _stateTransitionTo :: !State
    -- ^ 'State' where 'DksState' ended after receiving '_cause' 'Event'. If
    -- transition was unsuccessful, then this will be the same 'State' as in
    -- '_stateTransitionFrom'.
    }
  deriving (Data, Eq, Generic, Show, Typeable)

-- }}} Signal -----------------------------------------------------------------

-- {{{ DksState ---------------------------------------------------------------

data DksState = DksState
    { _currentState :: !State
    -- ^ 'State' in which this instance of 'State' machine currently is.
    , _lock :: !Bool
    , _leaveForward :: !Bool
    , _joinForward :: !Bool
    , _predecessor :: !(Maybe DksHash)
    , _oldPredecessor :: !(Maybe DksHash)
    , _successor :: !(Maybe DksHash)
    }
  deriving (Generic, Show, Typeable)

instance Default DksState where
    def = DksState
        { _currentState = def
        , _lock = False
        , _leaveForward = False
        , _joinForward = False
        , _predecessor = Nothing
        , _oldPredecessor = Nothing
        , _successor = Nothing
        }

-- | Transition function takes current 'State', 'Event' that 'DksState'
-- has to process and produces new 'State' and 'Signal' that is to be processed
-- by caller.
--
-- If State transition fails, then 'Signal' indicates this and new 'State'
-- is the same as current 'State'.
dksStateTransitionFunction :: State -> Event -> (State, Signal)
dksStateTransitionFunction cur event = case event of
    EventSelfJoinDone -> StateInitialized ~> StateInside
    EventJoinRequest -> StateInitialized ~> StateJoinRequest
    EventJoinRetry -> StateJoinRequest ~> StateJoinRequest
    EventJoinPoint -> StateJoinRequest ~> StateJoining
    EventJoinDone -> StateJoining ~> StateInside
    EventProcessingJoinRequest -> StateInside ~> StateInside
    EventNewSuccessor -> case cur of
        StateInside -> signal cur Success

        -- Things to the left from our node do not affect things that happen to
        -- the right of us. Unless there are only two nodes in the network, in
        -- which case the other one would be the one leaving, in which case new
        -- (third) node is joining and this has to be allowed.
        StatePredecessorLeaveRequest -> signal cur Success
        StatePredecessorLeaving -> signal cur Success

        -- Until node receives GrantLeave, it is safe to change a successor
        -- node.
        StateLeaveRequest -> signal cur Success

        StateInitialized -> signal cur Failure
        StateJoinRequest -> signal cur Failure
        StateJoining -> signal cur Failure
        StateLeaving -> signal cur Failure

    EventNewSuccessorAck -> StateInside ~> StateInside
    EventSelfLeaveDone -> StateInside ~> StateInitialized
    EventLeaveRequest -> StateInside ~> StateLeaveRequest
    EventLeaveRetry -> StateLeaveRequest ~> StateInside
    EventGrantLeave -> StateLeaveRequest ~> StateLeaving
    EventLeaveDone -> StateLeaving ~> StateInitialized
    EventPredecessorLeaveRequest -> StateInside ~> StatePredecessorLeaveRequest
    EventPredecessorLeavePoint ->
        StatePredecessorLeaveRequest ~> StatePredecessorLeaving
    EventPredecessorLeaveDone -> StatePredecessorLeaving ~> StateInside
    EventUpdateSuccessor -> case cur of
        StateInside -> signal cur Success

        -- Once again things happening to the left of us are independent from
        -- the things happening to our right, unless there are only two nodes
        -- in the overlay. In that case we want to be able to become a
        -- singleton.
        StatePredecessorLeaveRequest -> signal cur Success
        StatePredecessorLeaving -> signal cur Success

        StateInitialized -> signal cur Failure
        StateJoinRequest -> signal cur Failure
        StateJoining -> signal cur Failure
        StateLeaveRequest -> signal cur Failure
        StateLeaving -> signal cur Failure

    EventReset -> signal StateInitialized Success
  where
    (~>) :: State -> State -> (State, Signal)
    s ~> next
      | cur == s  = signal next Success
      | otherwise = signal cur  Failure

    signal :: State -> (SignalInfo -> Signal) -> (State, Signal)
    signal state f = (state, f (SignalInfo event cur state))

stepDksState
    :: Event
    -> E DksState
    -> DksState
    -> (DksState, Signal)
stepDksState event f s@DksState{_currentState = cur} =
    case dksStateTransitionFunction cur event of
        (_next, sig@(Failure _)) -> (s, sig)
        (next, sig@(Success _))  -> ((f s){_currentState = next}, sig)

-- }}} DksState ---------------------------------------------------------------

{-
State diagram in PlantUML format, see http://plantuml.com/state.html for
details.

@startuml node-state.png
title State diagram of a DKS node

[*] --> StateInitialized

note right of StateInitialized
  It is possible to get back in to StateInitialized,
  this includes StateInitialized itself, using EventReset.
  These transitions are omitted from this diagram to
  reduce noise.
end note

StateInitialized --> StateJoinRequest: EventJoinRequest
StateJoinRequest --> StateJoinRequest: EventJoinRetry
StateJoinRequest --> StateJoining: EventJoinPoint
StateInitialized --> StateInside: EventSelfJoinDone
StateJoining --> StateInside: EventJoinDone
StateInside --> StateInitialized: EventSelfLeaveDone
StateInside --> StateLeaveRequest: EventLeaveRequest
StateLeaveRequest --> StateInside: EventLeaveRetry
StateLeaveRequest --> StateLeaving: EventGrantLeave
StateInside --> StatePredecessorLeaveRequest: EventPredecessorLeaveRequest
StatePredecessorLeaveRequest --> StatePredecessorLeaving: EventPredecessorLeavePoint
StatePredecessorLeaving --> StateInside: EventPredecessorLeaveDone
StateLeaving --> StateInitialized: EventLeaveDone

StateInside --> StateInside: EventProcessingJoinRequest, EventNewSuccessor, EventNewSuccessorAck EventUpdateSuccessor

StateLeaveRequest --> StateLeaveRequest: EventNewSuccessor
StatePredecessorLeaveRequest --> StatePredecessorLeaveRequest: EventNewSuccessor, EventUpdateSuccessor
StatePredecessorLeaving --> StatePredecessorLeaving: EventNewSuccessor, EventUpdateSuccessor

StateInitialized --> [*]

@enduml
-}
