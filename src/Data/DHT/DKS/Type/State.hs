{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:       $HEADER$
-- Description:  State of a DKS node.
-- Copyright:    (c) 2015 Jan Šipr, Matej Kollár, 2015-2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- State of a DKS node.
module Data.DHT.DKS.Type.State
    (
    -- $stateDiagram

    -- * State of a DKS Node
      DksState(..)
    , stepDksState

    -- ** States
    , State(..)

    -- ** Events
    , Event(..)

    -- ** Signals
    , Signal(..)
    , SignalInfo(..)
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

-- | Current state of a DKS node. See '_currentState' field of 'DksState'.
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

-- | Events initiate state transition of 'DksState'.
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

-- | State of a DKS node that fully describes its current situation and
-- relation to other nodes in the overlay circle.
data DksState = DksState
    { _currentState :: !State
    -- ^ Symbolic 'State' in which this instance of DKS node currently is, i.e.
    -- status of this DKS node. This field can not be modified directly, only
    -- via state transition as described in the state diagram. Any attempt at
    -- modification will be discarded by 'stepDksState'.

    , _lock :: !Bool
    -- ^ DKS node lock is acquired during some transitions. It is used as an
    -- indicator of predecessor join/leave transaction.

    , _leaveForward :: !Bool
    -- ^ This node is leaving DKS overlay; any join requests will be forwarded
    -- to our successor.

    , _joinForward :: !Bool
    -- ^ Our future predecessor is joining the overlay; forward join requests.

    , _predecessor :: !(Maybe DksHash)
    -- ^ Current predecessor of this DKS node. Initiated as 'Nothing' when in
    -- 'StateInitialized', but can be set to 'Nothing' during network
    -- stabilization algorithm.

    , _oldPredecessor :: !(Maybe DksHash)
    -- ^ Previous predecessor of this DKS node. Note that referenced node may
    -- not be in the overlay anymore.

    , _successor :: !(Maybe DksHash)
    -- ^ Current successor of this DKS node. Initiated as 'Nothing' when in
    -- 'StateInitialized', but can be set to 'Nothing' during network
    -- stabilization algorithm.
    }
  deriving (Generic, Show, Typeable)

-- |
-- @
-- def = 'DksState'
--     { '_currentState' = def  -- = 'StateInitialized'
--     , '_lock' = False
--     , '_leaveForward' = False
--     , '_joinForward' = False
--     , '_predecessor' = Nothing
--     , '_oldPredecessor' = Nothing
--     , '_successor' = Nothing
--     }
-- @
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

-- | Safely modify 'DksState'.
stepDksState
    :: Event
    -- ^ 'Event' triggering a state transition.
    -> E DksState
    -- ^ Function that modifies 'DksState' iff the state transition is
    -- successful. Any attempt at modification of '_currentState' is discarded.
    -> DksState
    -- ^ Current value of 'DksState'.
    -> (DksState, Signal)
    -- ^ Next value of 'DksState' (after state transition) and a 'Signal' that
    -- indicated what state transition occurred. 'Signal' is very useful for
    -- debugging and error reporting.
stepDksState event f s@DksState{_currentState = cur} =
    case dksStateTransitionFunction cur event of
        (_next, sig@(Failure _)) -> (s, sig)
        (next, sig@(Success _))  -> ((f s){_currentState = next}, sig)

-- }}} DksState ---------------------------------------------------------------

-- $stateDiagram
--
-- <<doc/img/node-state.png State diagram of a DKS node>>
--
-- Based on /Figure 3.6 State transition diagram/ from
-- /Distributed k-ary System: Algorithms for Distributed Hash Tables/ (page 64)
-- by Ali Ghodsi.

{-

State diagram in PlantUML format, see http://plantuml.com/state.html for
details. For information about embedded PlantUML documents see
http://plantuml.com/sources.html

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
