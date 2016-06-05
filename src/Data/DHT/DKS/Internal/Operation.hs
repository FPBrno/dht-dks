{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:       $HEADER$
-- Description:  Operations (RPC calls and message handlers) of a DKS (DHT)
--               node.
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Operations (RPC calls and message handlers) of a DKS (DHT) node. Based on
-- /Distributed k-ary System: Algorithms for Distributed Hash Tables/ by Ali
-- Ghodsi.
module Data.DHT.DKS.Internal.Operation
    (
    -- * Operations that can be invoked on a DKS node
      DksOperation(..)

    -- ** Callbacks
    , OnJoin
    , OnLeave
    , OnResult
    , OnDone

    -- * Joining
    --
    -- $joining

    -- ** Join
    --
    -- $joinDefinition
    , handleJoin

    -- ** JoinRetry
    , handleJoinRetry

    -- ** JoinRequest
    --
    -- $joinReqDefinition
    , handleJoinRequest

    -- ** JoinPoint
    --
    -- $joinPointDefinition
    , handleJoinPoint

    -- ** NewSuccessor
    --
    -- $newSuccessorDefinition
    , handleNewSuccessor

    -- ** NewSuccessorAck
    --
    -- $newSuccessorAckDefinition
    , handleNewSuccessorAck

    -- ** JoinDone
    --
    -- $joinDoneDefinition
    , handleJoinDone

    -- * Leaving
    --
    -- $leaving

    -- ** Leave
    --
    -- $leaveDefinition
    , handleLeave

    -- ** LeaveRequest
    --
    -- $leaveReqDefinition
    , handleLeaveRequest

    -- ** LeaveRetry
    --
    -- $retryLeaveDefinition
    , handleLeaveRetry

    -- ** GrantLeave
    --
    -- $grantLeaveDefinition
    , handleGrantLeave

    -- ** LeavePoint
    --
    -- $leavePointDefinition
    , handleLeavePoint

    -- ** UpdateSuccessor
    --
    -- $updateSuccessorDefinition
    , handleUpdateSuccessor

    -- ** UpdateSuccessorAck
    --
    -- $updateSuccessorAckDefinition
    , handleUpdateSuccessorAck

    -- ** LeaveDone
    --
    -- $leaveDoneDefinition
    , handleLeaveDone
    )
  where

import Control.Monad (Monad((>>=)), return)
import Data.Bool (Bool(False, True), (&&), (||), not, otherwise)
import Data.Eq (Eq((/=), (==)))
import Data.Function (($), (.), const)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show(showsPrec), showString)
import System.IO (IO)

import Data.Default.Class (Default(def))
import Data.LogStr.Formatting ((%), shown)

import Data.DHT.Type.Encoding (Encoding)
import Data.DHT.Type.Hash
    ( Bound(Excluding, Including)
    , inInterval
    , isWholeSpace
    )
import Data.DHT.Type.Key (DhtKey)

import Data.DHT.DKS.Internal.Monad
    ( DksM
    , dksState
    , fromDksState
    , getSelf
    , handle
    , hash
    , joinFailure
    , joinSuccess
    , leaveFailure
    , leaveSuccess
    , logf
    , predecessorChanged
    , send
    , send_
    , stepDksState
    , successorChanged
    )

import Data.DHT.DKS.Type.EVar (EVar)
import Data.DHT.DKS.Type.Hash (DksHash)
import Data.DHT.DKS.Type.Message
    ( DksMessage
    , DksMessageHeader(DksMessageHeader, _from, _to)
    , IsDksMessage(dksMessage)
    )
import Data.DHT.DKS.Type.Message.GrantLeave (GrantLeave(GrantLeave))
import qualified Data.DHT.DKS.Type.Message.GrantLeave as GrantLeave
    ( GrantLeave(..)
    )
import Data.DHT.DKS.Type.Message.JoinDone (JoinDone(JoinDone))
import qualified Data.DHT.DKS.Type.Message.JoinDone as JoinDone (JoinDone(..))
import Data.DHT.DKS.Type.Message.JoinPoint (JoinPoint(JoinPoint))
import qualified Data.DHT.DKS.Type.Message.JoinPoint as JoinPoint
    ( JoinPoint(..)
    )
import Data.DHT.DKS.Type.Message.JoinRequest (JoinRequest(JoinRequest))
import qualified Data.DHT.DKS.Type.Message.JoinRequest as JoinRequest
    ( JoinRequest(..)
    )
import Data.DHT.DKS.Type.Message.JoinRetry (JoinRetry(JoinRetry))
import qualified Data.DHT.DKS.Type.Message.JoinRetry as JoinRetry
    ( JoinRetry(..)
    )
import Data.DHT.DKS.Type.Message.LeaveDone (LeaveDone(LeaveDone))
import qualified Data.DHT.DKS.Type.Message.LeaveDone as LeaveDone
    ( LeaveDone(..)
    )
import Data.DHT.DKS.Type.Message.LeavePoint (LeavePoint(LeavePoint))
import qualified Data.DHT.DKS.Type.Message.LeavePoint as LeavePoint
    ( LeavePoint(..)
    )
import Data.DHT.DKS.Type.Message.LeaveRequest (LeaveRequest(LeaveRequest))
import qualified Data.DHT.DKS.Type.Message.LeaveRequest as LeaveRequest
    ( LeaveRequest(..)
    )
import Data.DHT.DKS.Type.Message.LeaveRetry (LeaveRetry(LeaveRetry))
import qualified Data.DHT.DKS.Type.Message.LeaveRetry as LeaveRetry
    ( LeaveRetry(..)
    )
import Data.DHT.DKS.Type.Message.NewSuccessor (NewSuccessor(NewSuccessor))
import qualified Data.DHT.DKS.Type.Message.NewSuccessor as NewSuccessor
    ( NewSuccessor(..)
    )
import Data.DHT.DKS.Type.Message.NewSuccessorAck
    ( NewSuccessorAck(NewSuccessorAck)
    )
import qualified Data.DHT.DKS.Type.Message.NewSuccessorAck as NewSuccessorAck
    ( NewSuccessorAck(..)
    )
import Data.DHT.DKS.Type.Message.UpdateSuccessor
    ( UpdateSuccessor(UpdateSuccessor)
    )
import qualified Data.DHT.DKS.Type.Message.UpdateSuccessor as UpdateSuccessor
    ( UpdateSuccessor(..)
    )
import Data.DHT.DKS.Type.Message.UpdateSuccessorAck
    ( UpdateSuccessorAck(UpdateSuccessorAck)
    )
import qualified Data.DHT.DKS.Type.Message.UpdateSuccessorAck
  as UpdateSuccessorAck
    ( UpdateSuccessorAck(..)
    )
import Data.DHT.DKS.Type.State
    ( DksState
        ( DksState
        , _joinForward
        , _leaveForward
        , _lock
        , _oldPredecessor
        , _predecessor
        , _successor
        )
    , Event
        ( EventGrantLeave
        , EventJoinDone
        , EventJoinPoint
        , EventJoinRequest
        , EventLeaveDone
        , EventLeaveRequest
        , EventLeaveRetry
        , EventNewSuccessor
        , EventNewSuccessorAck
        , EventPredecessorLeaveDone
        , EventPredecessorLeavePoint
        , EventPredecessorLeaveRequest
        , EventProcessingJoinRequest
        , EventSelfJoinDone
        , EventSelfLeaveDone
        , EventUpdateSuccessor
        )
    )


-- | Type of a callback invoked when node either failed to join the DHT overlay
-- network or if it succeeded. Callback either gets an exception (see 'EVar'
-- for details) or a pair @(predecessor, successor)@.
type OnJoin = EVar (Maybe DksHash, Maybe DksHash) -> IO ()

-- | Type of a callback invoked when node either failed to leave the DHT
-- overlay or if it succeeded.
type OnLeave = EVar () -> IO ()

type OnResult a = EVar a -> IO ()
type OnDone = OnResult ()


-- | DKS node receives events in the form of 'DksOperation's which are then
-- processed by a specialized handlers.
--
-- Implementation notes for 'DksOperation':
--
-- * Callback should always be the first argument of a constructor, because it
--   is most likely to be bound first and the rest will probably be left free.
--
-- * Arguments should be strict, so that DKS node is not responsible for
--   unneccessary evaluation.
data DksOperation
    = JoinOp !OnJoin !(Maybe DksHash)
    -- ^ Application (in which DKS node is embedded) is requesting it to join a
    -- DHT overlay using provided node as entry\/bootstrap node. If the
    -- bootstrap node is 'Nothing' then the DKS node should self-join.

    | LeaveOp !(Maybe OnLeave)
    -- ^ Application (in which DKS node is embedded) is requesting it to leave
    -- a DHT overlay network.

    | LookupOp !(OnResult Encoding) !DhtKey
    -- ^ Application (in which DKS node is embedded) is requesting it to lookup
    -- a value in a DHT overlay network which it is part of.

    | InsertOp !(Maybe OnDone) !DhtKey !Encoding
    -- ^ Application (in which DKS node is embedded) is requesting it to
    -- insert\/store a key-value pair in a DHT.

    | ProcessMessageOp !(EVar DksMessage)
    -- ^ Network layer has received a message for this DKS (DHT) node.

    | GetStateOp !(DksState -> IO ())
    -- ^ Application (in which DKS node is embedded) is requesting it to
    -- provide its current state. Useful for debugging.
  deriving (Generic, Typeable)

instance Show DksOperation where
    showsPrec _ = brackets . \case
        JoinOp _ n -> showString "JoinOp " . showsPrec 10 n
        LeaveOp _ -> showString "LeaveOp"
        LookupOp _ k -> showString "LookupOp " . showsPrec 10 k
        InsertOp _ k _ -> showString "InsertOp " . showsPrec 10 k
        ProcessMessageOp m -> showString "ProcessMessageOp " . showsPrec 10 m
        GetStateOp _ -> showString "GetStateOp"
      where
        brackets = (showString "<<" .) . (. showString ">>")

-- {{{ Joining ----------------------------------------------------------------
-- {{{ Join -------------------------------------------------------------------

handleJoin :: Maybe DksHash -> DksM ()
handleJoin = handle joinFailure . \case
    Nothing -> do
        self <- getSelf
        stepDksState EventSelfJoinDone $ \s -> s
            { _lock = False
            , _predecessor = Just self
            , _successor = Just self
            , _oldPredecessor = Nothing
            }
        logf (hash % ": Self join successful.") self
        predecessorChanged Nothing (Just self)
        successorChanged Nothing (Just self)
        joinSuccess (Just self) (Just self)

    Just node -> do
        -- Entires _oldPredecessor, _predecessor, and _successor are kept on
        -- their default value, which is Nothing.
        stepDksState EventJoinRequest $ \s -> s{_lock = True}
        self <- getSelf
        logf (hash % ": Sending join request to " % hash % ".") self node
        send $ dksMessage
            DksMessageHeader{_to = node, _from = self}
            JoinRequest{JoinRequest._requester = self}

-- $joinDefinition
--
-- > event n.Join(e) from app
-- >   if e = nil then
-- >     lock := free
-- >     pred := n
-- >     succ := n
-- >     status := inside    # Line missing in original paper.
-- >   else
-- >     lock := taken
-- >     pred := nil
-- >     succ := nil
-- >     status := joinreq
-- >     sendto e.JoinReq(n)
-- >   end if
-- > end event
--
-- Based on /Algorithm 7 Optimized atomic join algorithm/ from page 66.

-- }}} Join -------------------------------------------------------------------

-- {{{ JoinRetry --------------------------------------------------------------

handleJoinRetry :: JoinRetry -> DksM ()
handleJoinRetry _msg = return ()     -- TODO

-- }}} JoinRetry --------------------------------------------------------------

-- {{{ JoinRequest ------------------------------------------------------------

handleJoinRequest :: DksHash -> JoinRequest -> DksM ()
handleJoinRequest from msg@JoinRequest{JoinRequest._requester = rqstr} = do
    DksState
        { _joinForward = joinFwd
        , _oldPredecessor = oldPred
        , _predecessor = pred
        , _successor = succ
        , _leaveForward = leaveFwd
        , _lock = locked
        } <- dksState
    self <- getSelf

    let joinRetry = dksMessage
            DksMessageHeader{_to = from, _from = self}
            JoinRetry{JoinRetry._requester = rqstr}

        sendJoinRetry = do
            logf (hash % ": Respond with JoinRetry " % shown % " to "
                % hash % ".") self joinRetry from
            send_ joinRetry

        forwardJoinRequestTo n = do
            logf (hash % ": Forwarding JoinRequest " % shown % " to "
                % hash % ".") self msg n
            send_ $ dksMessage DksMessageHeader{_to = n, _from = from} msg

    if
      -- Check if this node can accept join request and if not then
      -- forward it to predecessor.
      | joinFwd && oldPred == Just rqstr, Just p <- oldPred ->
            forwardJoinRequestTo p

      -- Check if this node can accept join request and if not then
      -- forward it to successor.
      | leaveFwd || notResponsibleForRequester self pred ->
            maybe sendJoinRetry forwardJoinRequestTo succ

      -- If node is inside a transaction, then send retry.
      | locked -> sendJoinRetry

      | Just p <- pred -> do
            stepDksState EventProcessingJoinRequest $ \s -> s
                { _lock = True
                , _joinForward = True
                , _oldPredecessor = _predecessor s
                , _predecessor = Just rqstr
                }
            logf (hash % ": Sending JoinPoint to " % hash % ".") self rqstr
            send_ $ dksMessage
                DksMessageHeader{_to = rqstr, _from = self}
                JoinPoint
                    { JoinPoint._requester = rqstr
                    , JoinPoint._predecessor = p
                    , JoinPoint._successor = self
                    }

      -- pred == Nothing
      | otherwise -> sendJoinRetry  -- TODO: What does this case mean?
  where
    notResponsibleForRequester self = maybe False $ \n ->
        -- (n, pred] actually is I \ [pred, n)
        let bounds = (Excluding self, Including n)
        -- isWholeSpace bounds = True <==> (Excluding self, Including self),
        -- i.e. we are the only node in the DHT and therefore responsible for
        -- any joining node.
        in not (isWholeSpace bounds) && inInterval bounds rqstr

-- $joinReqDefinition
--
-- > event n.JoinReq(d) from m
-- >   if JoinForward and m = oldpred then
-- >     sendto pred.JoinReq(d)     # Join Forwarding.
-- >   else if LeaveForward then
-- >     sendto succ.JoinReq(d)     # Leave Forwarding.
-- >   else if pred ≠ nil and d ∈ (n, pred] and pred ≠ n then
-- >     # Paper doesn't include "pred ≠ n" condition. When "pred = n" then
-- >     # (n, pred] = (n, n] = I and that causes node to forward join request
-- >     # to itself.
-- >     sendto succ.JoinReq(d)
-- >   else
-- >     if lock ≠ nil or pred = nil then
-- >       sendto m.RetryJoin()
-- >     else
-- >       JoinForward := true
-- >       lock := taken
-- >       sendto d.JoinPoint(pred) # Paper mentions "sendto m.JoinPoint(pred)".
-- >       oldpred := pred
-- >       pred := d                # Paper mentions "pred := m".
-- >     end if
-- >   end if
-- > end event
--
-- Based on /Algorithm 7 Optimized atomic join algorithm/ from page 66.

-- }}} JoinRequest ------------------------------------------------------------

-- {{{ JoinPoint --------------------------------------------------------------

handleJoinPoint :: JoinPoint -> DksM ()
handleJoinPoint msg@JoinPoint{JoinPoint._requester = rqstr} = do
    self <- getSelf
    if rqstr /= self
        -- Message is not for us, and we are unable (node is not inside the
        -- DHT) to forward it.
        then logf (hash % ": Received JoinPoint which is not intended for us;"
            % " message discarded " % shown % ".") self msg
        else do
            let JoinPoint
                    { JoinPoint._predecessor = pred
                    , JoinPoint._successor = succ
                    } = msg
            stepDksState EventJoinPoint $ \s -> s
                { _predecessor = Just pred
                , _successor = Just succ
                }
            predecessorChanged Nothing (Just pred)
            successorChanged Nothing (Just succ)
            send $ dksMessage
                DksMessageHeader{_to = pred, _from = self}
                NewSuccessor
                    { NewSuccessor._requester = self
                    , NewSuccessor._oldSuccessor = succ
                    , NewSuccessor._successor = self
                    }

-- $joinPointDefinition
--
-- > event n.JoinPoint(p) from m
-- >   status := joining
-- >   pred := p
-- >   succ := m
-- >   sendto pred.NewSuccessor
-- > end event
--
-- /Algorithm 8 Optimized atomic join algorithm continued/ from page 67.

-- }}} JoinPoint --------------------------------------------------------------

-- {{{ NewSuccessor -----------------------------------------------------------

handleNewSuccessor :: NewSuccessor -> DksM ()
handleNewSuccessor msg@NewSuccessor{NewSuccessor._successor = newSucc} =
    fromDksState _successor >>= \case
        Nothing      -> return ()
        Just oldSucc -> do
            stepDksState EventNewSuccessor $ \s -> s
                { _successor = Just newSucc
--              , _oldSuccessor = oldSucc
                }
            successorChanged (Just oldSucc) (Just newSucc)
            self <- getSelf
            send $ dksMessage
                DksMessageHeader{_to = oldSucc, _from = self}
                NewSuccessorAck
                    { NewSuccessorAck._requester = NewSuccessor._requester msg
                    , NewSuccessorAck._oldSuccessor = oldSucc
                    , NewSuccessorAck._successor = newSucc
                    }

-- $newSuccessorDefinition
--
-- > event n.NewSucc() from m
-- >   sendto succ.NewSuccessorAck(m)
-- >   succ := m
-- > end event

-- }}} NewSuccessor -----------------------------------------------------------

-- {{{ NewSuccessorAck --------------------------------------------------------

handleNewSuccessorAck :: NewSuccessorAck -> DksM ()
handleNewSuccessorAck NewSuccessorAck{NewSuccessorAck._requester = rqstr} =
    fromDksState _oldPredecessor >>= \case
        Nothing      -> return () -- Error.
        Just oldPred -> do
            stepDksState EventNewSuccessorAck $ \s -> s
                { _lock = False
                , _joinForward = False
                }
            self <- getSelf
            logf (hash % ": Sending JoinDone to " % hash % ".") self rqstr
            send $ dksMessage
                DksMessageHeader{_to = rqstr, _from = self}
                JoinDone
                    { JoinDone._requester = rqstr
                    , JoinDone._successor = self
                    , JoinDone._predecessor = oldPred
                    }

-- $newSuccessorAckDefinition
--
-- > event n.NewSuccAck(q) from m
-- >   lock := free
-- >   joinForward := false
-- >   sendto q.JoinDone()
-- > end event

-- }}} NewSuccessorAck --------------------------------------------------------

-- {{{ JoinDone ---------------------------------------------------------------

handleJoinDone :: JoinDone -> DksM ()
handleJoinDone JoinDone{} = handle joinFailure $ do
    stepDksState EventJoinDone $ \s -> s{_lock = False}
    (pred, succ) <- fromDksState $ \s -> (_predecessor s, _successor s)
    self <- getSelf
    logf (hash % ": Successfully joined DHT network (JoinDone); "
        % "our predecessor is " % shown % " and successor is " % shown % ".")
        self pred succ
    joinSuccess pred succ

-- $joinDoneDefinition
--
-- > event n.JoinDone() from m
-- >   lock := free
-- >   status := inside
-- > end event

-- }}} JoinDone ---------------------------------------------------------------

-- $joining
--
-- <<doc/img/node-joining-overlay.png Sequence diagram: Node joining overlay>>
--
-- Based on /Figure 3.7 Thime-space diagram of the successful join of a node/
-- from page 68.

{-
Sequence diagram in PlantUML format see http://plantuml.net/sequence.html for
details. For information about embedded PlantUML documents see
http://plantuml.com/sources.html

@startuml node-joining-overlay.png

title Node q joining DKS overlay network
hide footbox

participant "Node p" as P
participant "Node q (joining)" as Q
participant "Node r" as R

Q -> R: JoinRequest
loop while R locked or predecessor = nil
  R -> Q: JoinRetry
  ...
  Q -> R: JoinRequest
end
R -> Q: JoinPoint
Q -> P: NewSuccessor
P -> R: NewSuccessorAck
R -> Q: JoinDone

@enduml
-}

-- }}} Joining ----------------------------------------------------------------

-- {{{ Leaving ----------------------------------------------------------------
-- {{{ Leave ------------------------------------------------------------------

handleLeave :: DksM ()
handleLeave = handle leaveFailure $ do
    DksState
        { _lock = locked
        , _predecessor = possiblyPred
        , _successor = possiblySucc
        } <- dksState
    self <- getSelf
    if
      | locked -> retryLeave

        -- Singleton (only node in DHT overlay) can leave on its own. Note that
        -- "succ == pred" does not guarantee that the node is a singleton. This
        -- happens also when only two nodes are in a network.
      | possiblyPred == possiblySucc && possiblyPred == Just self -> do
            stepDksState EventSelfLeaveDone $ const def
            logf (hash % ": Self leave done.") self
            predecessorChanged possiblyPred Nothing
            successorChanged possiblySucc Nothing
            leaveSuccess

      | Just succ <- possiblySucc -> do
            stepDksState EventLeaveRequest $ \s -> s{_lock = True}
            logf (hash % ": Sendign LeaveRequest to " % hash % ".") self succ
            send $ dksMessage
                DksMessageHeader{_to = succ, _from = self}
                LeaveRequest{LeaveRequest._requester = self}
            leaveSuccess

        -- succ == Nothing
      | otherwise -> retryLeave
        -- TODO: What does this mean? Stabilization? Error?
  where
    retryLeave = return ()  -- TODO

-- $leaveDefinition
--
-- > event n.Leave() from app
-- >   if lock ≠ free then
-- >     # Application should try again later.
-- >     noop                                   # Line not in original paper
-- >   else if succ = pred and succ = n then    # Last node, can quit.
-- >     noop                                   # Line not in original paper.
-- >   else
-- >     status := leavereq
-- >     lock := taken
-- >     sendto succ.LeaveReq()
-- >   end if
-- > end event
--
-- Based on /Algorithm 9 Optimized atomic leave algorithm/ from page 70.

-- }}} Leave ------------------------------------------------------------------

-- {{{ LeaveRequest -----------------------------------------------------------

handleLeaveRequest :: LeaveRequest -> DksM ()
handleLeaveRequest LeaveRequest{LeaveRequest._requester = rqstr} = do
    self <- getSelf
    DksState{_lock = locked, _predecessor = pred} <- dksState
    if not locked && pred == Just rqstr
        then do
            stepDksState EventPredecessorLeaveRequest $ \s -> s{_lock = True}
            logf (hash % ": GrantLeave to " % hash % ".") self rqstr
            send $ dksMessage
                DksMessageHeader{_to = rqstr, _from = self}
                GrantLeave{GrantLeave._requester = rqstr}
        else do
            logf (hash % ": Unable to GrantLeave to " % hash % ".") self rqstr
            send $ dksMessage
                DksMessageHeader{_to = rqstr, _from = self}
                LeaveRetry{LeaveRetry._requester = rqstr}

-- $leaveReqDefinition
--
-- > event n.LeaveReq() from m
-- >   if lock = free then
-- >     lock := taken
-- >     sendto m.GrantLeave()
-- >     state := predleavereq
-- >   else                      # Paper uses "else if lock ≠ free then".
-- >     sendto m.RetryLeave()
-- >   end if
-- > end event
--
-- Based on /Algorithm 9 Optimized atomic leave algorithm/ from page 70.

-- }}} LeaveRequest -----------------------------------------------------------

-- {{{ LeaveRetry -------------------------------------------------------------

handleLeaveRetry :: DksHash -> LeaveRetry -> DksM ()
handleLeaveRetry from msg@LeaveRetry{LeaveRetry._requester = rqstr} = do
    self <- getSelf
    if rqstr == self
        then do
            stepDksState EventLeaveRetry $ \s -> s{_lock = False}
            logf (hash % ": LeaveRetry: Our leave request wasn't granted.")
                self
            retryLeave
        else do
            logf (hash % ": Received LeaveRetry for someone else, "
                % "resending to intended recipient " % hash % ".") self rqstr
            send $ dksMessage DksMessageHeader{_to = rqstr, _from = from} msg
  where
    retryLeave = return ()  -- TODO

-- $retryLeaveDefinition
--
-- > event n.RetryLeave()
-- >   status := inside
-- >   lock := free          # Retry leaving later.
-- > end event
--
-- /Algorithm 9 Optimized atomic leave algorithm/ from page 70.

-- }}} LeaveRetry -------------------------------------------------------------

-- {{{ GrantLeave -------------------------------------------------------------

handleGrantLeave :: DksHash -> GrantLeave -> DksM ()
handleGrantLeave from msg@GrantLeave{GrantLeave._requester = rqstr} = do
    self <- getSelf
    DksState{_predecessor = ppred, _successor = psucc} <- dksState
    if
      | rqstr == self, Just pred <- ppred, Just succ <- psucc -> do
            stepDksState EventGrantLeave $ \s -> s{_leaveForward = True}
            logf (hash % ": We have been granted a leave (GrantLeave).") self
            logf (hash % ": Sending LeavePoint to successor " % hash % ".")
                self succ
            send $ dksMessage
                DksMessageHeader{_to = succ, _from = self}
                LeavePoint
                    { LeavePoint._requester = self
                    , LeavePoint._predecessor = pred
                    }

      -- We have received GrantLeave intended for someone else.
      | rqstr /= self -> do
            logf (hash % ": Passing grant leave to: " % hash) self rqstr
            send $ dksMessage DksMessageHeader{_to = rqstr, _from = from} msg

      | otherwise -> return ()  -- TODO: What does this mean?

-- $grantLeaveDefinition
--
-- > event n.GrantLeave()
-- >   LeaveForward := true
-- >   status := leaving
-- >   sendto m.LeavePoint(pred)
-- > end event
--
-- /Algorithm 9 Optimized atomic leave algorithm/ from page 70.

-- }}} GrantLeave -------------------------------------------------------------

-- {{{ LeavePoint -------------------------------------------------------------

handleLeavePoint :: LeavePoint -> DksM ()
handleLeavePoint msg@LeavePoint{LeavePoint._requester = rqstr} = do
    oldPred <- fromDksState _predecessor
    if oldPred /= Just rqstr
        then return ()  -- TODO: What does this mean?
        else do
            self <- getSelf
            let newPred = LeavePoint._predecessor msg
            stepDksState EventPredecessorLeavePoint $ \s -> s
                { _predecessor = Just newPred
                , _oldPredecessor = oldPred
                }
            predecessorChanged oldPred (Just newPred)
            logf (hash % ": Predecessor " % hash % " is leaving"
                % "; sending UpdateSuccessor to our new predecessor " % hash
                % ".") self rqstr newPred
            send $ dksMessage
                DksMessageHeader{_to = newPred, _from = self}
                UpdateSuccessor
                    { UpdateSuccessor._requester = rqstr
                    , UpdateSuccessor._successor = self
                    }

-- $leavePointDefinition
--
-- > event n.LeavePoint(q) from m
-- >   status := predleaving
-- >   pred := q
-- >   sendto pred.UpdateSucc()
-- > end event
--
-- /Algorithm 10 Optimized atomic leave algorithm continued/ from page 71.

-- }}} LeavePoint -------------------------------------------------------------

-- {{{ UpdateSuccessor --------------------------------------------------------

handleUpdateSuccessor :: UpdateSuccessor -> DksM ()
handleUpdateSuccessor msg = do
    let UpdateSuccessor
            { UpdateSuccessor._requester = rqstr
            , UpdateSuccessor._successor = newSucc
            } = msg
    fromDksState _successor >>= \case
        Nothing      -> return ()   -- TODO: What does this mean?
        Just oldSucc
          | oldSucc /= rqstr -> return ()   -- TODO: What does this mean?
          | otherwise        -> do
                stepDksState EventUpdateSuccessor $ \s -> s
                    { _successor = Just newSucc
                    }
                self <- getSelf
                logf (hash % ": Successor changed from " % hash % " to "
                    % hash % ".") self oldSucc newSucc
                send $ dksMessage
                    DksMessageHeader{_to = oldSucc, _from = self}
                    UpdateSuccessorAck
                        { UpdateSuccessorAck._requester = rqstr
                        , UpdateSuccessorAck._oldSuccessor = oldSucc
                        , UpdateSuccessorAck._successor = newSucc
                        }

-- $updateSuccessorDefinition
--
-- > event n.UpdateSucc() from m
-- >   sendto succ.UpdateSuccAck()
-- >   succ := m
-- > end event
--
-- /Algorithm 10 Optimized atomic leave algorithm continued/ from page 71.

-- }}} UpdateSuccessor --------------------------------------------------------

-- {{{ UpdateSuccessorAck -----------------------------------------------------

handleUpdateSuccessorAck :: UpdateSuccessorAck -> DksM ()
handleUpdateSuccessorAck msg = do
    let UpdateSuccessorAck
            { UpdateSuccessorAck._requester = rqstr
            , UpdateSuccessorAck._oldSuccessor = predOldSucc
            , UpdateSuccessorAck._successor = predNewSucc
            } = msg
    fromDksState _successor >>= \case
        Nothing   -> return ()  -- TODO: What does this mean?
        Just succ -> do
            self <- getSelf
            if rqstr == self && predOldSucc == self && predNewSucc == succ
                then do
                    stepDksState EventLeaveDone $ const def
                    send_ $ dksMessage
                        DksMessageHeader{_to = succ, _from = self}
                        LeaveDone{LeaveDone._requester = self}
                    logf (hash % ": Successfully left the overlay.") self
                    leaveSuccess
                else return ()  -- TODL: What does this mean?

-- $updateSuccessorAckDefinition
--
-- > event n.UpdateSuccessorAck() from m
-- >   sendto succ.LeaveDone()              # Leave the system.
-- > end event
--
-- /Algorithm 10 Optimized atomic leave algorithm continued/ from page 71.

-- }}} UpdateSuccessorAck -----------------------------------------------------

-- {{{ LeaveDone --------------------------------------------------------------

handleLeaveDone :: LeaveDone -> DksM ()
handleLeaveDone LeaveDone{LeaveDone._requester = rqstr} = do
    oldPred <- fromDksState _oldPredecessor
    self <- getSelf
    if oldPred == Just rqstr
        then do
            logf (hash % ": Old predecessor left the overlay successfully: "
                % hash) self rqstr
            stepDksState EventPredecessorLeaveDone $ \s -> s{_lock = False}
        else do
            logf (hash % ": Discarding LeaveDone message from " % hash
                % " that doesn't belong to our old predecessor " % shown) self
                rqstr oldPred

-- $leaveDoneDefinition
--
-- > event n.LeaveDone() from m
-- >   lock := free
-- >   status := inside
-- > end event
--
-- /Algorithm 10 Optimized atomic leave algorithm continued/ from page 71.

-- }}} LeaveDone --------------------------------------------------------------

-- $leaving
--
-- <<doc/img/node-leaving-overlay.png Sequence diagram: Node leaving overlay>>
--
-- Based on /Figure 3.8 Thime-space diagram of the successful leave of a node/
-- from page 72.

{-
Sequence diagram in PlantUML format see http://plantuml.net/sequence.html for
details. For information about embedded PlantUML documents see
http://plantuml.com/sources.html

@startuml node-leaving-overlay.png

title Node q leaving DKS overlay network
hide footbox

participant "Node p" as P
participant "Node q (leaving)" as Q
participant "Node r" as R

Q -> R: LeaveRequest
loop while R locked
  R -> Q: LeaveRetry
  ...
  Q -> R: LeaveRequest
end
R -> Q: GrantLeave
Q -> R: LeavePoint
R -> P: UpdateSuccessor
P -> Q: UpdateSuccessorAck
Q -> R: LeaveDone
destroy Q

@enduml
-}

-- }}} Leaving ----------------------------------------------------------------
