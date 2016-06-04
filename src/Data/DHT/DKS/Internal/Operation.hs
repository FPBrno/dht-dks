{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2016 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Data.DHT.DKS.Internal.Operation
    ( DksOperation(..)
    , OnJoin
    , OnLeave
    , OnResult
    , OnDone

    -- * Joining

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
    -- TODO
    )
  where

import Control.Monad (Monad((>>=)), return)
import Data.Bool (Bool(False, True), (&&), (||), not, otherwise)
import Data.Eq (Eq((/=), (==)))
import Data.Function (($), (.))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show(showsPrec), showString)
import System.IO (IO)

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
        ( EventJoinDone
        , EventJoinPoint
        , EventJoinRequest
        , EventNewSuccessor
        , EventNewSuccessorAck
        , EventProcessingJoinRequest
        , EventSelfJoinDone
        )
    )


type OnJoin = EVar (Maybe DksHash, Maybe DksHash) -> IO ()
type OnLeave = EVar () -> IO ()

type OnResult a = EVar a -> IO ()
type OnDone = OnResult ()


-- | Implementation notes for DksOperation:
--
-- * Callback should always be the first argument of a constructor, because it
--   is most likely to be boud first and the rest will probably be left free.
data DksOperation
    = JoinOp !OnJoin !(Maybe DksHash)
--  | JoinRetry !OnJoin !DksHash
    | LeaveOp !(Maybe OnLeave)
    | LookupOp !(OnResult Encoding) !DhtKey
    | InsertOp !(Maybe OnDone) !DhtKey !Encoding
    | ProcessMessageOp !(EVar DksMessage)
    | GetStateOp !(DksState -> IO ())
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
        logf (hash % ": Sending join request to " % hash) self node
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
        , _lock = lck
        } <- dksState
    self <- getSelf

    let joinRetry = dksMessage
            DksMessageHeader{_to = from, _from = self}
            JoinRetry{JoinRetry._requester = rqstr}

        sendJoinRetry = do
            logf (hash % ": Respond with retry join: " % shown % " to: "
                % hash) self joinRetry from
            send_ joinRetry

        forwardJoinRequestTo n = do
            logf (hash % ": Forwarding join request: " % shown % " to: "
                % hash) self msg n
            send_ $ dksMessage DksMessageHeader{_to = n, _from = from} msg

    if
      -- Check if this node can accept join request and if not then
      -- forward it to predecessor.
      | joinFwd && oldPred == Just rqstr, Just p <- oldPred ->
            forwardJoinRequestTo p

      -- Check if this node can accept join request and if not then
      -- forward it to successor in following cases:
      --
      -- * Leaving is in progress.
      -- * Node doesn't belong to the interval we are responsible for.
      | leaveFwd || responsibleForRequester self pred ->
            maybe sendJoinRetry forwardJoinRequestTo succ

      -- If node is inside a transaction, then send retry.
      | lck == True -> sendJoinRetry

      | Just p <- pred -> do
            stepDksState EventProcessingJoinRequest $ \s -> s
                { _lock = True
                , _joinForward = True
                , _oldPredecessor = _predecessor s
                , _predecessor = Just rqstr
                }
            send_ $ dksMessage
                DksMessageHeader{_to = rqstr, _from = self}
                JoinPoint
                    { JoinPoint._requester = rqstr
                    , JoinPoint._predecessor = p
                    , JoinPoint._successor = self
                    }

      -- pred == Nothing
      | otherwise -> sendJoinRetry
  where
    responsibleForRequester self = maybe False $ \n ->
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
-- >     #to its self.
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
        then logf (hash % ": Received JoinPoint which is not intended for us."
            % " Message discarded: " % shown) self msg
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
    joinSuccess pred succ

-- $joinDoneDefinition
--
-- > event n.JoinDone() from m
-- >   lock := free
-- >   status := inside
-- > end event

-- }}} JoinDone ---------------------------------------------------------------

-- {{{ Leave ------------------------------------------------------------------

-- $leaveDefinition
--
-- > event n.Leave() from app
-- >   if lock ≠ free then                   # Application should try again
-- >                                         #   later.
-- >     noop                                # Line not in original paper
-- >   else if succ = pred and succ = n then # Last node, can quit.
-- >     noop                                # Line not in original paper.
-- >   else
-- >     status := leavereq
-- >     lock := true
-- >     sendto succ.LeaveReq()
-- >   end if
-- > end event
--
-- Based on /Algorithm 9 Optimized atomic leave algorithm/ from page 70.

-- }}} Leave ------------------------------------------------------------------

-- {{{ LeaveRequest -----------------------------------------------------------

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

-- {{{ RetryLeave -------------------------------------------------------------

-- $retryLeaveDefinition
--
-- > event n.RetryLeave()
-- >   status := inside
-- >   lock := free          # Retry leaving later.
-- > end event
--
-- /Algorithm 9 Optimized atomic leave algorithm/ from page 70.

-- }}} RetryLeave -------------------------------------------------------------

-- {{{ GrantLeave -------------------------------------------------------------

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
