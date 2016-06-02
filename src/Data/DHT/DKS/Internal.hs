{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
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
module Data.DHT.DKS.Internal
  where

--import Control.Applicative (Applicative((<*>)))
import Control.Concurrent (ThreadId)
import Control.Exception (Exception(toException), SomeException)
import Control.Monad (Monad((>>=)), (>=>), forever, return, void)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (Bool(False, True), (&&), (||), not, otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq((/=), (==)))
import Data.Function (($), (.), flip)
import Data.Functor (Functor(fmap), {-(<$), -}(<$>))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import System.IO (IO)
import Text.Show (Show(showsPrec), showChar, shows, showString)

import Control.Concurrent.Chan.Unagi
    ( InChan
    , newChan
    , readChan
    , writeChan
    )
import Control.Monad.Except (ExceptT(ExceptT), runExceptT, throwError)
import Data.Default.Class (Default(def))
import Data.DHT (DhtKey, Encoding)
import Data.DHT.Type.Hash (Bound(Excluding, Including), isWholeSpace)
import qualified Data.DHT.Type.Hash as Hash (inInterval)
--import Data.HashMap.Strict (HashMap)
import Data.LogStr.Formatting ((%), format, shown)
import Data.Monoid.Endo (E)
import System.Lumberjack.Backend (SomeLoggingBackend, pushLogStrLn)

import Data.DHT.DKS.Type.DksState
    ( DksState
        ( DksState
        , _joinForward
        , _leaveForward
        , _lock
        , _oldPredecessor
--      , _oldSuccessor
        , _predecessor
        , _successor
        )
    , Event
--      ( EventGrantLeave
        ( EventJoinDone
        , EventJoinPoint
        , EventJoinRequest
--      , EventJoinRetry
--      , EventLeaveRequest
--      , EventLeaveRetry
        , EventProcessingJoinRequest
        , EventNewSuccessor
        , EventNewSuccessorAck
--      , EventPredecessorLeaveDone
--      , EventPredecessorLeavePoint
--      , EventPredecessorLeaveRequest
--      , EventReset
        , EventSelfJoinDone
        )
    , Signal(Failure, Success)
    , SignalInfo
    )
import qualified Data.DHT.DKS.Type.DksState as Internal (stepDksState)
import Data.DHT.DKS.Type.EVar
    ( EVar
    , EVarIO
    , failure
    , success
    , success_
    )
import Data.DHT.DKS.Type.Hash (DksHash)
import Data.DHT.DKS.Type.Message
    ( DksMessage(DksMessage, _body, _header)
    , DksMessageBody
        ( JoinDoneBody
        , JoinPointBody
        , JoinRequestBody
        , NewSuccessorAckBody
        , NewSuccessorBody
        )
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
import Data.DHT.DKS.Type.MessageChannel
    ( DksMessageChannel
        ( registerReceiveMessage
        , sendMessage
        )
    )
import Data.DHT.DKS.Type.Params
    ( DksParams
        ( DksParams
        , _discovery
        , _logging
        , _runThread
        , _singleton
        , _yield
        )
    )


type M = ExceptT SomeException IO

throw :: Exception e => e -> M a
throw = throwError . toException

runM :: (SomeException -> IO a) -> M a -> IO a
runM handler = runExceptT >=> \case
    Left e -> handler e
    Right a -> return a

-- {{{ State ------------------------------------------------------------------

data DksException = DksStateTransitionFailed SignalInfo
  deriving (Eq, Show)

instance Exception DksException

-- | State of a main thread of DKS node.
data ThreadState = ThreadState
    { _dksState :: DksState
--  , _storage ::
    }

mkThreadState :: DksParams -> IO ThreadState
mkThreadState _opts = return ThreadState
    { _dksState = def
--  , _storage =
    }

-- | Mutable 'ThreadState' that exposes only read and (atomic) modify
-- operations.
data BoxedThreadState = BoxedThreadState
    { _getThreadState :: !(IO ThreadState)
    -- ^ Read current thread state.

    , _modifyThreadState
        :: !(forall b. (ThreadState -> (ThreadState, b)) -> IO b)
    -- ^ Atomicaly modify thread state.
    }

-- | Smart constructor for 'BoxedThreadState'.
mkBoxedThreadState :: DksParams -> IO BoxedThreadState
mkBoxedThreadState opts = mk <$> (mkThreadState opts >>= newIORef)
  where
    mk stateRef = BoxedThreadState
        { _getThreadState = readIORef stateRef
        , _modifyThreadState = atomicModifyIORef' stateRef
        }

askThreadState :: BoxedThreadState -> (ThreadState -> a) -> IO a
askThreadState BoxedThreadState{_getThreadState = get} f = f <$> get

-- }}} State ------------------------------------------------------------------

-- {{{ Operations -------------------------------------------------------------

type OnJoin = EVar () -> IO ()
type OnLeave = EVar () -> IO ()

type OnResult a = EVar a -> IO ()
type OnDone = OnResult ()

data DksOperation
    = JoinOp !OnJoin !(Maybe DksHash)
--  | JoinRetry !OnJoin !DksHash
    | LeaveOp !(Maybe OnLeave)
    | LookupOp !(OnResult Encoding) !DhtKey
    | InsertOp !(Maybe OnDone) !DhtKey !Encoding
    | ProcessMessageOp !(EVar DksMessage)
    | GetStateOp !(DksState -> IO ())

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

-- Implementation notes for DksOperation:
--
-- * Callback should always be the first argument of a constructor, because it
--   is most likely to be boud first and the rest will probably be left free.

-- }}} Operations -------------------------------------------------------------

-- {{{ Handle -----------------------------------------------------------------

data DksHandle = DksHandle
    { _options :: !DksParams
    , _self :: !DksHash
    , _mainThread :: !ThreadId
    , _sendOp :: !(DksOperation -> IO ())
    }

instance Show DksHandle where
    showsPrec _ DksHandle{_self = self} =
        showString "{implementation = DKS, self = " . shows self . showChar '}'

-- | Smart constructor for 'DksHandle'.
mkDksHandle
    :: DksParams
    -> DksHash
    -> InChan DksOperation
    -> ThreadId
    -> DksHandle
mkDksHandle opts hash inch mainTid = DksHandle
    { _options = opts
    , _self = hash
    , _mainThread = mainTid
    , _sendOp = writeChan inch
    }

newDksHandle :: DksMessageChannel chan => chan -> DksParams -> DksHash -> IO DksHandle
newDksHandle msgChan opts hash = do
    (inChan, outChan) <- newChan
    state <- mkBoxedThreadState opts
    let DksParams{_runThread = run, _logging = log, _yield = yield} = opts
    h <- mkDksHandle opts hash inChan
        <$> run (mainLoop log (sendMessage msgChan) yield outChan state)
    registerReceiveMessage msgChan hash (receiveDksMessage h)
    return h
  where
    -- TODO: Finalizer; exception handling.
    mainLoop log send yield outChan state = forever $ do
        readChan outChan >>= threadMain log send yield state hash

    receiveDksMessage :: DksHandle -> DksMessage -> EVarIO ()
    receiveDksMessage h = fmap success . _sendOp h . ProcessMessageOp . success

threadMain
    :: SomeLoggingBackend
    -> (DksMessage -> EVarIO ())
    -- ^ Send message to a network.
    -> IO ()
    -- ^ Yield \"processor\" time.
    -> BoxedThreadState
    -> DksHash
    -> DksOperation
    -> IO ()
threadMain log send' _yield state self op = withOp $ \case
    JoinOp onJoin possiblyEntryNode ->
        runM (onJoin . failure) $ case possiblyEntryNode of
            Nothing -> do
                stepDksState state EventSelfJoinDone $ \s -> s
                    { _lock = False
                    , _predecessor = Just self
                    , _successor = Just self
                    , _oldPredecessor = Nothing
                    }
                liftIO $ do
                    pushLogStrLn log $ format (shown % ": JoinOp: Self join done.") self
                    onJoin success_

            Just node -> do
                stepDksState state EventJoinRequest $ \s -> s
                    { _lock = True
                    , _predecessor = Nothing
                    , _successor = Nothing
                    , _oldPredecessor = Nothing
                    }
                liftIO . pushLogStrLn log $ format (shown % ": JoinOp: Sending join request to " % shown) self node
                send $ dksMessage
                    DksMessageHeader{_to = node, _from = self}
                    JoinRequest{JoinRequest._requester = self}

    LeaveOp _possiblyOnLeave ->
        pushLogStrLn log $ format (shown % ": LeaveOp: Not implemented.") self

    LookupOp _onResult _key ->
        pushLogStrLn log $ format (shown % ": LookupOp: Not implemented.") self

    InsertOp _onDone _key _encoding ->
        pushLogStrLn log $ format (shown % ": InsertOp: Not implemented.") self

    ProcessMessageOp (Right msg@DksMessage{_header = hdr})
      | _to hdr == self   -> processMessage (_from hdr) (_body msg)
      | otherwise         -> do
        pushLogStrLn log $ format (shown % ": Message not for us; resending it: " % shown) self msg
        send_ msg
    ProcessMessageOp _    -> return ()

    GetStateOp onState -> askThreadState state _dksState >>= onState
  where
    withOp f = do
        pushLogStrLn log $ format (shown % ": Operation: " % shown) self op
        f op

    send = ExceptT . send'
    send_ = void . send'

    processMessage from = \case
        JoinRequestBody msg@JoinRequest{JoinRequest._requester = rqstr} -> do
            pushLogStrLn log $ format (shown % ": Processing: " % shown) self msg
            dksState <- askThreadState state $ _dksState
            let DksState
                    { _joinForward = joinFwd
                    , _oldPredecessor = oldPred
                    , _predecessor = pred
                    , _successor = succ
                    , _leaveForward = leaveFwd
                    , _lock = lck
                    } = dksState

                joinRetry =  dksMessage
                    DksMessageHeader{_to = from, _from = self}
                    JoinRetry{JoinRetry._requester = rqstr}

                sendJoinRetry = do
                    pushLogStrLn log $ format (shown % ": Respond with retry join: " % shown % " to: " % shown) self joinRetry from
                    send_ joinRetry

                forwardJoinRequestTo n = do
                    pushLogStrLn log $ format (shown % ": Forwarding join request: " % shown % " to: " % shown) self msg n
                    send_ $ dksMessage DksMessageHeader{_to = n, _from = from} msg
            if
              | joinFwd && oldPred == Just rqstr, Just p <- oldPred ->
                forwardJoinRequestTo p

              -- Leaving in progress means that we can not accept join
              -- requests.
              --
              -- Node has to belong to the interval we are responsible for, if
              -- not then we can not accept the join request.
              | leaveFwd || responsibleFor rqstr pred ->
                maybe sendJoinRetry forwardJoinRequestTo succ

              | lck == True -> sendJoinRetry

              | Just p <- pred -> runM (\_ -> return ()) $ do
                stepDksState state EventProcessingJoinRequest $ \s -> s
                    { _lock = True
                    , _joinForward = True
                    , _oldPredecessor = _predecessor s
                    , _predecessor = Just rqstr
                    }
                send $ dksMessage
                    DksMessageHeader{_to = rqstr, _from = self}
                    JoinPoint
                        { JoinPoint._requester = rqstr
                        , JoinPoint._predecessor = p
                        , JoinPoint._successor = self
                        }

              | otherwise -> sendJoinRetry

--      JoinRetryBody msg@JoinRetry{JoinRetry._requester = rqstr}
--        | rqstr /= self -> ...
--        | otherwise     -> ...

        JoinPointBody msg@JoinPoint{JoinPoint._requester = rqstr}
          | rqstr /= self -> return ()  -- Message is not for us.
          | otherwise     -> runM (\_ -> return ()) $ do
            let pred = JoinPoint._predecessor msg
                succ = JoinPoint._successor msg
            stepDksState state EventJoinPoint $ \s -> s
                { _predecessor = Just pred
                , _successor = Just succ
                }
            send $ dksMessage
                DksMessageHeader{_to = pred, _from = self}
                NewSuccessor
                    { NewSuccessor._requester = self
                    , NewSuccessor._oldSuccessor = succ
                    , NewSuccessor._successor = self
                    }

        NewSuccessorBody msg@NewSuccessor{NewSuccessor._successor = newSucc} -> do
            askThreadState state (_successor . _dksState) >>= \case
                Nothing -> return () -- Error.
                Just oldSucc -> runM (\_ -> return ()) $ do
                    stepDksState state EventNewSuccessor $ \s -> s
                        { _successor = Just newSucc
--                      , _oldSuccessor = oldSucc
                        }
                    send $ dksMessage
                        DksMessageHeader{_to = oldSucc, _from = self}
                        NewSuccessorAck
                            { NewSuccessorAck._requester = NewSuccessor._requester msg
                            , NewSuccessorAck._oldSuccessor = oldSucc
                            , NewSuccessorAck._successor = newSucc
                            }

        NewSuccessorAckBody NewSuccessorAck{NewSuccessorAck._requester = rqstr} -> do
            askThreadState state (_oldPredecessor . _dksState) >>= \case
                Nothing -> return () -- Error.
                Just oldPred -> runM (\_ -> return ()) $ do
                    stepDksState state EventNewSuccessorAck $ \s -> s
                        { _lock = False
                        , _joinForward = False
                        }
                    send $ dksMessage
                        DksMessageHeader{_to = rqstr, _from = self}
                        JoinDone
                            { JoinDone._requester = rqstr
                            , JoinDone._successor = self
                            , JoinDone._predecessor = oldPred
                            }

        JoinDoneBody JoinDone{} -> runM (\_ -> return ()) $ do
            stepDksState state EventJoinDone $ \s -> s
                { _lock = False
                }

        _ -> return ()

    responsibleFor rqstr = maybe False $ \n ->
        let bounds = (Excluding self, Including n)
        -- isWholeSpace bounds = True <==> (Excluding self, Including self),
        -- i.e. we are the only node in the DHT and therefore responsible for
        -- any joining node.
        in not (isWholeSpace bounds) && Hash.inInterval bounds rqstr

--  runPossibleCallback :: r -> a -> Maybe (a -> IO ()) -> IO r
--  runPossibleCallback r a = maybe (return r) (\f -> r <$ f a)

stepDksState :: BoxedThreadState -> Event -> E DksState -> M ()
stepDksState BoxedThreadState{_modifyThreadState = modify} event f =
    ExceptT $ modify go <$$> \case
        Success _sig -> success_
        Failure sig -> failure $ DksStateTransitionFailed sig
  where
    go threadState@ThreadState{_dksState = dksState} =
        -- Setting DKS states after applying f ensures that it can not mess
        -- with the DKS state.
        (threadState{_dksState = newDksState}, sig)
      where
        (newDksState, sig) = Internal.stepDksState event f dksState

    (<$$>) = flip (<$>)

-- }}} Handle -----------------------------------------------------------------

-- {{{ DHT Operations ---------------------------------------------------------

dksJoin :: OnJoin -> DksHandle -> IO ()
dksJoin callback handle@DksHandle{_options = opts} =
    discoverEntryNode >>= \case
        Left e -> callback (failure e)
        Right entry -> _sendOp handle $ JoinOp callback entry
  where
    discoverEntryNode = if _singleton opts
        then return (Right Nothing)
        else _discovery opts

dksLeave :: Maybe OnLeave -> DksHandle -> IO ()
dksLeave callback handle = _sendOp handle (LeaveOp callback)

dksLookup :: OnResult Encoding -> DksHandle -> DhtKey -> IO ()
dksLookup callback handle = _sendOp handle . LookupOp callback

dksInsert :: Maybe OnDone -> DksHandle -> DhtKey -> Encoding -> IO ()
dksInsert callback handle = (_sendOp handle .) . InsertOp callback

dksGetState :: (DksState -> IO ()) -> DksHandle -> IO ()
dksGetState callback handle = _sendOp handle $ GetStateOp callback

-- }}} DHT Operations ---------------------------------------------------------
