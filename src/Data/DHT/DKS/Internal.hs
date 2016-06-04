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
import Control.Exception (throwIO)
import Control.Monad (Monad((>>=)), (>>), forever, return)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (otherwise)
import Data.Either (Either(Left, Right))
import Data.Eq (Eq((==)))
import Data.Function (($), (.))
import Data.Functor (Functor(fmap), {-(<$), -}(<$>))
import Data.Maybe (Maybe(Nothing))
import System.IO (IO)
import Text.Show (Show(showsPrec), showChar, shows, showString)

import Control.Concurrent.Chan.Unagi
    ( InChan
    , newChan
    , readChan
    , writeChan
    )
import Data.DHT (DhtKey, Encoding)
--import Data.HashMap.Strict (HashMap)
import Data.LogStr.Formatting ((%), shown)
import System.Lumberjack.Backend (pushLogStrLn)

import Data.DHT.DKS.Internal.Monad
    ( DksM
    , DksMonadEnv(DksMonadEnv)
    , dksState
    , hash
    , logf
    , mkBoxedThreadState
--  , registerOnInsertDoneCallback
    , registerOnJoinCallback
--  , registerOnLeaveCallback
--  , registerOnLookupResultCallback
    , runDksM
    , send_
    )
import qualified Data.DHT.DKS.Internal.Monad as DksMonadEnv
    ( DksMonadEnv
        ( _logger
        , _mutableState
        , _self
        , _send
        , _yield
        )
    )
import Data.DHT.DKS.Internal.Operation
    ( DksOperation
        ( JoinOp
        , LeaveOp
        , LookupOp
        , InsertOp
        , ProcessMessageOp
        , GetStateOp
        )
    , OnDone
    , OnJoin
    , OnLeave
    , OnResult
    , handleJoin
    , handleJoinDone
    , handleJoinPoint
    , handleJoinRequest
    , handleJoinRetry
    , handleNewSuccessor
    , handleNewSuccessorAck
    )
import Data.DHT.DKS.Type.State (DksState)
import Data.DHT.DKS.Type.EVar
    ( EVarIO
    , failure
    , success
    )
import Data.DHT.DKS.Type.Hash (DksHash)
import Data.DHT.DKS.Type.Message
    ( DksMessage(DksMessage, _body, _header)
    , DksMessageBody
        ( JoinDoneBody
        , JoinPointBody
        , JoinRequestBody
        , JoinRetryBody
        , NewSuccessorAckBody
        , NewSuccessorBody
        )
    , DksMessageHeader(_from, _to)
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


-- {{{ Handle -----------------------------------------------------------------

data DksHandle = DksHandle
    { _options :: !DksParams
    , _self :: !DksHash
    , _mainThread :: !ThreadId
    , _sendOp :: !(DksOperation -> IO ())
    }

instance Show DksHandle where
    showsPrec _ DksHandle{_self = self} =
        showString "DhtHandle{implementation = DKS, self = " . shows self
        . showChar '}'

-- | Smart constructor for 'DksHandle'.
mkDksHandle
    :: DksParams
    -> DksHash
    -> InChan DksOperation
    -> ThreadId
    -> DksHandle
mkDksHandle opts self inch mainTid = DksHandle
    { _options = opts
    , _self = self
    , _mainThread = mainTid
    , _sendOp = writeChan inch
    }

newDksHandle :: DksMessageChannel chan => chan -> DksParams -> DksHash -> IO DksHandle
newDksHandle msgChan opts self = do
    (inChan, outChan) <- newChan
    env <- mkDksMonadEnv <$> mkBoxedThreadState opts
    let DksParams{_runThread = run} = opts
    h <- mkDksHandle opts self inChan <$> run (mainLoop outChan env)
    registerReceiveMessage msgChan self (receiveDksMessage h)
    return h
  where
    -- TODO: Finalizer; exception handling.
    mainLoop outChan env = forever $ do
        readChan outChan >>= runHandler env (threadMain self)

    -- If exception makes its way up here, then it should crash
    -- the whole node.
    runHandler env f op = runDksM throwIO env $ do
        logf (hash % ": Processig DKS operation: " % shown) self op
        f op

    receiveDksMessage :: DksHandle -> DksMessage -> EVarIO ()
    receiveDksMessage h = fmap success . _sendOp h . ProcessMessageOp . success

    mkDksMonadEnv s = DksMonadEnv
        { DksMonadEnv._self = self
        , DksMonadEnv._logger = pushLogStrLn (_logging opts)
        , DksMonadEnv._send = sendMessage msgChan
        , DksMonadEnv._yield = _yield opts
        , DksMonadEnv._mutableState = s
        }

threadMain :: DksHash -> DksOperation -> DksM ()
threadMain self = \case
    JoinOp onJoin possiblyEntryNode ->
        registerOnJoinCallback onJoin >> handleJoin possiblyEntryNode

    LeaveOp _possiblyOnLeave ->
--      registerOnLeaveCallback onLeave >> handleLeave
        logf (hash % ": LeaveOp: Not implemented.") self

    LookupOp _onResult _key ->
--      registerOnLookupResultCallback key onResult >> handleLookup key
        logf (hash % ": LookupOp: Not implemented.") self

    InsertOp _onDone _key _encoding ->
--      registerOnInsertDoneCallback key onDone >> handleInsert key encoding
        logf (hash % ": InsertOp: Not implemented.") self

    ProcessMessageOp (Right msg@DksMessage{_header = hdr})
      | _to hdr == self   -> do
        logf (hash % ": Processing received message: " % shown) self msg
        processMessage (_from hdr) (_body msg)
      | otherwise         -> do
        logf (hash % ": Message not for us; resending it: " % shown) self msg
        send_ msg
    ProcessMessageOp _    -> return ()

    GetStateOp onState -> dksState >>= liftIO . onState
  where
    processMessage from = \case
        JoinRequestBody msg -> handleJoinRequest from msg
        JoinRetryBody msg -> handleJoinRetry msg
        JoinPointBody msg -> handleJoinPoint msg
        NewSuccessorBody msg -> handleNewSuccessor msg
        NewSuccessorAckBody msg -> handleNewSuccessorAck msg
        JoinDoneBody msg -> handleJoinDone msg

        _ -> return ()

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
