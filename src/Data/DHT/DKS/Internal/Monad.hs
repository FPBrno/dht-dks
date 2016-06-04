{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
module Data.DHT.DKS.Internal.Monad
    (
    -- * Monad DksM
      DksM

    -- ** Evaluation
    , DksMonadEnv(..)
    , DksCallbacks(..)
    , runDksM

    -- ** Exception
    , DksException(..)

    -- ** Control Structures
    , throw
    , catch
    , handle
    , yield

    -- ** Logging
    , log
    , logf
    , hash

    -- ** Queries
    , getSelf

    -- ** Network
    , send
    , send_

    -- ** DksState
    , dksState
    , fromDksState
    , stepDksState

    -- ** Callbacks
    , registerOnJoinCallback
    , joinSuccess
    , joinFailure

    , registerOnLeaveCallback
    , leaveSuccess
    , leaveFailure

    , registerOnSuccessorChangeCallback
    , successorChanged
    , registerOnPredecessorChangeCallback
    , predecessorChanged

    -- * Internals

    -- ** BoxedThreadState
    , BoxedThreadState(..)
    , mkBoxedThreadState

    -- ** ThreadState
    , ThreadState(..)
    , mkThreadState
    )
  where

import Control.Monad (Monad((>>=)), return)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Exception (Exception(toException), SomeException)
import Data.Eq (Eq)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.), const, flip)
import Data.Functor ((<$>))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (Maybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show)

import Control.Monad.Except
    ( ExceptT--(ExceptT)
    , catchError
    , runExceptT
    , throwError
    )
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Data.Default.Class (Default(def))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (empty)
import Data.LogStr (LogStr)
import Data.LogStr.Formatting (Format, (%), runFormat, shown)
import Data.Monoid.Endo (E)

import Data.DHT.Type.Encoding (Encoding)

import Data.DHT.DKS.Type.EVar (EVar, EVarIO, failure, success_)
import Data.DHT.DKS.Type.Hash (DksHash)
import Data.DHT.DKS.Type.Message (DksMessage)
import Data.DHT.DKS.Type.Params (DksParams)
import Data.DHT.DKS.Type.State
    ( DksState
    , Event
    , Signal(Failure, Success)
    , SignalInfo
    )
import qualified Data.DHT.DKS.Type.State as Pure (stepDksState)


-- {{{ DksException -----------------------------------------------------------

-- TODO: Move this data type into its own module.

data DksException = DksStateTransitionFailed SignalInfo
  deriving (Eq, Show)

instance Exception DksException

-- }}} DksException -----------------------------------------------------------

-- {{{ ThreadState ------------------------------------------------------------

data DksCallbacks = DksCallbacks
    { _onJoin :: EVar () -> IO ()
    , _onLeave :: EVar () -> IO ()
    , _onSuccessorChange :: Maybe DksHash -> Maybe DksHash -> IO ()
    , _onPredecessorChange :: Maybe DksHash -> Maybe DksHash -> IO ()
    , _onLookupResult :: HashMap DksHash (EVar Encoding -> IO ())
    }

-- | State of a main thread of DKS node.
data ThreadState = ThreadState
    { _dksState :: DksState
    , _callbacks :: DksCallbacks
    }
  deriving (Generic, Typeable)

mkThreadState :: DksParams -> IO ThreadState
mkThreadState _opts = return ThreadState
    { _dksState = def
    , _callbacks = DksCallbacks
        { _onJoin = const $ return ()
        , _onLeave = const $ return ()
        , _onSuccessorChange = \_ _ -> return ()
        , _onPredecessorChange = \_ _ -> return ()
        , _onLookupResult = HashMap.empty
        }
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
  deriving Typeable

-- | Smart constructor for 'BoxedThreadState'.
mkBoxedThreadState :: DksParams -> IO BoxedThreadState
mkBoxedThreadState opts = mk <$> (mkThreadState opts >>= newIORef)
  where
    mk stateRef = BoxedThreadState
        { _getThreadState = readIORef stateRef
        , _modifyThreadState = atomicModifyIORef' stateRef
        }

-- }}} ThreadState ------------------------------------------------------------

-- | Mostly read-only state available to DKS message and event handlers.
data DksMonadEnv = DksMonadEnv
    { _self :: !DksHash
    , _logger :: !(LogStr -> IO ())
    , _send :: !(DksMessage -> EVarIO ())
    , _yield :: !(IO ())
    , _mutableState :: !BoxedThreadState
    }
  deriving (Generic, Typeable)

-- | Monad in which handlers of DKS messages and events live.
type DksM = ReaderT DksMonadEnv (ExceptT SomeException IO)

-- | Evaluate 'DksM'.
runDksM :: (SomeException -> IO a) -> DksMonadEnv -> DksM a -> IO a
runDksM handler r m = runExceptT (runReaderT m r) >>= \case
    Left e -> handler e
    Right a -> return a
{-# INLINE runDksM #-}

throw :: Exception e => e -> DksM a
throw = throwError . toException
{-# INLINE throw #-}

catch :: DksM a -> (SomeException -> DksM a) -> DksM a
catch = catchError
{-# INLINE catch #-}

handle :: (SomeException -> DksM a) -> DksM a -> DksM a
handle = flip catch
{-# INLINE handle #-}

liftEVar :: EVar a -> DksM a
liftEVar = \case
    Left e -> throw e
    Right a -> return a
{-# INLINE liftEVar #-}

liftEVarIO :: EVarIO a -> DksM a
liftEVarIO m = liftIO m >>= liftEVar
{-# INLINE liftEVarIO #-}

-- | Yield CPU time.
yield :: DksM ()
yield = asks _yield >>= liftIO
{-# INLINE yield #-}

-- | Retrieve hash of current DKS node.
getSelf :: DksM DksHash
getSelf = asks _self
{-# INLINE getSelf #-}

-- {{{ Logging ----------------------------------------------------------------

-- | Log message.
log :: LogStr -> DksM ()
log msg = asks _logger >>= \f -> liftIO $ f msg
{-# INLINE log #-}

-- | Variant of 'log' with type-safe @printf@-like formatting.
logf :: Format (DksM ()) a -> a
logf fmt = runFormat fmt log
{-# INLINE logf #-}

-- | Formatter for 'DksHash'.
hash :: Format r (DksHash -> r)
hash = shown
{-# INLINE hash #-}

-- }}} Logging ----------------------------------------------------------------

-- {{{ Network ----------------------------------------------------------------

-- | Send a DKS message to another node via network.
send :: DksMessage -> DksM ()
send msg = asks _send >>= \f -> liftIO (f msg) >>= \case
    Right () -> return ()
    Left e -> throw e
{-# INLINE send #-}

-- | Same as 'send', but exceptions are logged and then discarded. Useful in
-- cases when there is nothing we can do about the exception and crashing our
-- node doesn't make sense.
send_ :: DksMessage -> DksM ()
send_ msg = asks _send >>= \f -> liftIO (f msg) >>= \case
    Right () -> return ()
    Left e -> do
        self <- getSelf
        logf (hash % ": send_: Failed with an exception: " % shown) self e
{-# INLINE send_ #-}

-- }}} Network ----------------------------------------------------------------

-- {{{ DksState ---------------------------------------------------------------

-- | Get snapshot of current 'DksState'; use 'fromDksState' for querying only
-- part of 'DksState'.
dksState :: DksM DksState
dksState = do
    getThreadState <- asks $ _getThreadState . _mutableState
    _dksState <$> liftIO getThreadState
{-# INLINE dksState #-}

-- | Query 'DksState' using provided function. Example:
--
-- @
-- ... = do
--    -- >8 --
--    succ <- fromDksState _successor
--    -- >8 --
-- @
fromDksState :: (DksState -> a) -> DksM a
fromDksState f = f <$> dksState
{-# INLINE fromDksState #-}

-- | 'DksState' contains state DKS node including a small state machine. For
-- that reason it can be modified only using a (atomic) transactional model and
-- changes are commited only if state transition is successful.
stepDksState :: Event -> E DksState -> DksM ()
stepDksState event f = do
    modifyState <- asks (_modifyThreadState . _mutableState)
    liftEVarIO $ modifyState go <$$> \case
        Success _   -> success_
        Failure sig -> failure $ DksStateTransitionFailed sig
  where
    go threadState@ThreadState{_dksState = state} =
        -- Setting DKS _dksState after applying f ensures that it can not mess
        -- with it.
        (threadState{_dksState = newState}, sig)
      where
        (newState, sig) = Pure.stepDksState event f state

    (<$$>) = flip (<$>)
{-# INLINE stepDksState #-}

-- }}} DksState ---------------------------------------------------------------

-- {{{ Callbacks --------------------------------------------------------------

registerOnJoinCallback :: (EVar () -> IO ()) -> DksM ()
registerOnJoinCallback _ = return ()                -- TODO

joinSuccess :: Maybe DksHash -> Maybe DksHash -> DksM ()
joinSuccess _pred _succ = return ()                 -- TODO

joinFailure :: SomeException -> DksM ()
joinFailure _e = return ()                          -- TODO

registerOnLeaveCallback :: (EVar () -> IO ()) -> DksM ()
registerOnLeaveCallback _ = return ()               -- TODO

leaveSuccess :: DksM ()
leaveSuccess = return () -- TODO

leaveFailure :: SomeException -> DksM ()
leaveFailure _e = return ()                         -- TODO

registerOnSuccessorChangeCallback
    :: (Maybe DksHash -> Maybe DksHash -> IO ())
    -> DksM ()
registerOnSuccessorChangeCallback _ = return ()     -- TODO

successorChanged :: Maybe DksHash -> Maybe DksHash -> DksM ()
successorChanged _oldSucc _succ = return ()         -- TODO

registerOnPredecessorChangeCallback
    :: (Maybe DksHash -> Maybe DksHash -> IO ())
    -> DksM ()
registerOnPredecessorChangeCallback _ = return ()   -- TODO

predecessorChanged :: Maybe DksHash -> Maybe DksHash -> DksM ()
predecessorChanged _oldPred _pred = return ()       -- TODO

-- }}} Callbacks --------------------------------------------------------------
