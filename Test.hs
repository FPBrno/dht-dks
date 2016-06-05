{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
module Test
  where

import Control.Concurrent (ThreadId, forkIO{-, threadDelay-})
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newTChanIO, readTChan)
import Control.Exception (finally)
import Control.Monad ((>>=), forever, return)
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.String (IsString(fromString))
import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show (Show(show))

import Data.Default.Class (Default(def))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
    ( empty
    , insert
    , lookup
    )
import Data.LogStr.Formatting ((%), format, shown)
import System.Lumberjack.Backend
    ( SomeLoggingBackend(SomeLoggingBackend)
    , pushLogStrLn
    )
import System.Lumberjack.FastLogger (fastLogger)

import Data.DHT.DKS.Internal
    ( DksHandle
    , newDksHandle
    )
import Data.DHT.DKS.Type.EVar (EVarIO, success)
import Data.DHT.DKS.Type.Hash (DksHash, dksHash)
import Data.DHT.DKS.Type.Message
    ( DksMessage(DksMessage, _header)
    , DksMessageHeader(DksMessageHeader, _to)
    )
import Data.DHT.DKS.Type.MessageChannel (StmMessageChannel(StmMessageChannel))
import Data.DHT.DKS.Type.Params (DksParams(_discovery, _logging))


node1, node2, node3 :: DksHash
node1 = dksHash "1464808099"
node2 = dksHash "1464808107"
node3 = dksHash "1464893893"

data S = S
    { _network :: HashMap DksHash (DksMessage -> EVarIO ())
    , _networkBus :: TChan DksMessage
    , _logger :: SomeLoggingBackend
    , _knownNode :: Maybe DksHash
    }

state :: IORef S
state = unsafePerformIO $ newIORef S
    { _network = HashMap.empty
    , _networkBus = unsafePerformIO newTChanIO
    , _logger = SomeLoggingBackend . unsafePerformIO $ fastLogger def
    , _knownNode = Nothing
    }

startNetworkMain :: IO ThreadId
startNetworkMain = do
    l <- getLogging
    forkIO $ networkMain l
        `finally` pushLogStrLn l "networkMain: Thread terminated unexpectedly."

networkMain :: SomeLoggingBackend -> IO ()
networkMain l = do
    bus <- readIORef state >>= atomically . dupTChan . _networkBus
    forever $ do
        pushLogStrLn l "networkMain: Iteration start."
        msg@DksMessage{_header = DksMessageHeader{_to = toNode}}
            <- atomically $ readTChan bus
        pushLogStrLn l $ format ("networkMain: Processing: " % shown) msg
        net <- _network <$> readIORef state
        case HashMap.lookup toNode net of
            Nothing -> pushLogStrLn l
                $ format ("networkMain: Unknown \"to\" node: " % shown) toNode
            Just send -> do
                pushLogStrLn l $ format
                    ("networkMain: Passing message to node: " % shown) toNode
                send msg >>= handleSendFailure toNode
  where
    handleSendFailure toNode = \case
        Right () -> return ()
        Left e -> do
            pushLogStrLn l $ format
                ( "networkMain: Encountered failure when passing message to "
                % " node: " % shown % ": " % shown) toNode e

discoverNode :: DksHash -> EVarIO (Maybe DksHash)
discoverNode node =
    atomicModifyIORef' state $ \s ->
        let possibleNode = _knownNode s
        in  ( s{_knownNode = Just $ fromMaybe node possibleNode}
            , success possibleNode
            )

setLogging :: IO ()
setLogging = do
    l <- fastLogger def
    atomicModifyIORef' state $ \s -> (s{_logger = SomeLoggingBackend l}, ())

getLogging :: IO SomeLoggingBackend
getLogging = _logger <$> readIORef state

insertChan :: DksHash -> (DksMessage -> EVarIO ()) -> IO ()
insertChan node send =
    atomicModifyIORef' state $ \s -> (, ()) s
        { _network = HashMap.insert node send (_network s)
        }

newDksChan :: IO StmMessageChannel
newDksChan = StmMessageChannel insertChan . _networkBus <$> readIORef state

-- |
--
-- > stack ghci
-- > ghci> :l Test.hs
-- > ghci> import Data.DHT.DKS.Internal
-- > ghci> startNetworkMain
-- > ghci> --
-- > ghci> h1 <- test node1
-- > ghci> dksJoin printLog h1
-- > ghci> --
-- > ghci> h2 <- test node2
-- > ghci> dksJoin printLog h2
-- > ghci> --
-- > ghci> h3 <- test node3
-- > ghci> dksJoin printLog h3
test :: DksHash -> IO DksHandle
test node = do
    l <- getLogging
    dksChan <- newDksChan
    let opts = def
            { _logging = l
--          , _yield = threadDelay 10000000
            , _discovery = discoverNode node
            }
    newDksHandle dksChan opts node

printLog :: Show a => a -> IO ()
printLog a = do
    l <- _logger <$> readIORef state
    pushLogStrLn l . fromString $ show a
