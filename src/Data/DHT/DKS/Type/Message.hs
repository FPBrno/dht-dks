{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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
module Data.DHT.DKS.Type.Message
    ( DksMessageHeader(..)
    , DksMessageBody(..)
    , DksMessage(..)
    , IsDksMessage(..)
    )
  where


import Data.Eq (Eq)
import Data.Function ((.))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show)

import Data.Default.Class (Default(def))
import Data.OverloadedRecords.TH (overloadedRecord)

import Data.DHT.DKS.Type.Hash (DksHash)
import Data.DHT.DKS.Type.Message.GrantLeave (GrantLeave)
import Data.DHT.DKS.Type.Message.JoinDone (JoinDone)
import Data.DHT.DKS.Type.Message.JoinPoint (JoinPoint)
import Data.DHT.DKS.Type.Message.JoinRequest (JoinRequest)
import Data.DHT.DKS.Type.Message.JoinRetry (JoinRetry)
import Data.DHT.DKS.Type.Message.LeaveDone (LeaveDone)
import Data.DHT.DKS.Type.Message.LeavePoint (LeavePoint)
import Data.DHT.DKS.Type.Message.LeaveRequest (LeaveRequest)
import Data.DHT.DKS.Type.Message.LeaveRetry (LeaveRetry)
import Data.DHT.DKS.Type.Message.NewSuccessorAck (NewSuccessorAck)
import Data.DHT.DKS.Type.Message.NewSuccessor (NewSuccessor)
import Data.DHT.DKS.Type.Message.UpdateSuccessorAck (UpdateSuccessorAck)
import Data.DHT.DKS.Type.Message.UpdateSuccessor (UpdateSuccessor)


data DksMessageHeader = DksMessageHeader
    { _to :: !DksHash
    , _from :: !DksHash
--  , _originator :: !DksHash
    }
  deriving (Eq, Generic, Show, Typeable)

overloadedRecord def ''DksMessageHeader

data DksMessageBody
    -- {{{ Joining ------------------------------------------------------------
    = JoinRequestBody JoinRequest
    | JoinRetryBody JoinRetry
    | JoinPointBody JoinPoint
    | JoinDoneBody JoinDone
    | NewSuccessorBody NewSuccessor
    | NewSuccessorAckBody NewSuccessorAck
    -- }}} Joining ------------------------------------------------------------

    -- {{{ Leaving ------------------------------------------------------------
    | LeaveRequestBody LeaveRequest
    | LeaveRetryBody LeaveRetry
    | GrantLeaveBody GrantLeave
    | LeavePointBody LeavePoint
    | UpdateSuccessorBody UpdateSuccessor
    | UpdateSuccessorAckBody UpdateSuccessorAck
    | LeaveDoneBody LeaveDone
    -- }}} Leaving ----------------------------------------------------------------
  deriving (Eq, Generic, Show, Typeable)

-- UpdatePredecessorBody
-- StopForwardingBody

-- TODO: Timestamps, etc.
data DksMessage = DksMessage
    { _header :: DksMessageHeader
    , _body :: DksMessageBody
    }
  deriving (Eq, Generic, Show, Typeable)

overloadedRecord def ''DksMessage

class IsDksMessage a where
    dksMessage :: DksMessageHeader -> a -> DksMessage

-- {{{ Joining ----------------------------------------------------------------

instance IsDksMessage JoinRequest where
    dksMessage h = DksMessage h . JoinRequestBody

instance IsDksMessage JoinRetry where
    dksMessage h = DksMessage h . JoinRetryBody

instance IsDksMessage JoinPoint where
    dksMessage h = DksMessage h . JoinPointBody

instance IsDksMessage JoinDone where
    dksMessage h = DksMessage h . JoinDoneBody

instance IsDksMessage NewSuccessor where
    dksMessage h = DksMessage h . NewSuccessorBody

instance IsDksMessage NewSuccessorAck where
    dksMessage h = DksMessage h . NewSuccessorAckBody

-- }}} Joining ----------------------------------------------------------------

-- {{{ Leaving ----------------------------------------------------------------

instance IsDksMessage LeaveRequest where
    dksMessage h = DksMessage h . LeaveRequestBody

instance IsDksMessage LeaveRetry where
    dksMessage h = DksMessage h . LeaveRetryBody

instance IsDksMessage GrantLeave where
    dksMessage h = DksMessage h . GrantLeaveBody

instance IsDksMessage LeavePoint where
    dksMessage h = DksMessage h . LeavePointBody

instance IsDksMessage LeaveDone where
    dksMessage h = DksMessage h . LeaveDoneBody

instance IsDksMessage UpdateSuccessor where
    dksMessage h = DksMessage h . UpdateSuccessorBody

instance IsDksMessage UpdateSuccessorAck where
    dksMessage h = DksMessage h . UpdateSuccessorAckBody

-- }}} Leaving ----------------------------------------------------------------
