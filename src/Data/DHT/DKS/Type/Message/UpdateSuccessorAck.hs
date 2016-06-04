{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
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
module Data.DHT.DKS.Type.Message.UpdateSuccessorAck
    ( UpdateSuccessorAck(..)
    )
  where

import Data.Eq (Eq)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show)

import Data.Default.Class (Default(def))
import Data.OverloadedRecords.TH (overloadedRecord)

import Data.DHT.DKS.Type.Hash (DksHash)


data UpdateSuccessorAck = UpdateSuccessorAck
    { _requester :: !DksHash
    , _oldSuccessor :: !DksHash
    , _successor :: !DksHash
    }
  deriving (Eq, Generic, Show, Typeable)

overloadedRecord def ''UpdateSuccessorAck
