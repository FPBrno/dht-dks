{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  All test cases aggregated and exported as tests :: [Test].
-- Copyright:    (c) 2015, Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    stable
-- Portability:  NoImplicitPrelude
--
-- All test cases aggregated and exported as @tests :: ['Test']@.
module TestCase (tests)
    where

import Test.Framework (Test, testGroup)

import qualified TestCase.Data.DHT.DKS as DKS (tests)
import qualified TestCase.Data.DHT.DKS.StateMachine as DKS.StateMachine (tests)


tests :: [Test]
tests =
    [ testGroup "Data.DHT.DKS.StateMachine" DKS.StateMachine.tests
    , testGroup "Data.DHT.DKS" DKS.tests
    ]
