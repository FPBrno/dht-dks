{-# LANGUAGE NoImplicitPrelude #-}
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
module Data.DHT.DKS.Type.EVar
  where

import Control.Applicative (pure)
import Control.Exception (Exception(toException), SomeException)
import Data.Either (Either(Left, Right))
import Data.Function ((.))
import System.IO (IO)


type EVar a = Either SomeException a

type EVarIO a = IO (EVar a)

failure :: Exception e => e -> EVar a
failure = Left . toException

success :: a -> EVar a
success = Right

success_ :: EVar ()
success_ = success ()

successIO :: a -> EVarIO a
successIO = pure . success

successIO_ :: EVarIO ()
successIO_ = successIO ()
