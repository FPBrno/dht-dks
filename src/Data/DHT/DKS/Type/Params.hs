{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2015, Jan Šipr, Matej Kollár, Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- TODO
module Data.DHT.DKS.Type.Params
  where

import Data.Bool (Bool(True))
import Data.Functor (Functor, (<$>))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show)

import Data.Default.Class (Default(def))


data DksParams = DksParams
    { _singleton :: Bool
    }
  deriving (Generic, Show, Typeable)

singleton :: Functor f => (Bool -> f Bool) -> DksParams -> f DksParams
singleton f params@DksParams{_singleton} =
    (\b -> params{_singleton = b}) <$> f _singleton

-- |
-- @
-- 'def' = 'DksParams'
--     { '_singleton' = True
--     }
-- @
instance Default DksParams where
    def = DksParams True
