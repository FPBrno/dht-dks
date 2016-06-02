{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
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
module Data.DHT.DKS.Type.Hash
    ( DksHash(..)
    , dksHash
    , dksHashText
    , dksHashKey

    -- * Unsafe
    , unsafeMkDksHash
    )
  where

import Prelude (Bounded(maxBound, minBound), Num((+), (-)))

import Data.Bool (Bool(False, True), otherwise)
import Data.Eq (Eq((==)))
import Data.Function (($), (.))
import Data.Ord (Ord(compare))
import Data.Tuple (snd)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show(showsPrec), showString)

import Crypto.Hash (Digest, SHA1, digestToHexByteString)
import qualified Crypto.Hash as Crypto (hash)
import Data.Byteable (toBytes)
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (mapAccumR, replicate)
import qualified Data.ByteString.Char8 as ByteString.Char8 (unpack)
import Data.DHT.Type.Hash (DhtHash(pred, succ))
import Data.DHT.Type.Key (DhtKey(DhtKey, DhtKeyText))
import Data.Hashable (Hashable)
import qualified Data.Hashable as Hashable (Hashable(hash, hashWithSalt))
import Data.Text (Text)
import qualified Data.Text.Encoding as Text (encodeUtf8)


data DksHash = DksHash !ByteString ByteString
  deriving (Generic, Typeable)

instance Bounded DksHash where
    minBound = unsafeMkDksHash $ ByteString.replicate 20 minBound
    maxBound = unsafeMkDksHash $ ByteString.replicate 20 maxBound

instance Eq DksHash where
    DksHash bs1 _ == DksHash bs2 _ = bs1 == bs2

instance Ord DksHash where
    DksHash bs1 _ `compare` DksHash bs2 _ = bs1 `compare` bs2

instance Show DksHash where
    showsPrec _ (DksHash _ hex) =
        showString "<<"
        . showString (ByteString.Char8.unpack hex)
        . showString ">>"

instance Hashable DksHash where
    hashWithSalt n (DksHash bs _) = Hashable.hashWithSalt n bs
    hash (DksHash bs _) = Hashable.hash bs

instance DhtHash DksHash where
    succ (DksHash bs _) =
        unsafeMkDksHash . snd $ ByteString.mapAccumR go True bs
      where
        go True  w
          | w == maxBound = (True, minBound)
          | otherwise     = (False, w + 1)
        go carry w = (carry, w)

    pred (DksHash bs _) =
        unsafeMkDksHash . snd $ ByteString.mapAccumR go True bs
      where
        go True  w
          | w == minBound = (True, maxBound)
          | otherwise     = (False, w - 1)
        go carry w = (carry, w)

unsafeMkDksHash :: ByteString -> DksHash
unsafeMkDksHash bs = DksHash bs (convertToBase Base16 bs)

dksHashKey :: DhtKey -> DksHash
dksHashKey = \case
    DhtKeyText t -> dksHashText t
    DhtKey bs -> dksHash bs

dksHashText :: Text -> DksHash
dksHashText = dksHash . Text.encodeUtf8

dksHash :: ByteString -> DksHash
dksHash bs = DksHash (toBytes h) (digestToHexByteString h)
  where
    h = Crypto.hash bs :: Digest SHA1
