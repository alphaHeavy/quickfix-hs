{-# LANGUAGE CPP, BangPatterns, MagicHash, CApiFFI, UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy #-}
-- | A module containing low-level hash primitives.
module Data.Hashable.LowLevel (
    Salt,
    defaultSalt,
    hashInt,
    hashInt64,
    hashWord64,
    hashPtrWithSalt,
    hashByteArrayWithSalt,
) where

#include "MachDeps.h"

import Data.Array.Byte (ByteArray (..))
import Foreign.Ptr (Ptr, castPtr)
import GHC.Base (ByteArray#)

#ifdef HASHABLE_RANDOM_SEED
import System.IO.Unsafe (unsafePerformIO)
#endif

import Data.Hashable.Imports
import Data.Hashable.Mix
import Data.Hashable.XXH3

-------------------------------------------------------------------------------
-- Initial seed
-------------------------------------------------------------------------------

#ifdef HASHABLE_RANDOM_SEED
initialSeed :: Word64
initialSeed = unsafePerformIO initialSeedC
{-# NOINLINE initialSeed #-}

foreign import capi "HsHashable.h hs_hashable_init" initialSeedC :: IO Word64
#endif

-- | A default salt used in the implementation of 'hash'.
defaultSalt :: Salt
#ifdef HASHABLE_RANDOM_SEED
defaultSalt = hashInt defaultSalt' (fromIntegral initialSeed)
#else
defaultSalt = defaultSalt'
#endif
{-# INLINE defaultSalt #-}

defaultSalt' :: Salt
#if WORD_SIZE_IN_BITS == 64
defaultSalt' = -3750763034362895579 -- 14695981039346656037 :: Int64
#else
defaultSalt' = -2128831035 -- 2166136261 :: Int32
#endif
{-# INLINE defaultSalt' #-}

-------------------------------------------------------------------------------
-- Hash primitives
-------------------------------------------------------------------------------

-- | Hash 'Int'. First argument is a salt, second argument is an 'Int'.
-- The result is new salt / hash value.
hashInt :: Salt -> Int -> Salt
hashInt !s !x = fromIntegral (mixHash (fromIntegral s) (fromIntegral x))

hashInt64  :: Salt -> Int64 -> Salt
hashWord64 :: Salt -> Word64 -> Salt

#if WORD_SIZE_IN_BITS == 64
hashInt64  !s !x = hashInt s (fromIntegral x)
hashWord64 !s !x = hashInt s (fromIntegral x)
#else
hashInt64  !s !x = hashInt (hashInt s (fromIntegral x)) (fromIntegral (x `unsafeShiftR` 32))
hashWord64 !s !x = hashInt (hashInt s (fromIntegral x)) (fromIntegral (x `unsafeShiftR` 32))
#endif

-- | Compute a hash value for the content of this pointer, using an
-- initial salt.
--
-- This function can for example be used to hash non-contiguous
-- segments of memory as if they were one contiguous segment, by using
-- the output of one hash as the salt for the next.
hashPtrWithSalt :: Ptr a   -- ^ pointer to the data to hash
                -> Int     -- ^ length, in bytes
                -> Salt    -- ^ salt
                -> IO Salt -- ^ hash value
hashPtrWithSalt ptr len salt =
    fromIntegral `fmap` xxh3_64bit_withSeed_ptr (castPtr ptr) len (fromIntegral salt)

-- | Compute a hash value for the content of this 'ByteArray#', using
-- an initial salt.
--
-- This function can for example be used to hash non-contiguous
-- segments of memory as if they were one contiguous segment, by using
-- the output of one hash as the salt for the next.
hashByteArrayWithSalt
    :: ByteArray#  -- ^ data to hash
    -> Int         -- ^ offset, in bytes
    -> Int         -- ^ length, in bytes
    -> Salt        -- ^ salt
    -> Salt        -- ^ hash value
hashByteArrayWithSalt ba !off !len !salt =
    fromIntegral (xxh3_64bit_withSeed_ba (ByteArray ba) off len (fromIntegral salt))
