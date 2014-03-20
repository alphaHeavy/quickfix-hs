{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module AlphaHeavy.FIX (
  Currency,
  DayOfMonth,
  Decimal,
  Enumeration(..),
  Exchange(..),
  FIXException(..),
  Field(..),
  FieldTag(..),
  Group(..),
  MarketLocalTime,
  Message(..),
  MessageDirection(..),
  MonthYear,
  Price(..),
  Quantity(..),
  AlphaHeavy.FIX.UTCDate,
  AlphaHeavy.FIX.UTCTime,
  UTCTimeStamp,
) where

import Control.Exception (Exception)
import Data.Decimal (Decimal)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import GHC.TypeLits (Nat)

newtype Enumeration (n :: Nat) a = Enumeration a
  deriving (Generic, Eq, Show)

newtype Field (n :: Nat) a = Field a
  deriving (Generic, Eq, Show)

newtype Group (n :: Nat) a = Group [a]
  deriving (Generic, Eq, Show)

newtype Message (n :: Nat) (dir :: MessageDirection) a = Message a
  deriving (Generic, Eq, Show)

newtype Price = Price Decimal
  deriving (Generic, Num, Ord, Eq, Fractional, Real, RealFrac, Read, Show)

newtype Quantity = Quantity Decimal
  deriving (Generic, Num, Ord, Eq, Fractional, Real, RealFrac, Read, Show)

class FieldTag a where
  type FieldTagRep a :: *
  toFieldTagRep :: a -> FieldTagRep a
  fromFieldTagRep :: FieldTagRep a -> Maybe a

data MessageDirection
  = RequestDirection
  | ResponseDirection
    deriving (Show)

data Exchange
  = Exchange_NASDAQ
  | Exchange_NYSE
  | Exchange_SMART
  | Exchange_OTHER String
  -- | Exchange_DirectEdge
  -- | Exchange_BATS
    deriving (Eq, Show)

type MonthYear = Int
type DayOfMonth = Int
type Currency = String
type MarketLocalTime = Int
type UTCTimeStamp = UTCTime
type UTCDate = Int
type UTCTime = Int

data FIXException
  = DoNotSend String
  | FieldConvertError String
  | FieldNotFound {-# UNPACK #-} !Int String
  | IncorrectDataFormat {-# UNPACK #-} !Int String
  | IncorrectTagValue {-# UNPACK #-} !Int String
  | RejectLogon String
  | RequiredTagMissing {-# UNPACK #-} !Int String
  | UnsupportedMessageType String
    deriving (Show, Typeable)

instance Exception FIXException
