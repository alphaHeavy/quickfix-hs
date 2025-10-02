{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AlphaHeavy.QuickFIX.GetMessageField where

import Control.Applicative
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Int
import Data.Time
import GHC.Generics

import AlphaHeavy.FIX as FIX
import AlphaHeavy.QuickFIX.Foreign
import AlphaHeavy.QuickFIX.Types

class GetMessageField a where
  getMessageField :: QuickFIXMessagePtr -> Int -> IO a

instance GetMessageField Bool where
  getMessageField msg fid =
    getBoolField msg (fromIntegral fid)

instance GetMessageField Char where
  getMessageField msg fid =
    getCharField msg (fromIntegral fid)

instance GetMessageField Int where
  getMessageField msg fid =
    fromIntegral <$> getIntField msg (fromIntegral fid)

instance GetMessageField Int32 where
  getMessageField msg fid =
    getIntField msg (fromIntegral fid)

instance GetMessageField Int64 where
  getMessageField msg fid =
    fromIntegral <$> getIntField msg (fromIntegral fid)

instance GetMessageField Float where
  getMessageField msg fid =
    realToFrac <$> getDoubleField msg (fromIntegral fid)

instance GetMessageField Double where
  getMessageField msg fid =
    getDoubleField msg (fromIntegral fid)

instance GetMessageField String where
  getMessageField msg fid =
    getStringFieldCPS msg (fromIntegral fid)

instance GetMessageField ByteString where
  getMessageField = error "no bytestring support yet"

instance GetMessageField Data.Time.UTCTime where
  getMessageField msg fid = do
    txt <- getMessageField msg fid
    case parseTimeM True defaultTimeLocale "%Y%m%d-%H:%M:%S" txt of
      Just val -> return $! val
      Nothing  -> throwIO . IncorrectTagValue fid $ txt

instance GetMessageField Decimal where
  getMessageField msg fid =
    read <$> getMessageField msg fid

instance GetMessageField Exchange where
  getMessageField msg fid = do
    val <- getMessageField msg fid
    return $! case val of
      "O"     -> Exchange_NASDAQ
      "N"     -> Exchange_NYSE
      "SMART" -> Exchange_SMART
      _       -> Exchange_OTHER val

instance GetMessageField (U1 x) where
  getMessageField _ _ =
    return U1
