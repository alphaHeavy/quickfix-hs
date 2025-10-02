{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AlphaHeavy.QuickFIX.GReceive where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad -- (forM_)
import Control.Exception (SomeException, catch, throwIO)
import qualified Data.ByteString as B
import Data.Char (chr)
import Data.Proxy (Proxy (..))
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal)

import AlphaHeavy.FIX as FIX
import AlphaHeavy.QuickFIX.Foreign
import AlphaHeavy.QuickFIX.Types
import AlphaHeavy.QuickFIX.GetMessageField

receiveMessage
  :: (Generic a, GRecvMessage (Rep a))
  => QuickFIXMessagePtr
  -> IO a
receiveMessage ptr = do
  msgId <- getMessageType ptr
  mmsg <- fmap to <$> gRecvMessage ptr msgId
  case mmsg of
    Just msg -> return $! msg
    Nothing  -> throwIO . UnsupportedMessageType $ [msgId]

decodeMessage
  :: (Generic a, GRecvMessage (Rep a))
  => B.ByteString
  -> IO a
decodeMessage msgBS = do
  mv <- newEmptyMVar
  decodeMessageWithWrapper msgBS $ \ msgPtr -> do
    msg <- receiveMessage msgPtr
    putMVar mv msg
  mmsg <- tryTakeMVar mv
  case mmsg of
    Just msg -> return $! msg
    -- receiveMessage should throw an exception on failure
    Nothing  -> fail "decodeMessage/tryTakeMVar returned Nothing"

-- |
-- Entry point, we're looking for 'Message' constructors with the message id
-- and sending direction encoded as type parameters
class GRecvMessage (f :: * -> *) where
  gRecvMessage :: QuickFIXMessagePtr -> Char -> IO (Maybe (f a))

instance GRecvMessage f => GRecvMessage (M1 i c f) where
  gRecvMessage ptr msgId = fmap M1 <$> gRecvMessage ptr msgId

instance (GRecvMessage a, GRecvMessage b) => GRecvMessage (a :+: b) where
  gRecvMessage ptr msgId = do
    ma <- gRecvMessage ptr msgId
    case ma of
      Just a  -> return $! Just (L1 a)
      Nothing -> do
        mb <- gRecvMessage ptr msgId
        return $! case mb of
          Just b  -> Just (R1 b)
          Nothing -> Nothing

instance (GRecvMessage a, GRecvMessage b) => GRecvMessage (a :*: b) where
  gRecvMessage ptr msgId = do
    ma <- gRecvMessage ptr msgId
    mb <- gRecvMessage ptr msgId
    return $! case (ma, mb) of
      (Just a, Just b) -> Just (a :*: b)
      _ -> Nothing

instance (Generic a, GGetMessageFields (Rep a), KnownNat n) => GRecvMessage (K1 c (Message n dir a)) where
  gRecvMessage ptr msgId
    | msgId == msgId' =
        Just . K1 . Message . to <$> gGetMessageFields ptr
    | otherwise = return Nothing
    where msgId' = chr . fromIntegral $ natVal (Proxy :: Proxy n)

-- |
-- Message field iteration. Find each 'Field' within the message and its
-- associated field id
class GGetMessageFields (f :: * -> *) where
  gGetMessageFields :: QuickFIXMessagePtr -> IO (f a)

instance GGetMessageFields f => GGetMessageFields (M1 i c f) where
  gGetMessageFields msg = M1 <$> gGetMessageFields msg

instance (FieldTag a, GetMessageField (FieldTagRep a), KnownNat n, Show (FieldTagRep a)) => GGetMessageFields (K1 c (Maybe (Enumeration n a))) where
  gGetMessageFields msg = do
    let fieldId = fromIntegral $ natVal (Proxy :: Proxy n)
    isSet <- isFieldSet msg (fromIntegral fieldId)
    if isSet
      then do
        rawValue <- getMessageField msg fieldId
        case fromFieldTagRep rawValue of
          Just val -> return $! K1 . Just . Enumeration $ val
          Nothing  -> throwIO . IncorrectTagValue fieldId $ show rawValue
      else return $! K1 Nothing

instance (FieldTag a, GetMessageField (FieldTagRep a), KnownNat n, Show (FieldTagRep a)) => GGetMessageFields (K1 c (Enumeration n a)) where
  gGetMessageFields msg = do
    let fieldId = fromIntegral $ natVal (Proxy :: Proxy n)
    isSet <- isFieldSet msg (fromIntegral fieldId)
    unless isSet . throwIO . FieldNotFound fieldId $ "Missing required field: " ++ show fieldId
    rawValue <- getMessageField msg fieldId
    case fromFieldTagRep rawValue of
      Just val -> return $! K1 . Enumeration $ val
      Nothing  -> throwIO . IncorrectTagValue fieldId $ show rawValue

instance (Generic a, GGetMessageField (Rep a), KnownNat n) => GGetMessageFields (K1 c (Maybe (Field n a))) where
  gGetMessageFields msg = do
    let fieldId = fromIntegral $ natVal (Proxy :: Proxy n)
    isSet <- isFieldSet msg (fromIntegral fieldId)
    if isSet
      then K1 . Just . Field . to <$> gGetMessageField msg fieldId
      else return $! K1 Nothing

instance (Generic a, GGetMessageField (Rep a), KnownNat n) => GGetMessageFields (K1 c (Field n a)) where
  gGetMessageFields msg = do
    let fieldId = fromIntegral $ natVal (Proxy :: Proxy n)
    isSet <- isFieldSet msg (fromIntegral fieldId)
    unless isSet . throwIO . FieldNotFound fieldId $ "Missing required field: " ++ show fieldId
    K1 . Field . to <$> gGetMessageField msg fieldId

instance (Generic a, GGetMessageFields (Rep a), KnownNat n) => GGetMessageFields (K1 c (Group n a)) where
  gGetMessageFields _ = do
    let fieldId = natVal (Proxy :: Proxy n)
    putStrLn $ "TODO: properly implement groups: " ++ show fieldId
    -- forM_ xs $ gGetMessageFields msg . from
    return $! K1 (Group [])

instance GGetMessageFields U1 where
  gGetMessageFields _ = return U1

instance (GGetMessageFields a, GGetMessageFields b) => GGetMessageFields (a :*: b) where
  gGetMessageFields msg = do
    a <- gGetMessageFields msg
    b <- gGetMessageFields msg
    return $! (a :*: b)

instance (GGetMessageFields a, GGetMessageFields b) => GGetMessageFields (a :+: b) where
  gGetMessageFields msg =
    catch
      (L1 <$> gGetMessageFields msg)
      (\ (_ :: SomeException) -> R1 <$> gGetMessageFields msg)

-- |
-- Now that we have the field id, set the value on the message
class GGetMessageField f where
  gGetMessageField :: QuickFIXMessagePtr -> Int -> IO (f a)

instance GGetMessageField f => GGetMessageField (M1 i c f) where
  gGetMessageField msg fid =
    M1 <$> gGetMessageField msg fid

instance (GGetMessageField a, GGetMessageField b) => GGetMessageField (a :*: b) where
  gGetMessageField msg fid = do
    a <- gGetMessageField msg fid
    b <- gGetMessageField msg fid
    return $! (a :*: b)

instance (GGetMessageField a, GGetMessageField b) => GGetMessageField (a :+: b) where
  gGetMessageField msg fid =
    catch
      (L1 <$> gGetMessageField msg fid)
      (\ (_ :: SomeException) -> R1 <$> gGetMessageField msg fid)

instance GetMessageField a => GGetMessageField (K1 i a) where
  gGetMessageField msg fid =
    K1 <$> getMessageField msg fid

instance GGetMessageField U1 where
  gGetMessageField _ _ = return U1
