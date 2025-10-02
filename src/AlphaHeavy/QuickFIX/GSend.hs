{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module AlphaHeavy.QuickFIX.GSend where

import Control.Monad (forM_)
import Data.Char (chr)
import Data.Proxy (Proxy(..))
import GHC.Generics
import GHC.TypeLits (KnownNat, natVal)

import AlphaHeavy.FIX as FIX
import AlphaHeavy.QuickFIX.Foreign
import AlphaHeavy.QuickFIX.Types
import AlphaHeavy.QuickFIX.SetMessageField

sendMessage
  :: (Generic a, GSendMessage (Rep a))
  => String
  -> String
  -> a
  -> IO ()
sendMessage sender target = gSendMessage sender target . from

sendMessage'
  :: forall a dir n . (Generic a, GSetMessageFields (Rep a), KnownNat n)
  => String
  -> String
  -> Message n dir a
  -> IO ()
sendMessage' senderCompID targetCompID (FIX.Message msg) = do
  let msgId = chr . fromIntegral $ natVal (Proxy :: Proxy n)
  sendMessageWithWrapper senderCompID targetCompID msgId $ \ h ->
    gSetMessageFields h (from msg)

-- |
-- Entry point, we're looking for 'Message' constructors with the message id
-- and sending direction encoded as type parameters
class GSendMessage (f :: * -> *) where
  gSendMessage :: String -> String -> f a -> IO ()

instance GSendMessage f => GSendMessage (M1 i c f) where
  gSendMessage sender target = gSendMessage sender target . unM1

instance (GSendMessage a, GSendMessage b) => GSendMessage (a :+: b) where
  gSendMessage sender target (L1 x) = gSendMessage sender target x
  gSendMessage sender target (R1 x) = gSendMessage sender target x

instance (GSendMessage a, GSendMessage b) => GSendMessage (a :*: b) where
  gSendMessage sender target (x :*: y) = do
    gSendMessage sender target x
    gSendMessage sender target y

instance (Generic a, GSetMessageFields (Rep a), KnownNat n) => GSendMessage (K1 c (Message n dir a)) where
  gSendMessage sender target = sendMessage' sender target . unK1

-- |
-- Message field iteration. Find each 'Field' within the message and its
-- associated field id
class GSetMessageFields (f :: * -> *) where
  gSetMessageFields :: QuickFIXMessagePtr -> f a -> IO ()

instance GSetMessageFields f => GSetMessageFields (M1 i c f) where
  gSetMessageFields msg = gSetMessageFields msg . unM1

instance (FieldTag a, KnownNat n, SetMessageField (FieldTagRep a)) => GSetMessageFields (K1 c (Enumeration n a)) where
  gSetMessageFields msg (K1 (Enumeration val)) = do
    let fieldId = fromIntegral $ natVal (Proxy :: Proxy n)
    setMessageField msg fieldId (toFieldTagRep val)

instance (FieldTag a, KnownNat n,  SetMessageField (FieldTagRep a)) => GSetMessageFields (K1 c (Maybe (Enumeration n a))) where
  gSetMessageFields msg (K1 (Just (Enumeration val))) = do
    let fieldId = fromIntegral $ natVal (Proxy :: Proxy n)
    setMessageField msg fieldId (toFieldTagRep val)

  gSetMessageFields _ (K1 Nothing) =
    return ()

instance (Generic a, GSetMessageField (Rep a), KnownNat n) => GSetMessageFields (K1 c (Maybe (Field n a))) where
  gSetMessageFields msg (K1 (Just (Field val))) = do
    let fieldId = fromIntegral $ natVal (Proxy :: Proxy n)
    gSetMessageField msg fieldId (from val)

  gSetMessageFields _ (K1 Nothing) =
    return ()

instance (Generic a, GSetMessageField (Rep a), KnownNat n) => GSetMessageFields (K1 c (Field n a)) where
  gSetMessageFields msg (K1 (Field val)) = do
    let fieldId = fromIntegral $ natVal (Proxy :: Proxy n)
    gSetMessageField msg fieldId (from val)

instance (Generic a, GSetMessageFields (Rep a), KnownNat n) => GSetMessageFields (K1 c (Group n a)) where
  gSetMessageFields msg (K1 (Group xs@(_:_))) = do
    let fieldId = natVal (Proxy :: Proxy n)
    putStrLn $ "TODO: properly implement groups: " ++ show fieldId
    forM_ xs $ gSetMessageFields msg . from

  gSetMessageFields _ (K1 (Group [])) =
    return ()

instance GSetMessageFields U1 where
  gSetMessageFields _ _ = return ()

instance (GSetMessageFields a, GSetMessageFields b) => GSetMessageFields (a :*: b) where
  gSetMessageFields msg (x :*: y) = do
    gSetMessageFields msg x
    gSetMessageFields msg y

instance (GSetMessageFields a, GSetMessageFields b) => GSetMessageFields (a :+: b) where
  gSetMessageFields msg (L1 x) = gSetMessageFields msg x
  gSetMessageFields msg (R1 x) = gSetMessageFields msg x

-- |
-- Now that we have the field id, set the value on the message
class GSetMessageField f where
  gSetMessageField :: QuickFIXMessagePtr -> Int -> f a -> IO ()

instance GSetMessageField f => GSetMessageField (M1 i c f) where
  gSetMessageField msg fid = gSetMessageField msg fid . unM1

instance (GSetMessageField a, GSetMessageField b) => GSetMessageField (a :*: b) where
  gSetMessageField msg fid (a :*: b) = do
    gSetMessageField msg fid a
    gSetMessageField msg fid b

instance (GSetMessageField a, GSetMessageField b) => GSetMessageField (a :+: b) where
  gSetMessageField msg fid (L1 x) = gSetMessageField msg fid x
  gSetMessageField msg fid (R1 x) = gSetMessageField msg fid x

instance SetMessageField a => GSetMessageField (K1 i a) where
  gSetMessageField msg fid =
    setMessageField msg fid . unK1

instance GSetMessageField U1 where
  gSetMessageField _ _ _ = return ()
