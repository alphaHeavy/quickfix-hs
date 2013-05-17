{-# LANGUAGE NamedFieldPuns #-}

module AlphaHeavy.QuickFIX.Foreign where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (bracket, throwIO)
import Control.Monad (unless)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import Data.Int
import Foreign.C.String
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr
import Foreign.StablePtr

import AlphaHeavy.FIX as FIX
import AlphaHeavy.QuickFIX.Types

{-
import System.IO.Unsafe (unsafePerformIO)

instance Show SessionID where
  show sess = unsafePerformIO $ do
    str <- sessionString sess
    if str == nullPtr
      then return ""
      else do
        str' <- peekCString str
        free str
        return str'
-}

throwIfNotNull :: CString -> IO ()
throwIfNotNull str =
  unless (str == nullPtr) $ do
    str' <- peekCString str
    free str
    throwIO $! QuickFIXException str'

sendMessageWithWrapper
  :: String
  -> String
  -> Char
  -> (QuickFIXMessagePtr -> IO ())
  -> IO ()
sendMessageWithWrapper senderCompID targetCompID msgType fun =
  let body ptr =
        withCString senderCompID $ \ senderCompStr ->
        withCString targetCompID $ \ targetCompStr -> do
          res <- AlphaHeavy.QuickFIX.Foreign.sendMessageWith senderCompStr targetCompStr msgType ptr
          throwIfNotNull res

  in bracket
       (mkMessageCallback fun)
       freeHaskellFunPtr
       body

decodeMessageWithWrapper
  :: B.ByteString
  -> (QuickFIXMessagePtr -> IO ())
  -> IO ()
decodeMessageWithWrapper msg fun =
  let body ptr =
        B.useAsCString msg $ \ msgStr -> do
          res <- AlphaHeavy.QuickFIX.Foreign.decodeMessageWith msgStr ptr
          throwIfNotNull res

  in bracket
       (mkMessageCallback fun)
       freeHaskellFunPtr
       body

setStringFieldWrapper
  :: QuickFIXMessagePtr
  -> Int32
  -> String
  -> IO ()
setStringFieldWrapper msg fieldId str =
  withCString str (AlphaHeavy.QuickFIX.Foreign.setStringField msg fieldId)

getStringFieldCPS
  :: QuickFIXMessagePtr
  -> Int32
  -> IO String
getStringFieldCPS msg fieldId = do
  mv <- newEmptyMVar
  let callback str = peekCAString str >>= putMVar mv
  bracket
    (mkStringCallback callback)
    freeHaskellFunPtr
    (AlphaHeavy.QuickFIX.Foreign.getStringField msg fieldId)
  takeMVar mv

foreign import ccall safe "QuickFIXThunks.h"
  runApplication :: StablePtr ConduitApp -> CString -> IO CString

foreign import ccall safe "QuickFIXThunks.h"
  runAcceptor :: StablePtr ConduitApp -> CString -> IO CString

foreign import ccall "wrapper"
  mkStringCallback :: (CString -> IO ()) -> IO (FunPtr (CString -> IO ()))

foreign import ccall "wrapper"
  mkMessageCallback :: (QuickFIXMessagePtr -> IO ()) -> IO (FunPtr (QuickFIXMessagePtr -> IO ()))

foreign import ccall "QuickFIXThunks.h"
  sendMessageWith :: CString -> CString -> Char -> FunPtr (QuickFIXMessagePtr -> IO ()) -> IO CString

foreign import ccall "QuickFIXThunks.h"
  decodeMessageWith :: CString -> FunPtr (QuickFIXMessagePtr -> IO ()) -> IO CString

foreign import ccall "QuickFIXThunks.h"
  setBoolField :: QuickFIXMessagePtr -> Int32 -> Bool -> IO ()

foreign import ccall "QuickFIXThunks.h"
  setCharField :: QuickFIXMessagePtr -> Int32 -> Char -> IO ()

foreign import ccall "QuickFIXThunks.h"
  setDoubleField :: QuickFIXMessagePtr -> Int32 -> Double -> IO ()

foreign import ccall "QuickFIXThunks.h"
  setIntField :: QuickFIXMessagePtr -> Int32 -> Int32 -> IO ()

foreign import ccall "QuickFIXThunks.h"
  setStringField :: QuickFIXMessagePtr -> Int32 -> CString -> IO ()

foreign import ccall "QuickFIXThunks.h"
  isFieldSet :: QuickFIXMessagePtr -> Int32 -> IO Bool

foreign import ccall "QuickFIXThunks.h"
  getBoolField :: QuickFIXMessagePtr -> Int32 -> IO Bool

foreign import ccall "QuickFIXThunks.h"
  getCharField :: QuickFIXMessagePtr -> Int32 -> IO Char

foreign import ccall "QuickFIXThunks.h"
  getDoubleField :: QuickFIXMessagePtr -> Int32 -> IO Double

foreign import ccall "QuickFIXThunks.h"
  getIntField :: QuickFIXMessagePtr -> Int32 -> IO Int32

foreign import ccall "QuickFIXThunks.h"
  getStringField :: QuickFIXMessagePtr -> Int32 -> FunPtr (CString -> IO ()) -> IO ()

foreign import ccall "QuickFIXThunks.h"
  getMessageType :: QuickFIXMessagePtr -> IO Char

foreign import ccall "QuickFIXThunks.h"
  sessionLogon :: SessionID -> IO CString

foreign import ccall "QuickFIXThunks.h"
  sessionLogout :: SessionID -> CString -> IO CString

foreign import ccall "QuickFIXThunks.h"
  sessionDisconnect :: SessionID -> IO CString

foreign import ccall "QuickFIXThunks.h"
  sessionString :: SessionID -> IO CString

foreign export ccall applicationBlock :: StablePtr ConduitApp -> IO ()
applicationBlock :: StablePtr ConduitApp -> IO ()
applicationBlock ptr = do
  ConduitApp{conduitAppHalt} <- deRefStablePtr ptr

  atomically $ takeTMVar conduitAppHalt

foreign export ccall applicationOnCreate :: StablePtr ConduitApp -> SessionID -> IO (Ptr FIXException)
applicationOnCreate :: StablePtr ConduitApp -> SessionID -> IO (Ptr FIXException)
applicationOnCreate ptr sess = do
  ConduitApp{conduitAppSessions} <- deRefStablePtr ptr

  atomically . modifyTVar' conduitAppSessions $ Map.insert sess SessionClosed

  return nullPtr

foreign export ccall applicationOnLogon :: StablePtr ConduitApp -> SessionID -> IO (Ptr FIXException)
applicationOnLogon :: StablePtr ConduitApp -> SessionID -> IO (Ptr FIXException)
applicationOnLogon ptr sess = do
  ConduitApp{conduitAppSessions} <- deRefStablePtr ptr

  atomically . modifyTVar' conduitAppSessions $ Map.insert sess SessionOpen

  return nullPtr

foreign export ccall applicationOnLogout :: StablePtr ConduitApp -> SessionID -> IO (Ptr FIXException)
applicationOnLogout :: StablePtr ConduitApp -> SessionID -> IO (Ptr FIXException)
applicationOnLogout ptr sess = do
  ConduitApp{conduitAppSessions} <- deRefStablePtr ptr

  atomically . modifyTVar' conduitAppSessions $ Map.insert sess SessionClosed

  return nullPtr

foreign export ccall applicationToAdmin :: StablePtr ConduitApp -> SessionID -> QuickFIXMessagePtr -> IO (Ptr FIXException)
applicationToAdmin :: StablePtr ConduitApp -> SessionID -> QuickFIXMessagePtr -> IO (Ptr FIXException)
applicationToAdmin _ptr _sess _msg = return nullPtr

foreign export ccall applicationToApp :: StablePtr ConduitApp -> SessionID -> QuickFIXMessagePtr -> IO (Ptr FIXException)
applicationToApp :: StablePtr ConduitApp -> SessionID -> QuickFIXMessagePtr -> IO (Ptr FIXException)
applicationToApp _ptr _sess _msg = return nullPtr

foreign export ccall applicationFromAdmin :: StablePtr ConduitApp -> SessionID -> QuickFIXMessagePtr -> IO (Ptr FIXException)
applicationFromAdmin :: StablePtr ConduitApp -> SessionID -> QuickFIXMessagePtr -> IO (Ptr FIXException)
applicationFromAdmin _ptr _sess _msg = return nullPtr

foreign export ccall applicationFromApp :: StablePtr ConduitApp -> SessionID -> QuickFIXMessagePtr -> IO (Ptr FIXException)
applicationFromApp :: StablePtr ConduitApp -> SessionID -> QuickFIXMessagePtr -> IO (Ptr FIXException)
applicationFromApp ptr _sess msg = do
  ConduitApp{conduitAppRecv} <- deRefStablePtr ptr

  mv <- newEmptyMVar

  atomically $ putTMVar conduitAppRecv (msg, putMVar mv)

  -- wait for 'sourceQuickFIX' to parse the message
  mval <- takeMVar mv
  case mval of
    Nothing  -> return nullPtr
    -- if it failed go ahead and let QuickFIX know
    Just val -> fail $ "Allocate exception pointer for: " ++ show val
