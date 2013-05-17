{-# LANGUAGE DeriveDataTypeable #-}

module AlphaHeavy.QuickFIX.Types where

import Control.Exception (Exception)
import Control.Concurrent.STM
import Data.Map (Map)
import Data.Typeable (Typeable)
import Foreign.Ptr (Ptr)

import AlphaHeavy.FIX (FIXException)

data EngineState
  = EngineStopped
  | EngineRunning

data SessionState
  = SessionOpen
  | SessionClosed
    deriving Show

data EngineManagement
  = SessionLogon SessionID
  | SessionLogout SessionID String
  | SessionDisconnect SessionID

data ConduitApp = ConduitApp
  { conduitAppRecv     :: TMVar (QuickFIXMessagePtr, Maybe FIXException -> IO ())
  , conduitAppHalt     :: TMVar ()
  , conduitAppMgmt     :: TChan EngineManagement
  , conduitAppStatus   :: TVar EngineState
  , conduitAppSessions :: TVar (Map SessionID SessionState)
  }

data QuickFIXMessage
type QuickFIXMessagePtr = Ptr QuickFIXMessage

newtype SessionID = Ptr Int deriving (Eq, Ord)

data QuickFIXException = QuickFIXException String
  deriving (Show, Typeable)

instance Exception QuickFIXException
