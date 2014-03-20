{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module AlphaHeavy.QuickFIX
  ( QuickFIX
  , QuickFIXException
  , SessionID
  , SessionState(..)
  , createAcceptor
  , createInitiator
  , sendMessage
  , sendMessage'
  , decodeMessage

  -- * Session Management
  , sessionStates
  , AlphaHeavy.QuickFIX.sessionLogon
  , AlphaHeavy.QuickFIX.sessionLogout
  , AlphaHeavy.QuickFIX.sessionDisconnect
  ) where

import Control.Applicative
import Control.Concurrent (forkIO, forkOS)
import Control.Concurrent.STM
import Control.Exception (catch, finally, throwIO)
import Control.Monad (unless)
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Map (Map)
import qualified Data.Map as Map
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.Marshal (free)
import Foreign.Ptr (nullPtr)
import Foreign.StablePtr
import GHC.Generics

import Data.Conduit
import qualified Data.Conduit.List as Cl

import AlphaHeavy.QuickFIX.Foreign as Foreign
import AlphaHeavy.QuickFIX.GReceive
import AlphaHeavy.QuickFIX.GSend
import AlphaHeavy.QuickFIX.Types

type QuickFIX = ConduitApp

createAcceptor
  :: (Generic a, GSendMessage (Rep a), GRecvMessage (Rep a), MonadResource m)
  => FilePath
  -> String
  -> String
  -> m (QuickFIX, Producer m a, Consumer a m ())
createAcceptor = createQuickFIXEngine runAcceptor

createInitiator
  :: (Generic a, GSendMessage (Rep a), GRecvMessage (Rep a), MonadResource m)
  => FilePath
  -> String
  -> String
  -> m (QuickFIX, Producer m a, Consumer a m ())
createInitiator = createQuickFIXEngine runApplication

createQuickFIXEngine
  :: (Generic a, GSendMessage (Rep a), GRecvMessage (Rep a), MonadResource m)
  => (StablePtr ConduitApp -> CString -> IO CString)
  -> FilePath
  -> String
  -> String
  -> m (QuickFIX, Producer m a, Consumer a m ())
createQuickFIXEngine createFunction configPath sender target = do
  app@ConduitApp{..} <- liftIO . atomically $ do
    recv <- newEmptyTMVar
    halt <- newEmptyTMVar
    mgmt <- newTChan
    status <- newTVar EngineStopped
    sessions <- newTVar Map.empty
    return $ ConduitApp recv halt mgmt status sessions

  let initEngine = do
        ptr <- newStablePtr app
        _ <- forkOS $ do
          atomically $ writeTVar conduitAppStatus EngineRunning
          res <- withCString configPath (createFunction ptr)
          atomically $ writeTVar conduitAppStatus EngineStopped
          unless (res == nullPtr) $ do
            str <- peekCString res
            free res
            throwIO $! QuickFIXException str

        return ptr

      waitForShutdown = do
        -- signal shutdown
        ifRunning $ putTMVar conduitAppHalt ()

        -- wait for exit
        ifRunning retry

      haltEngine ptr =
        finally
          waitForShutdown
          (freeStablePtr ptr)

      ifRunning f =
        atomically $ do
          st <- readTVar conduitAppStatus
          case st of
            EngineStopped -> return ()
            EngineRunning -> f

  (releaseKey, _) <- allocate
    (initEngine >>= \ ptr -> putStrLn "engine init complete" >> return ptr)
    (\ ptr -> haltEngine ptr >> putStrLn "engine halted")

  _ <- liftIO . forkIO $ managementLoop conduitAppMgmt conduitAppStatus

  return (app, sourceQuickFIX app, sinkQuickFIX releaseKey conduitAppSessions sender target)

sourceQuickFIX
  :: (Generic a, GRecvMessage (Rep a), MonadIO m)
  => ConduitApp
  -> Producer m a
sourceQuickFIX ConduitApp{conduitAppRecv, conduitAppStatus} = step where
  step = do
    mval <- liftIO . atomically $ do
      mval <- Right <$> takeTMVar conduitAppRecv <|> Left <$> readTVar conduitAppStatus
      case mval of
        Left EngineRunning -> retry
        _ -> return mval

    case mval of
      Right (val, future) -> do
        -- the QuickFIX callbacks don't know the correct type, so we need to do
        -- the parsing with the GRecvMessage constraint in scope. The QuickFIX engine
        -- needs any parse related exceptions returned synchronously... this is
        -- done by passing a future for the exception along with the message pointer
        mmsg <- liftIO $ catch
          (receiveMessage val >>= \ !msg -> future Nothing >> return (Just msg))
          (\ ex -> future (Just ex) >> return Nothing)
        case mmsg of
          Just msg -> yield msg
          Nothing  -> return ()
        step

      -- this should always be Left EngineStopped
      Left _ -> return ()

sinkQuickFIX
  :: (Generic a, GSendMessage (Rep a), MonadResource m)
  => ReleaseKey
  -> TVar (Map k SessionState)
  -> String
  -> String
  -> Consumer a m ()
sinkQuickFIX releaseKey sessions sender target = do
  liftIO . atomically $ do
    sv <- readTVar sessions
    unless (or [True | SessionOpen <- Map.elems sv]) retry

  r <- Cl.mapM_ (liftIO . sendMessage sender target)
  lift $ release releaseKey
  return r

managementLoop
  :: TChan EngineManagement
  -> TVar EngineState
  -> IO ()
managementLoop mgmt state = do
  let block = do
        state' <- readTVar state
        case state' of
          EngineRunning -> retry
          EngineStopped -> return $ Left ()

  next <- atomically $ Right <$> readTChan mgmt <|> block

  case next of
    Left _    -> return ()
    Right cmd -> do
      res <- case cmd of
        SessionLogon sess ->
          Foreign.sessionLogon sess

        SessionLogout sess reason ->
          withCString reason $ \ c'reason ->
            Foreign.sessionLogout sess c'reason

        SessionDisconnect sess ->
          Foreign.sessionDisconnect sess

      throwIfNotNull res

      managementLoop mgmt state

sessionStates
  :: QuickFIX
  -> STM (Map SessionID SessionState)
sessionStates =
  readTVar . conduitAppSessions

sessionLogon
  :: QuickFIX
  -> SessionID
  -> STM ()
sessionLogon app =
  writeTChan (conduitAppMgmt app) . SessionLogon

sessionLogout
  :: QuickFIX
  -> SessionID
  -> String
  -> STM ()
sessionLogout app sess =
  writeTChan (conduitAppMgmt app) . SessionLogout sess

sessionDisconnect
  :: QuickFIX
  -> SessionID
  -> STM ()
sessionDisconnect app =
  writeTChan (conduitAppMgmt app) . SessionDisconnect
