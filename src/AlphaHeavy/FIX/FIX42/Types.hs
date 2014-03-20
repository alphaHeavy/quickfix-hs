{-# LANGUAGE DeriveGeneric, DataKinds, GeneralizedNewtypeDeriving,
  TypeFamilies, TypeOperators #-}
module AlphaHeavy.FIX.FIX42.Types where
import qualified Data.ByteString
import qualified Control.Lens
import GHC.Generics
import AlphaHeavy.FIX
 
data Logon = Logon (AlphaHeavy.FIX.Enumeration 98 EncryptMethod)
                   (AlphaHeavy.FIX.Field 108 HeartBtInt)
                   (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 141 ResetSeqNumFlag))
           deriving (Generic, Show, Eq)
 
instance EncryptMethodLens Logon where
        encryptMethod = Control.Lens.lens g s
          where g (Logon (AlphaHeavy.FIX.Enumeration x) _ _) = x
                s (Logon _ _x2 _x3) val
                  = Logon (AlphaHeavy.FIX.Enumeration val) _x2 _x3
 
instance HeartBtIntLens Logon where
        heartBtInt = Control.Lens.lens g s
          where g (Logon _ (AlphaHeavy.FIX.Field x) _) = x
                s (Logon _x1 _ _x3) val = Logon _x1 (AlphaHeavy.FIX.Field val) _x3
 
instance ResetSeqNumFlagMaybeLens Logon where
        optResetSeqNumFlag = Control.Lens.lens g s
          where g (Logon _ _ (Just (AlphaHeavy.FIX.Enumeration x))) = Just x
                g _ = Nothing
                s (Logon _x1 _x2 _) val
                  = Logon _x1 _x2 (fmap AlphaHeavy.FIX.Enumeration val)
 
data Heartbeat = Heartbeat (Prelude.Maybe
                              (AlphaHeavy.FIX.Field 112 TestReqID))
               deriving (Generic, Show, Eq)
 
instance TestReqIDMaybeLens Heartbeat where
        optTestReqID = Control.Lens.lens g s
          where g (Heartbeat (Just (AlphaHeavy.FIX.Field x))) = Just x
                g _ = Nothing
                s (Heartbeat _) val = Heartbeat (fmap AlphaHeavy.FIX.Field val)
 
data TestRequest = TestRequest (AlphaHeavy.FIX.Field 112 TestReqID)
                 deriving (Generic, Show, Eq)
 
instance TestReqIDLens TestRequest where
        testReqID = Control.Lens.lens g s
          where g (TestRequest (AlphaHeavy.FIX.Field x)) = x
                s (TestRequest _) val = TestRequest (AlphaHeavy.FIX.Field val)
 
data ResendRequest = ResendRequest (AlphaHeavy.FIX.Field 7
                                      BeginSeqNo)
                                   (AlphaHeavy.FIX.Field 16 EndSeqNo)
                   deriving (Generic, Show, Eq)
 
instance BeginSeqNoLens ResendRequest where
        beginSeqNo = Control.Lens.lens g s
          where g (ResendRequest (AlphaHeavy.FIX.Field x) _) = x
                s (ResendRequest _ _x2) val
                  = ResendRequest (AlphaHeavy.FIX.Field val) _x2
 
instance EndSeqNoLens ResendRequest where
        endSeqNo = Control.Lens.lens g s
          where g (ResendRequest _ (AlphaHeavy.FIX.Field x)) = x
                s (ResendRequest _x1 _) val
                  = ResendRequest _x1 (AlphaHeavy.FIX.Field val)
 
data SequenceReset = SequenceReset (AlphaHeavy.FIX.Field 36
                                      NewSeqNo)
                                   (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 123 GapFillFlag))
                   deriving (Generic, Show, Eq)
 
instance NewSeqNoLens SequenceReset where
        newSeqNo = Control.Lens.lens g s
          where g (SequenceReset (AlphaHeavy.FIX.Field x) _) = x
                s (SequenceReset _ _x2) val
                  = SequenceReset (AlphaHeavy.FIX.Field val) _x2
 
instance GapFillFlagMaybeLens SequenceReset where
        optGapFillFlag = Control.Lens.lens g s
          where g (SequenceReset _ (Just (AlphaHeavy.FIX.Enumeration x)))
                  = Just x
                g _ = Nothing
                s (SequenceReset _x1 _) val
                  = SequenceReset _x1 (fmap AlphaHeavy.FIX.Enumeration val)
 
data Reject = Reject (AlphaHeavy.FIX.Field 45 RefSeqNum)
                     (Prelude.Maybe (AlphaHeavy.FIX.Field 58 Text))
            deriving (Generic, Show, Eq)
 
instance RefSeqNumLens Reject where
        refSeqNum = Control.Lens.lens g s
          where g (Reject (AlphaHeavy.FIX.Field x) _) = x
                s (Reject _ _x2) val = Reject (AlphaHeavy.FIX.Field val) _x2
 
instance TextMaybeLens Reject where
        optText = Control.Lens.lens g s
          where g (Reject _ (Just (AlphaHeavy.FIX.Field x))) = Just x
                g _ = Nothing
                s (Reject _x1 _) val = Reject _x1 (fmap AlphaHeavy.FIX.Field val)
 
data Logout = Logout (Prelude.Maybe (AlphaHeavy.FIX.Field 58 Text))
            deriving (Generic, Show, Eq)
 
instance TextMaybeLens Logout where
        optText = Control.Lens.lens g s
          where g (Logout (Just (AlphaHeavy.FIX.Field x))) = Just x
                g _ = Nothing
                s (Logout _) val = Logout (fmap AlphaHeavy.FIX.Field val)
 
data NewOrderSingle = NewOrderSingle (AlphaHeavy.FIX.Field 11
                                        ClOrdID)
                                     (AlphaHeavy.FIX.Enumeration 204 CustomerOrFirm)
                                     (AlphaHeavy.FIX.Field 100 ExDestination)
                                     (AlphaHeavy.FIX.Enumeration 21 HandlInst)
                                     (AlphaHeavy.FIX.Enumeration 114 LocateReqd)
                                     (AlphaHeavy.FIX.Field 5700 MPID)
                                     (AlphaHeavy.FIX.Enumeration 40 OrdType)
                                     (AlphaHeavy.FIX.Field 38 Quantity)
                                     (AlphaHeavy.FIX.Enumeration 54 Side)
                                     (AlphaHeavy.FIX.Field 55 Symbol)
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 1 Account))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 440 ClearingAccount))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 439 ClearingFirm))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 231 ContractMultiplier))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 15 Currency))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 388 DiscretionInst))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 389 DiscretionOffset))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 168 EffectiveTime))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 18 ExecInst))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 432 ExpireDate))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 126 ExpireTime))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 22 IDSource))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 205 MaturityDay))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 200 MaturityMonthYear))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 111 Quantity))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 77 OpenClose))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 211 PegDifference))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 44 Price))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 201 PutOrCall))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 207 SecurityExchange))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 48 SecurityID))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 167 SecurityType))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 99 Price))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 202 Price))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Field 58 Text))
                                     (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 59 TimeInForce))
                    deriving (Generic, Show, Eq)
 
instance ClOrdIDLens NewOrderSingle where
        clOrdID = Control.Lens.lens g s
          where g (NewOrderSingle (AlphaHeavy.FIX.Field x) _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (NewOrderSingle _ _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle (AlphaHeavy.FIX.Field val) _x2 _x3 _x4 _x5 _x6 _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance CustomerOrFirmLens NewOrderSingle where
        customerOrFirm = Control.Lens.lens g s
          where g (NewOrderSingle _ (AlphaHeavy.FIX.Enumeration x) _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (NewOrderSingle _x1 _ _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 (AlphaHeavy.FIX.Enumeration val) _x3 _x4 _x5
                      _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance ExDestinationLens NewOrderSingle where
        exDestination = Control.Lens.lens g s
          where g (NewOrderSingle _ _ (AlphaHeavy.FIX.Field x) _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (NewOrderSingle _x1 _x2 _ _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 (AlphaHeavy.FIX.Field val) _x4 _x5 _x6 _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance HandlInstLens NewOrderSingle where
        handlInst = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ (AlphaHeavy.FIX.Enumeration x) _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (NewOrderSingle _x1 _x2 _x3 _ _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 (AlphaHeavy.FIX.Enumeration val) _x5
                      _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance LocateReqdLens NewOrderSingle where
        locateReqd = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ (AlphaHeavy.FIX.Enumeration x) _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (NewOrderSingle _x1 _x2 _x3 _x4 _ _x6 _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 (AlphaHeavy.FIX.Enumeration val)
                      _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance MPIDLens NewOrderSingle where
        mPID = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ (AlphaHeavy.FIX.Field x) _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _ _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 (AlphaHeavy.FIX.Field val) _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance OrdTypeLens NewOrderSingle where
        ordType = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ (AlphaHeavy.FIX.Enumeration x)
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _ _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6
                      (AlphaHeavy.FIX.Enumeration val)
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance OrderQtyLens NewOrderSingle where
        orderQty = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ (AlphaHeavy.FIX.Field x) _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _ _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7
                      (AlphaHeavy.FIX.Field val)
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance SideLens NewOrderSingle where
        side = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _
                     (AlphaHeavy.FIX.Enumeration x) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _)
                  = x
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _ _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8
                      (AlphaHeavy.FIX.Enumeration val)
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance SymbolLens NewOrderSingle where
        symbol = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ (AlphaHeavy.FIX.Field x)
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _ _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      (AlphaHeavy.FIX.Field val)
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance AccountMaybeLens NewOrderSingle where
        optAccount = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _ _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10
                      (fmap AlphaHeavy.FIX.Field val)
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance ClearingAccountMaybeLens NewOrderSingle where
        optClearingAccount = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      (fmap AlphaHeavy.FIX.Field val)
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance ClearingFirmMaybeLens NewOrderSingle where
        optClearingFirm = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _ _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      (fmap AlphaHeavy.FIX.Field val)
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance ContractMultiplierMaybeLens NewOrderSingle where
        optContractMultiplier = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _ _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      (fmap AlphaHeavy.FIX.Field val)
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance CurrencyMaybeLens NewOrderSingle where
        optCurrency = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _ _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      (fmap AlphaHeavy.FIX.Field val)
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance DiscretionInstMaybeLens NewOrderSingle where
        optDiscretionInst = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _ _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance DiscretionOffsetMaybeLens NewOrderSingle where
        optDiscretionOffset = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _ _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      (fmap AlphaHeavy.FIX.Field val)
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance EffectiveTimeMaybeLens NewOrderSingle where
        optEffectiveTime = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _ _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      (fmap AlphaHeavy.FIX.Field val)
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance ExecInstMaybeLens NewOrderSingle where
        optExecInst = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _ _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance ExpireDateMaybeLens NewOrderSingle where
        optExpireDate = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _ _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      (fmap AlphaHeavy.FIX.Field val)
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance ExpireTimeMaybeLens NewOrderSingle where
        optExpireTime = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _ _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      (fmap AlphaHeavy.FIX.Field val)
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance IDSourceMaybeLens NewOrderSingle where
        optIDSource = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _ _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance MaturityDayMaybeLens NewOrderSingle where
        optMaturityDay = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _ _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      (fmap AlphaHeavy.FIX.Field val)
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance MaturityMonthYearMaybeLens NewOrderSingle where
        optMaturityMonthYear = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _ _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      (fmap AlphaHeavy.FIX.Field val)
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance MaxFloorMaybeLens NewOrderSingle where
        optMaxFloor = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      (fmap AlphaHeavy.FIX.Field val)
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance OpenCloseMaybeLens NewOrderSingle where
        optOpenClose = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _ _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance PegDifferenceMaybeLens NewOrderSingle where
        optPegDifference = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _ _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      (fmap AlphaHeavy.FIX.Field val)
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance PriceMaybeLens NewOrderSingle where
        optPrice = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _ _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      (fmap AlphaHeavy.FIX.Field val)
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance PutOrCallMaybeLens NewOrderSingle where
        optPutOrCall = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _ _x30 _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance SecurityExchangeMaybeLens NewOrderSingle where
        optSecurityExchange = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _ _x31 _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      (fmap AlphaHeavy.FIX.Field val)
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance SecurityIDMaybeLens NewOrderSingle where
        optSecurityID = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _ _x32 _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      (fmap AlphaHeavy.FIX.Field val)
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
 
instance SecurityTypeMaybeLens NewOrderSingle where
        optSecurityType = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _ _x33 _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x33
                      _x34
                      _x35
                      _x36
 
instance StopPxMaybeLens NewOrderSingle where
        optStopPx = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _ _x34 _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      (fmap AlphaHeavy.FIX.Field val)
                      _x34
                      _x35
                      _x36
 
instance StrikePriceMaybeLens NewOrderSingle where
        optStrikePrice = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _ _x35 _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      (fmap AlphaHeavy.FIX.Field val)
                      _x35
                      _x36
 
instance TextMaybeLens NewOrderSingle where
        optText = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _)
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _ _x36)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      (fmap AlphaHeavy.FIX.Field val)
                      _x36
 
instance TimeInForceMaybeLens NewOrderSingle where
        optTimeInForce = Control.Lens.lens g s
          where g (NewOrderSingle _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Enumeration x)))
                  = Just x
                g _ = Nothing
                s (NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _)
                  val
                  = NewOrderSingle _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      (fmap AlphaHeavy.FIX.Enumeration val)
 
data ExecutionReport = ExecutionReport (AlphaHeavy.FIX.Field 6
                                          Price)
                                       (AlphaHeavy.FIX.Field 14 Quantity)
                                       (AlphaHeavy.FIX.Field 17 ExecID)
                                       (AlphaHeavy.FIX.Enumeration 20 ExecTransType)
                                       (AlphaHeavy.FIX.Enumeration 150 ExecType)
                                       (AlphaHeavy.FIX.Field 31 Price)
                                       (AlphaHeavy.FIX.Field 32 Quantity)
                                       (AlphaHeavy.FIX.Field 151 Quantity)
                                       (AlphaHeavy.FIX.Enumeration 39 OrdStatus)
                                       (AlphaHeavy.FIX.Field 37 OrderID)
                                       (AlphaHeavy.FIX.Field 38 Quantity)
                                       (AlphaHeavy.FIX.Enumeration 54 Side)
                                       (AlphaHeavy.FIX.Field 55 Symbol)
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 1 Account))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 11 ClOrdID))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 440 ClearingAccount))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 439 ClearingFirm))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 109 ClientID))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 15 Currency))
                                       (Prelude.Maybe
                                          (AlphaHeavy.FIX.Enumeration 204 CustomerOrFirm))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 100 ExDestination))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 19 ExecRefID))
                                       (Prelude.Maybe
                                          (AlphaHeavy.FIX.Enumeration 378 ExecRestatementReason))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 432 ExpireDate))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 126 ExpireTime))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 30 LastMkt))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 205 MaturityDay))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 200 MaturityMonthYear))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 103 OrdRejReason))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 40 OrdType))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 41 OrigClOrdID))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 44 Price))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 201 PutOrCall))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 47 Rule80A))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 207 SecurityExchange))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 167 SecurityType))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 63 SettlmntTyp))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 99 Price))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 202 Price))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 58 Text))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Enumeration 59 TimeInForce))
                                       (Prelude.Maybe (AlphaHeavy.FIX.Field 60 TransactTime))
                                       (Prelude.Maybe
                                          (AlphaHeavy.FIX.Enumeration 636 WorkingIndicator))
                     deriving (Generic, Show, Eq)
 
instance AvgPxLens ExecutionReport where
        avgPx = Control.Lens.lens g s
          where g (ExecutionReport (AlphaHeavy.FIX.Field x) _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _ _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport (AlphaHeavy.FIX.Field val) _x2 _x3 _x4 _x5 _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance CumQtyLens ExecutionReport where
        cumQty = Control.Lens.lens g s
          where g (ExecutionReport _ (AlphaHeavy.FIX.Field x) _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _x1 _ _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 (AlphaHeavy.FIX.Field val) _x3 _x4 _x5 _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ExecIDLens ExecutionReport where
        execID = Control.Lens.lens g s
          where g (ExecutionReport _ _ (AlphaHeavy.FIX.Field x) _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _x1 _x2 _ _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 (AlphaHeavy.FIX.Field val) _x4 _x5 _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ExecTransTypeLens ExecutionReport where
        execTransType = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ (AlphaHeavy.FIX.Enumeration x) _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _)
                  = x
                s (ExecutionReport _x1 _x2 _x3 _ _x5 _x6 _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 (AlphaHeavy.FIX.Enumeration val) _x5
                      _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ExecTypeLens ExecutionReport where
        execType = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ (AlphaHeavy.FIX.Enumeration x) _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _)
                  = x
                s (ExecutionReport _x1 _x2 _x3 _x4 _ _x6 _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 (AlphaHeavy.FIX.Enumeration val)
                      _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance LastPxLens ExecutionReport where
        lastPx = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ (AlphaHeavy.FIX.Field x) _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _ _x7 _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 (AlphaHeavy.FIX.Field val)
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance LastSharesLens ExecutionReport where
        lastShares = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ (AlphaHeavy.FIX.Field x) _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _ _x8 _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6
                      (AlphaHeavy.FIX.Field val)
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance LeavesQtyLens ExecutionReport where
        leavesQty = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ (AlphaHeavy.FIX.Field x) _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _ _x9 _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7
                      (AlphaHeavy.FIX.Field val)
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance OrdStatusLens ExecutionReport where
        ordStatus = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _
                     (AlphaHeavy.FIX.Enumeration x) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _ _x10 _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8
                      (AlphaHeavy.FIX.Enumeration val)
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance OrderIDLens ExecutionReport where
        orderID = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ (AlphaHeavy.FIX.Field x)
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _ _x11 _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      (AlphaHeavy.FIX.Field val)
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance OrderQtyLens ExecutionReport where
        orderQty = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _
                     (AlphaHeavy.FIX.Field x) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _ _x12
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10
                      (AlphaHeavy.FIX.Field val)
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance SideLens ExecutionReport where
        side = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _
                     (AlphaHeavy.FIX.Enumeration x) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11 _
                     _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      (AlphaHeavy.FIX.Enumeration val)
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance SymbolLens ExecutionReport where
        symbol = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _
                     (AlphaHeavy.FIX.Field x) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _)
                  = x
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _ _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      (AlphaHeavy.FIX.Field val)
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance AccountMaybeLens ExecutionReport where
        optAccount = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _ _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      (fmap AlphaHeavy.FIX.Field val)
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ClOrdIDMaybeLens ExecutionReport where
        optClOrdID = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _ _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      (fmap AlphaHeavy.FIX.Field val)
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ClearingAccountMaybeLens ExecutionReport where
        optClearingAccount = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _ _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      (fmap AlphaHeavy.FIX.Field val)
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ClearingFirmMaybeLens ExecutionReport where
        optClearingFirm = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _ _x18 _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      (fmap AlphaHeavy.FIX.Field val)
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ClientIDMaybeLens ExecutionReport where
        optClientID = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _ _x19 _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      (fmap AlphaHeavy.FIX.Field val)
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance CurrencyMaybeLens ExecutionReport where
        optCurrency = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _ _x20 _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      (fmap AlphaHeavy.FIX.Field val)
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance CustomerOrFirmMaybeLens ExecutionReport where
        optCustomerOrFirm = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _ _x21 _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ExDestinationMaybeLens ExecutionReport where
        optExDestination = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _ _x22 _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      (fmap AlphaHeavy.FIX.Field val)
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ExecRefIDMaybeLens ExecutionReport where
        optExecRefID = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _ _x23 _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      (fmap AlphaHeavy.FIX.Field val)
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ExecRestatementReasonMaybeLens ExecutionReport where
        optExecRestatementReason = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _ _x24 _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ExpireDateMaybeLens ExecutionReport where
        optExpireDate = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _ _x25
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      (fmap AlphaHeavy.FIX.Field val)
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance ExpireTimeMaybeLens ExecutionReport where
        optExpireTime = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24 _
                     _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      (fmap AlphaHeavy.FIX.Field val)
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance LastMktMaybeLens ExecutionReport where
        optLastMkt = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _ _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      (fmap AlphaHeavy.FIX.Field val)
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance MaturityDayMaybeLens ExecutionReport where
        optMaturityDay = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _ _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      (fmap AlphaHeavy.FIX.Field val)
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance MaturityMonthYearMaybeLens ExecutionReport where
        optMaturityMonthYear = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _ _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      (fmap AlphaHeavy.FIX.Field val)
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance OrdRejReasonMaybeLens ExecutionReport where
        optOrdRejReason = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _ _ _ _
                     _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _ _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance OrdTypeMaybeLens ExecutionReport where
        optOrdType = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _ _ _
                     _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _ _x31 _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance OrigClOrdIDMaybeLens ExecutionReport where
        optOrigClOrdID = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _ _x32 _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      (fmap AlphaHeavy.FIX.Field val)
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance PriceMaybeLens ExecutionReport where
        optPrice = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _ _x33 _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      (fmap AlphaHeavy.FIX.Field val)
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance PutOrCallMaybeLens ExecutionReport where
        optPutOrCall = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _
                     _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _ _x34 _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance Rule80AMaybeLens ExecutionReport where
        optRule80A = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _
                     _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _ _x35 _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance SecurityExchangeMaybeLens ExecutionReport where
        optSecurityExchange = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _ _x36 _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      (fmap AlphaHeavy.FIX.Field val)
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance SecurityTypeMaybeLens ExecutionReport where
        optSecurityType = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Enumeration x)) _
                     _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _ _x37 _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance SettlmntTypMaybeLens ExecutionReport where
        optSettlmntTyp = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Enumeration x))
                     _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _ _x38
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance StopPxMaybeLens ExecutionReport where
        optStopPx = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _ _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37 _
                     _x39 _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      (fmap AlphaHeavy.FIX.Field val)
                      _x39
                      _x40
                      _x41
                      _x42
                      _x43
 
instance StrikePriceMaybeLens ExecutionReport where
        optStrikePrice = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x)) _
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37
                     _x38 _ _x40 _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      (fmap AlphaHeavy.FIX.Field val)
                      _x40
                      _x41
                      _x42
                      _x43
 
instance TextMaybeLens ExecutionReport where
        optText = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ (Just (AlphaHeavy.FIX.Field x))
                     _ _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37
                     _x38 _x39 _ _x41 _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      (fmap AlphaHeavy.FIX.Field val)
                      _x41
                      _x42
                      _x43
 
instance TimeInForceMaybeLens ExecutionReport where
        optTimeInForce = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)) _ _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37
                     _x38 _x39 _x40 _ _x42 _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x42
                      _x43
 
instance TransactTimeMaybeLens ExecutionReport where
        optTransactTime = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _)
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37
                     _x38 _x39 _x40 _x41 _ _x43)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      (fmap AlphaHeavy.FIX.Field val)
                      _x43
 
instance WorkingIndicatorMaybeLens ExecutionReport where
        optWorkingIndicator = Control.Lens.lens g s
          where g (ExecutionReport _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)))
                  = Just x
                g _ = Nothing
                s (ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                     _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20 _x21 _x22 _x23 _x24
                     _x25 _x26 _x27 _x28 _x29 _x30 _x31 _x32 _x33 _x34 _x35 _x36 _x37
                     _x38 _x39 _x40 _x41 _x42 _)
                  val
                  = ExecutionReport _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10 _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
                      _x21
                      _x22
                      _x23
                      _x24
                      _x25
                      _x26
                      _x27
                      _x28
                      _x29
                      _x30
                      _x31
                      _x32
                      _x33
                      _x34
                      _x35
                      _x36
                      _x37
                      _x38
                      _x39
                      _x40
                      _x41
                      _x42
                      (fmap AlphaHeavy.FIX.Enumeration val)
 
data OrderCancelReplaceRequest = OrderCancelReplaceRequest (AlphaHeavy.FIX.Field
                                                              11
                                                              ClOrdID)
                                                           (AlphaHeavy.FIX.Enumeration 21 HandlInst)
                                                           (AlphaHeavy.FIX.Enumeration 40 OrdType)
                                                           (AlphaHeavy.FIX.Field 38 Quantity)
                                                           (AlphaHeavy.FIX.Field 41 OrigClOrdID)
                                                           (AlphaHeavy.FIX.Enumeration 54 Side)
                                                           (AlphaHeavy.FIX.Field 55 Symbol)
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Field 440
                                                                 ClearingAccount))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Field 439
                                                                 ClearingFirm))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Field 432 ExpireDate))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Field 126 ExpireTime))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Enumeration 22
                                                                 IDSource))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Field 37 OrderID))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Field 44 Price))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Enumeration 47
                                                                 Rule80A))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Field 48 SecurityID))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Enumeration 63
                                                                 SettlmntTyp))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Field 99 Price))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Field 58 Text))
                                                           (Prelude.Maybe
                                                              (AlphaHeavy.FIX.Enumeration 59
                                                                 TimeInForce))
                               deriving (Generic, Show, Eq)
 
instance ClOrdIDLens OrderCancelReplaceRequest where
        clOrdID = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest (AlphaHeavy.FIX.Field x) _ _ _ _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (OrderCancelReplaceRequest _ _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10
                     _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest (AlphaHeavy.FIX.Field val) _x2 _x3 _x4
                      _x5
                      _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance HandlInstLens OrderCancelReplaceRequest where
        handlInst = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ (AlphaHeavy.FIX.Enumeration x)
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (OrderCancelReplaceRequest _x1 _ _x3 _x4 _x5 _x6 _x7 _x8 _x9 _x10
                     _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 (AlphaHeavy.FIX.Enumeration val)
                      _x3
                      _x4
                      _x5
                      _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance OrdTypeLens OrderCancelReplaceRequest where
        ordType = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _
                     (AlphaHeavy.FIX.Enumeration x) _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (OrderCancelReplaceRequest _x1 _x2 _ _x4 _x5 _x6 _x7 _x8 _x9 _x10
                     _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2
                      (AlphaHeavy.FIX.Enumeration val)
                      _x4
                      _x5
                      _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance OrderQtyLens OrderCancelReplaceRequest where
        orderQty = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ (AlphaHeavy.FIX.Field x) _
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _ _x5 _x6 _x7 _x8 _x9 _x10
                     _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 (AlphaHeavy.FIX.Field val)
                      _x5
                      _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance OrigClOrdIDLens OrderCancelReplaceRequest where
        origClOrdID = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ (AlphaHeavy.FIX.Field x)
                     _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _ _x6 _x7 _x8 _x9 _x10
                     _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4
                      (AlphaHeavy.FIX.Field val)
                      _x6
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance SideLens OrderCancelReplaceRequest where
        side = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _
                     (AlphaHeavy.FIX.Enumeration x) _ _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _ _x7 _x8 _x9 _x10
                     _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5
                      (AlphaHeavy.FIX.Enumeration val)
                      _x7
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance SymbolLens OrderCancelReplaceRequest where
        symbol = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _
                     (AlphaHeavy.FIX.Field x) _ _ _ _ _ _ _ _ _ _ _ _ _)
                  = x
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _ _x8 _x9 _x10
                     _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6
                      (AlphaHeavy.FIX.Field val)
                      _x8
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance ClearingAccountMaybeLens OrderCancelReplaceRequest where
        optClearingAccount = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _ _x9 _x10
                     _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7
                      (fmap AlphaHeavy.FIX.Field val)
                      _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance ClearingFirmMaybeLens OrderCancelReplaceRequest where
        optClearingFirm = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _ _x10
                     _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8
                      (fmap AlphaHeavy.FIX.Field val)
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance ExpireDateMaybeLens OrderCancelReplaceRequest where
        optExpireDate = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9 _
                     _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      (fmap AlphaHeavy.FIX.Field val)
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance ExpireTimeMaybeLens OrderCancelReplaceRequest where
        optExpireTime = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                     _x10 _ _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      _x10
                      (fmap AlphaHeavy.FIX.Field val)
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance IDSourceMaybeLens OrderCancelReplaceRequest where
        optIDSource = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                     _x10 _x11 _ _x13 _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      _x10
                      _x11
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance OrderIDMaybeLens OrderCancelReplaceRequest where
        optOrderID = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                     _x10 _x11 _x12 _ _x14 _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      _x10
                      _x11
                      _x12
                      (fmap AlphaHeavy.FIX.Field val)
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance PriceMaybeLens OrderCancelReplaceRequest where
        optPrice = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                     _x10 _x11 _x12 _x13 _ _x15 _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      (fmap AlphaHeavy.FIX.Field val)
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance Rule80AMaybeLens OrderCancelReplaceRequest where
        optRule80A = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _ _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                     _x10 _x11 _x12 _x13 _x14 _ _x16 _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x16
                      _x17
                      _x18
                      _x19
                      _x20
 
instance SecurityIDMaybeLens OrderCancelReplaceRequest where
        optSecurityID = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _ _ _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                     _x10 _x11 _x12 _x13 _x14 _x15 _ _x17 _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      (fmap AlphaHeavy.FIX.Field val)
                      _x17
                      _x18
                      _x19
                      _x20
 
instance SettlmntTypMaybeLens OrderCancelReplaceRequest where
        optSettlmntTyp = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)) _ _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                     _x10 _x11 _x12 _x13 _x14 _x15 _x16 _ _x18 _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x18
                      _x19
                      _x20
 
instance StopPxMaybeLens OrderCancelReplaceRequest where
        optStopPx = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ (Just (AlphaHeavy.FIX.Field x)) _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                     _x10 _x11 _x12 _x13 _x14 _x15 _x16 _x17 _ _x19 _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      (fmap AlphaHeavy.FIX.Field val)
                      _x19
                      _x20
 
instance TextMaybeLens OrderCancelReplaceRequest where
        optText = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ (Just (AlphaHeavy.FIX.Field x)) _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                     _x10 _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _ _x20)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      (fmap AlphaHeavy.FIX.Field val)
                      _x20
 
instance TimeInForceMaybeLens OrderCancelReplaceRequest where
        optTimeInForce = Control.Lens.lens g s
          where g (OrderCancelReplaceRequest _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _
                     _ _ _ (Just (AlphaHeavy.FIX.Enumeration x)))
                  = Just x
                g _ = Nothing
                s (OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                     _x10 _x11 _x12 _x13 _x14 _x15 _x16 _x17 _x18 _x19 _)
                  val
                  = OrderCancelReplaceRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _x8 _x9
                      _x10
                      _x11
                      _x12
                      _x13
                      _x14
                      _x15
                      _x16
                      _x17
                      _x18
                      _x19
                      (fmap AlphaHeavy.FIX.Enumeration val)
 
data OrderCancelRequest = OrderCancelRequest (AlphaHeavy.FIX.Field
                                                11
                                                ClOrdID)
                                             (AlphaHeavy.FIX.Field 38 Quantity)
                                             (AlphaHeavy.FIX.Field 41 OrigClOrdID)
                                             (AlphaHeavy.FIX.Enumeration 54 Side)
                                             (AlphaHeavy.FIX.Field 55 Symbol)
                                             (Prelude.Maybe
                                                (AlphaHeavy.FIX.Enumeration 22 IDSource))
                                             (Prelude.Maybe (AlphaHeavy.FIX.Field 48 SecurityID))
                                             (Prelude.Maybe (AlphaHeavy.FIX.Field 58 Text))
                        deriving (Generic, Show, Eq)
 
instance ClOrdIDLens OrderCancelRequest where
        clOrdID = Control.Lens.lens g s
          where g (OrderCancelRequest (AlphaHeavy.FIX.Field x) _ _ _ _ _ _ _)
                  = x
                s (OrderCancelRequest _ _x2 _x3 _x4 _x5 _x6 _x7 _x8) val
                  = OrderCancelRequest (AlphaHeavy.FIX.Field val) _x2 _x3 _x4 _x5 _x6
                      _x7
                      _x8
 
instance OrderQtyLens OrderCancelRequest where
        orderQty = Control.Lens.lens g s
          where g (OrderCancelRequest _ (AlphaHeavy.FIX.Field x) _ _ _ _ _ _)
                  = x
                s (OrderCancelRequest _x1 _ _x3 _x4 _x5 _x6 _x7 _x8) val
                  = OrderCancelRequest _x1 (AlphaHeavy.FIX.Field val) _x3 _x4 _x5 _x6
                      _x7
                      _x8
 
instance OrigClOrdIDLens OrderCancelRequest where
        origClOrdID = Control.Lens.lens g s
          where g (OrderCancelRequest _ _ (AlphaHeavy.FIX.Field x) _ _ _ _ _)
                  = x
                s (OrderCancelRequest _x1 _x2 _ _x4 _x5 _x6 _x7 _x8) val
                  = OrderCancelRequest _x1 _x2 (AlphaHeavy.FIX.Field val) _x4 _x5 _x6
                      _x7
                      _x8
 
instance SideLens OrderCancelRequest where
        side = Control.Lens.lens g s
          where g (OrderCancelRequest _ _ _ (AlphaHeavy.FIX.Enumeration x) _
                     _ _ _)
                  = x
                s (OrderCancelRequest _x1 _x2 _x3 _ _x5 _x6 _x7 _x8) val
                  = OrderCancelRequest _x1 _x2 _x3 (AlphaHeavy.FIX.Enumeration val)
                      _x5
                      _x6
                      _x7
                      _x8
 
instance SymbolLens OrderCancelRequest where
        symbol = Control.Lens.lens g s
          where g (OrderCancelRequest _ _ _ _ (AlphaHeavy.FIX.Field x) _ _ _)
                  = x
                s (OrderCancelRequest _x1 _x2 _x3 _x4 _ _x6 _x7 _x8) val
                  = OrderCancelRequest _x1 _x2 _x3 _x4 (AlphaHeavy.FIX.Field val) _x6
                      _x7
                      _x8
 
instance IDSourceMaybeLens OrderCancelRequest where
        optIDSource = Control.Lens.lens g s
          where g (OrderCancelRequest _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)) _ _)
                  = Just x
                g _ = Nothing
                s (OrderCancelRequest _x1 _x2 _x3 _x4 _x5 _ _x7 _x8) val
                  = OrderCancelRequest _x1 _x2 _x3 _x4 _x5
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x7
                      _x8
 
instance SecurityIDMaybeLens OrderCancelRequest where
        optSecurityID = Control.Lens.lens g s
          where g (OrderCancelRequest _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)) _)
                  = Just x
                g _ = Nothing
                s (OrderCancelRequest _x1 _x2 _x3 _x4 _x5 _x6 _ _x8) val
                  = OrderCancelRequest _x1 _x2 _x3 _x4 _x5 _x6
                      (fmap AlphaHeavy.FIX.Field val)
                      _x8
 
instance TextMaybeLens OrderCancelRequest where
        optText = Control.Lens.lens g s
          where g (OrderCancelRequest _ _ _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)))
                  = Just x
                g _ = Nothing
                s (OrderCancelRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7 _) val
                  = OrderCancelRequest _x1 _x2 _x3 _x4 _x5 _x6 _x7
                      (fmap AlphaHeavy.FIX.Field val)
 
data OrderCancelReject = OrderCancelReject (AlphaHeavy.FIX.Field 11
                                              ClOrdID)
                                           (AlphaHeavy.FIX.Enumeration 434 CxlRejResponseTo)
                                           (AlphaHeavy.FIX.Field 37 OrderID)
                                           (AlphaHeavy.FIX.Field 41 OrigClOrdID)
                                           (Prelude.Maybe
                                              (AlphaHeavy.FIX.Enumeration 102 CxlRejReason))
                                           (Prelude.Maybe (AlphaHeavy.FIX.Field 58 Text))
                       deriving (Generic, Show, Eq)
 
instance ClOrdIDLens OrderCancelReject where
        clOrdID = Control.Lens.lens g s
          where g (OrderCancelReject (AlphaHeavy.FIX.Field x) _ _ _ _ _) = x
                s (OrderCancelReject _ _x2 _x3 _x4 _x5 _x6) val
                  = OrderCancelReject (AlphaHeavy.FIX.Field val) _x2 _x3 _x4 _x5 _x6
 
instance CxlRejResponseToLens OrderCancelReject where
        cxlRejResponseTo = Control.Lens.lens g s
          where g (OrderCancelReject _ (AlphaHeavy.FIX.Enumeration x) _ _ _
                     _)
                  = x
                s (OrderCancelReject _x1 _ _x3 _x4 _x5 _x6) val
                  = OrderCancelReject _x1 (AlphaHeavy.FIX.Enumeration val) _x3 _x4
                      _x5
                      _x6
 
instance OrderIDLens OrderCancelReject where
        orderID = Control.Lens.lens g s
          where g (OrderCancelReject _ _ (AlphaHeavy.FIX.Field x) _ _ _) = x
                s (OrderCancelReject _x1 _x2 _ _x4 _x5 _x6) val
                  = OrderCancelReject _x1 _x2 (AlphaHeavy.FIX.Field val) _x4 _x5 _x6
 
instance OrigClOrdIDLens OrderCancelReject where
        origClOrdID = Control.Lens.lens g s
          where g (OrderCancelReject _ _ _ (AlphaHeavy.FIX.Field x) _ _) = x
                s (OrderCancelReject _x1 _x2 _x3 _ _x5 _x6) val
                  = OrderCancelReject _x1 _x2 _x3 (AlphaHeavy.FIX.Field val) _x5 _x6
 
instance CxlRejReasonMaybeLens OrderCancelReject where
        optCxlRejReason = Control.Lens.lens g s
          where g (OrderCancelReject _ _ _ _
                     (Just (AlphaHeavy.FIX.Enumeration x)) _)
                  = Just x
                g _ = Nothing
                s (OrderCancelReject _x1 _x2 _x3 _x4 _ _x6) val
                  = OrderCancelReject _x1 _x2 _x3 _x4
                      (fmap AlphaHeavy.FIX.Enumeration val)
                      _x6
 
instance TextMaybeLens OrderCancelReject where
        optText = Control.Lens.lens g s
          where g (OrderCancelReject _ _ _ _ _
                     (Just (AlphaHeavy.FIX.Field x)))
                  = Just x
                g _ = Nothing
                s (OrderCancelReject _x1 _x2 _x3 _x4 _x5 _) val
                  = OrderCancelReject _x1 _x2 _x3 _x4 _x5
                      (fmap AlphaHeavy.FIX.Field val)
 
data OrderStatusRequest = OrderStatusRequest (AlphaHeavy.FIX.Field
                                                11
                                                ClOrdID)
                        deriving (Generic, Show, Eq)
 
instance ClOrdIDLens OrderStatusRequest where
        clOrdID = Control.Lens.lens g s
          where g (OrderStatusRequest (AlphaHeavy.FIX.Field x)) = x
                s (OrderStatusRequest _) val
                  = OrderStatusRequest (AlphaHeavy.FIX.Field val)
 
data News = News (AlphaHeavy.FIX.Field 6143 DailyNewID)
                 (AlphaHeavy.FIX.Field 148 Headline)
                 (AlphaHeavy.FIX.Enumeration 61 Urgency)
                 (Prelude.Maybe (AlphaHeavy.FIX.Field 207 SecurityExchange))
          deriving (Generic, Show, Eq)
 
instance DailyNewIDLens News where
        dailyNewID = Control.Lens.lens g s
          where g (News (AlphaHeavy.FIX.Field x) _ _ _) = x
                s (News _ _x2 _x3 _x4) val
                  = News (AlphaHeavy.FIX.Field val) _x2 _x3 _x4
 
instance HeadlineLens News where
        headline = Control.Lens.lens g s
          where g (News _ (AlphaHeavy.FIX.Field x) _ _) = x
                s (News _x1 _ _x3 _x4) val
                  = News _x1 (AlphaHeavy.FIX.Field val) _x3 _x4
 
instance UrgencyLens News where
        urgency = Control.Lens.lens g s
          where g (News _ _ (AlphaHeavy.FIX.Enumeration x) _) = x
                s (News _x1 _x2 _ _x4) val
                  = News _x1 _x2 (AlphaHeavy.FIX.Enumeration val) _x4
 
instance SecurityExchangeMaybeLens News where
        optSecurityExchange = Control.Lens.lens g s
          where g (News _ _ _ (Just (AlphaHeavy.FIX.Field x))) = Just x
                g _ = Nothing
                s (News _x1 _x2 _x3 _) val
                  = News _x1 _x2 _x3 (fmap AlphaHeavy.FIX.Field val)
 
data ExecInst = ExecInst_MARKET_PEG
              | ExecInst_PRIMARY_PEG
              | ExecInst_MID_PRICE_PEG
              | ExecInst_ALL_OR_NONE
              | ExecInst_PEG_TO_VWAP
              | ExecInst_TRAILING_STOP_PEG
              | ExecInst_PEG_TO_STOCK
              deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag ExecInst where
        type FieldTagRep ExecInst = String
        toFieldTagRep ExecInst_MARKET_PEG = "P"
        toFieldTagRep ExecInst_PRIMARY_PEG = "R"
        toFieldTagRep ExecInst_MID_PRICE_PEG = "M"
        toFieldTagRep ExecInst_ALL_OR_NONE = "G"
        toFieldTagRep ExecInst_PEG_TO_VWAP = "W"
        toFieldTagRep ExecInst_TRAILING_STOP_PEG = "a"
        toFieldTagRep ExecInst_PEG_TO_STOCK = "s"
        fromFieldTagRep "P" = Just ExecInst_MARKET_PEG
        fromFieldTagRep "R" = Just ExecInst_PRIMARY_PEG
        fromFieldTagRep "M" = Just ExecInst_MID_PRICE_PEG
        fromFieldTagRep "G" = Just ExecInst_ALL_OR_NONE
        fromFieldTagRep "W" = Just ExecInst_PEG_TO_VWAP
        fromFieldTagRep "a" = Just ExecInst_TRAILING_STOP_PEG
        fromFieldTagRep "s" = Just ExecInst_PEG_TO_STOCK
        fromFieldTagRep _ = Nothing
 
data ExecTransType = ExecTransType_NEW
                   | ExecTransType_CANCEL
                   | ExecTransType_CORRECT
                   | ExecTransType_STATUS
                   deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag ExecTransType where
        type FieldTagRep ExecTransType = Char
        toFieldTagRep ExecTransType_NEW = '0'
        toFieldTagRep ExecTransType_CANCEL = '1'
        toFieldTagRep ExecTransType_CORRECT = '2'
        toFieldTagRep ExecTransType_STATUS = '3'
        fromFieldTagRep '0' = Just ExecTransType_NEW
        fromFieldTagRep '1' = Just ExecTransType_CANCEL
        fromFieldTagRep '2' = Just ExecTransType_CORRECT
        fromFieldTagRep '3' = Just ExecTransType_STATUS
        fromFieldTagRep _ = Nothing
 
data HandlInst = HandlInst_AUTOMATED_EXECUTION_ORDER_PUBLIC_BROKER_INTERVENTION_OK
               deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag HandlInst where
        type FieldTagRep HandlInst = Char
        toFieldTagRep
          HandlInst_AUTOMATED_EXECUTION_ORDER_PUBLIC_BROKER_INTERVENTION_OK
          = '2'
        fromFieldTagRep '2'
          = Just
              HandlInst_AUTOMATED_EXECUTION_ORDER_PUBLIC_BROKER_INTERVENTION_OK
        fromFieldTagRep _ = Nothing
 
data IDSource = IDSource_CUSIP
              | IDSource_ISIN_NUMBER
              deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag IDSource where
        type FieldTagRep IDSource = String
        toFieldTagRep IDSource_CUSIP = "1"
        toFieldTagRep IDSource_ISIN_NUMBER = "4"
        fromFieldTagRep "1" = Just IDSource_CUSIP
        fromFieldTagRep "4" = Just IDSource_ISIN_NUMBER
        fromFieldTagRep _ = Nothing
 
data MsgType = MsgType_HEARTBEAT
             | MsgType_TEST_REQUEST
             | MsgType_RESEND_REQUEST
             | MsgType_REJECT
             | MsgType_SEQUENCE_RESET
             | MsgType_LOGOUT
             | MsgType_EXECUTION_REPORT
             | MsgType_ORDER_CANCEL_REJECT
             | MsgType_LOGON
             | MsgType_ORDER_SINGLE
             | MsgType_ORDER_CANCEL_REQUEST
             | MsgType_ORDER_CANCEL_REPLACE_REQUEST
             | MsgType_ORDER_STATUS_REQUEST
             | MsgType_NEW_ORDER_MULTILEG
             | MsgType_MULTILEG_CANCEL_REPLACE_REQUEST
             deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag MsgType where
        type FieldTagRep MsgType = String
        toFieldTagRep MsgType_HEARTBEAT = "0"
        toFieldTagRep MsgType_TEST_REQUEST = "1"
        toFieldTagRep MsgType_RESEND_REQUEST = "2"
        toFieldTagRep MsgType_REJECT = "3"
        toFieldTagRep MsgType_SEQUENCE_RESET = "4"
        toFieldTagRep MsgType_LOGOUT = "5"
        toFieldTagRep MsgType_EXECUTION_REPORT = "8"
        toFieldTagRep MsgType_ORDER_CANCEL_REJECT = "9"
        toFieldTagRep MsgType_LOGON = "A"
        toFieldTagRep MsgType_ORDER_SINGLE = "D"
        toFieldTagRep MsgType_ORDER_CANCEL_REQUEST = "F"
        toFieldTagRep MsgType_ORDER_CANCEL_REPLACE_REQUEST = "G"
        toFieldTagRep MsgType_ORDER_STATUS_REQUEST = "H"
        toFieldTagRep MsgType_NEW_ORDER_MULTILEG = "AB"
        toFieldTagRep MsgType_MULTILEG_CANCEL_REPLACE_REQUEST = "AC"
        fromFieldTagRep "0" = Just MsgType_HEARTBEAT
        fromFieldTagRep "1" = Just MsgType_TEST_REQUEST
        fromFieldTagRep "2" = Just MsgType_RESEND_REQUEST
        fromFieldTagRep "3" = Just MsgType_REJECT
        fromFieldTagRep "4" = Just MsgType_SEQUENCE_RESET
        fromFieldTagRep "5" = Just MsgType_LOGOUT
        fromFieldTagRep "8" = Just MsgType_EXECUTION_REPORT
        fromFieldTagRep "9" = Just MsgType_ORDER_CANCEL_REJECT
        fromFieldTagRep "A" = Just MsgType_LOGON
        fromFieldTagRep "D" = Just MsgType_ORDER_SINGLE
        fromFieldTagRep "F" = Just MsgType_ORDER_CANCEL_REQUEST
        fromFieldTagRep "G" = Just MsgType_ORDER_CANCEL_REPLACE_REQUEST
        fromFieldTagRep "H" = Just MsgType_ORDER_STATUS_REQUEST
        fromFieldTagRep "AB" = Just MsgType_NEW_ORDER_MULTILEG
        fromFieldTagRep "AC" = Just MsgType_MULTILEG_CANCEL_REPLACE_REQUEST
        fromFieldTagRep _ = Nothing
 
data OrdStatus = OrdStatus_NEW
               | OrdStatus_PARTIALLY_FILLED
               | OrdStatus_FILLED
               | OrdStatus_CANCELED
               | OrdStatus_REPLACED
               | OrdStatus_PENDING_CANCEL
               | OrdStatus_REJECTED
               | OrdStatus_PENDING_NEW
               | OrdStatus_EXPIRED
               | OrdStatus_PENDING_REPLACE
               deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag OrdStatus where
        type FieldTagRep OrdStatus = Char
        toFieldTagRep OrdStatus_NEW = '0'
        toFieldTagRep OrdStatus_PARTIALLY_FILLED = '1'
        toFieldTagRep OrdStatus_FILLED = '2'
        toFieldTagRep OrdStatus_CANCELED = '4'
        toFieldTagRep OrdStatus_REPLACED = '5'
        toFieldTagRep OrdStatus_PENDING_CANCEL = '6'
        toFieldTagRep OrdStatus_REJECTED = '8'
        toFieldTagRep OrdStatus_PENDING_NEW = 'A'
        toFieldTagRep OrdStatus_EXPIRED = 'C'
        toFieldTagRep OrdStatus_PENDING_REPLACE = 'E'
        fromFieldTagRep '0' = Just OrdStatus_NEW
        fromFieldTagRep '1' = Just OrdStatus_PARTIALLY_FILLED
        fromFieldTagRep '2' = Just OrdStatus_FILLED
        fromFieldTagRep '4' = Just OrdStatus_CANCELED
        fromFieldTagRep '5' = Just OrdStatus_REPLACED
        fromFieldTagRep '6' = Just OrdStatus_PENDING_CANCEL
        fromFieldTagRep '8' = Just OrdStatus_REJECTED
        fromFieldTagRep 'A' = Just OrdStatus_PENDING_NEW
        fromFieldTagRep 'C' = Just OrdStatus_EXPIRED
        fromFieldTagRep 'E' = Just OrdStatus_PENDING_REPLACE
        fromFieldTagRep _ = Nothing
 
data OrdType = OrdType_MARKET
             | OrdType_LIMIT
             | OrdType_STOP
             | OrdType_STOP_LIMIT
             | OrdType_MARKET_ON_CLOSE
             | OrdType_LIMIT_ON_CLOSE
             | OrdType_PEGGED
             deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag OrdType where
        type FieldTagRep OrdType = Char
        toFieldTagRep OrdType_MARKET = '1'
        toFieldTagRep OrdType_LIMIT = '2'
        toFieldTagRep OrdType_STOP = '3'
        toFieldTagRep OrdType_STOP_LIMIT = '4'
        toFieldTagRep OrdType_MARKET_ON_CLOSE = '5'
        toFieldTagRep OrdType_LIMIT_ON_CLOSE = 'B'
        toFieldTagRep OrdType_PEGGED = 'P'
        fromFieldTagRep '1' = Just OrdType_MARKET
        fromFieldTagRep '2' = Just OrdType_LIMIT
        fromFieldTagRep '3' = Just OrdType_STOP
        fromFieldTagRep '4' = Just OrdType_STOP_LIMIT
        fromFieldTagRep '5' = Just OrdType_MARKET_ON_CLOSE
        fromFieldTagRep 'B' = Just OrdType_LIMIT_ON_CLOSE
        fromFieldTagRep 'P' = Just OrdType_PEGGED
        fromFieldTagRep _ = Nothing
 
data PossDupFlag = PossDupFlag_NO
                 | PossDupFlag_YES
                 deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag PossDupFlag where
        type FieldTagRep PossDupFlag = Bool
        toFieldTagRep PossDupFlag_NO = False
        toFieldTagRep PossDupFlag_YES = True
        fromFieldTagRep False = Just PossDupFlag_NO
        fromFieldTagRep True = Just PossDupFlag_YES
 
data Rule80A = Rule80A_AGENCY_SINGLE_ORDER
             | Rule80A_PROGRAM_ORDER_INDEX_ARB_FOR_INDIVIDUAL_CUSTOMER
             | Rule80A_PROGRAM_ORDER_NON_INDEX_ARB_FOR_INDIVIDUAL_CUSTOMER
             | Rule80A_INDIVIDUAL_INVESTOR_SINGLE_ORDER
             | Rule80A_PRINCIPAL
             | Rule80A_PROGRAM_ORDER_INDEX_ARB_FOR_OTHER_AGENCY
             | Rule80A_PROGRAM_ORDER_NON_INDEX_ARB_FOR_OTHER_AGENCY
             | Rule80A_PROGRAM_ORDER_INDEX_ARB_FOR_OTHER_MEMBER
             | Rule80A_PROGRAM_ORDER_NON_INDEX_ARB_FOR_OTHER_MEMBER
             | Rule80A_ALL_OTHER_ORDERS_AS_AGENT_FOR_OTHER_MEMBER
             deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag Rule80A where
        type FieldTagRep Rule80A = Char
        toFieldTagRep Rule80A_AGENCY_SINGLE_ORDER = 'A'
        toFieldTagRep
          Rule80A_PROGRAM_ORDER_INDEX_ARB_FOR_INDIVIDUAL_CUSTOMER = 'J'
        toFieldTagRep
          Rule80A_PROGRAM_ORDER_NON_INDEX_ARB_FOR_INDIVIDUAL_CUSTOMER = 'K'
        toFieldTagRep Rule80A_INDIVIDUAL_INVESTOR_SINGLE_ORDER = 'I'
        toFieldTagRep Rule80A_PRINCIPAL = 'P'
        toFieldTagRep Rule80A_PROGRAM_ORDER_INDEX_ARB_FOR_OTHER_AGENCY
          = 'U'
        toFieldTagRep Rule80A_PROGRAM_ORDER_NON_INDEX_ARB_FOR_OTHER_AGENCY
          = 'Y'
        toFieldTagRep Rule80A_PROGRAM_ORDER_INDEX_ARB_FOR_OTHER_MEMBER
          = 'M'
        toFieldTagRep Rule80A_PROGRAM_ORDER_NON_INDEX_ARB_FOR_OTHER_MEMBER
          = 'N'
        toFieldTagRep Rule80A_ALL_OTHER_ORDERS_AS_AGENT_FOR_OTHER_MEMBER
          = 'W'
        fromFieldTagRep 'A' = Just Rule80A_AGENCY_SINGLE_ORDER
        fromFieldTagRep 'J'
          = Just Rule80A_PROGRAM_ORDER_INDEX_ARB_FOR_INDIVIDUAL_CUSTOMER
        fromFieldTagRep 'K'
          = Just Rule80A_PROGRAM_ORDER_NON_INDEX_ARB_FOR_INDIVIDUAL_CUSTOMER
        fromFieldTagRep 'I' = Just Rule80A_INDIVIDUAL_INVESTOR_SINGLE_ORDER
        fromFieldTagRep 'P' = Just Rule80A_PRINCIPAL
        fromFieldTagRep 'U'
          = Just Rule80A_PROGRAM_ORDER_INDEX_ARB_FOR_OTHER_AGENCY
        fromFieldTagRep 'Y'
          = Just Rule80A_PROGRAM_ORDER_NON_INDEX_ARB_FOR_OTHER_AGENCY
        fromFieldTagRep 'M'
          = Just Rule80A_PROGRAM_ORDER_INDEX_ARB_FOR_OTHER_MEMBER
        fromFieldTagRep 'N'
          = Just Rule80A_PROGRAM_ORDER_NON_INDEX_ARB_FOR_OTHER_MEMBER
        fromFieldTagRep 'W'
          = Just Rule80A_ALL_OTHER_ORDERS_AS_AGENT_FOR_OTHER_MEMBER
        fromFieldTagRep _ = Nothing
 
data Side = Side_ERROR
          | Side_BUY
          | Side_SELL
          | Side_BUY_MINUS
          | Side_SELL_PLUS
          | Side_SELL_SHORT
          | Side_SELL_SHORT_EXEMPT
          deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag Side where
        type FieldTagRep Side = Char
        toFieldTagRep Side_ERROR = '0'
        toFieldTagRep Side_BUY = '1'
        toFieldTagRep Side_SELL = '2'
        toFieldTagRep Side_BUY_MINUS = '3'
        toFieldTagRep Side_SELL_PLUS = '4'
        toFieldTagRep Side_SELL_SHORT = '5'
        toFieldTagRep Side_SELL_SHORT_EXEMPT = '6'
        fromFieldTagRep '0' = Just Side_ERROR
        fromFieldTagRep '1' = Just Side_BUY
        fromFieldTagRep '2' = Just Side_SELL
        fromFieldTagRep '3' = Just Side_BUY_MINUS
        fromFieldTagRep '4' = Just Side_SELL_PLUS
        fromFieldTagRep '5' = Just Side_SELL_SHORT
        fromFieldTagRep '6' = Just Side_SELL_SHORT_EXEMPT
        fromFieldTagRep _ = Nothing
 
data TimeInForce = TimeInForce_DAY
                 | TimeInForce_GOOD_TILL_CANCEL
                 | TimeInForce_AT_THE_OPENING
                 | TimeInForce_IMMEDIATE_OR_CANCEL
                 | TimeInForce_FILL_OR_KILL
                 | TimeInForce_GOOD_TILL_DATE
                 | TimeInForce_AT_THE_CLOSING
                 | TimeInForce_AUCTION
                 deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag TimeInForce where
        type FieldTagRep TimeInForce = Char
        toFieldTagRep TimeInForce_DAY = '0'
        toFieldTagRep TimeInForce_GOOD_TILL_CANCEL = '1'
        toFieldTagRep TimeInForce_AT_THE_OPENING = '2'
        toFieldTagRep TimeInForce_IMMEDIATE_OR_CANCEL = '3'
        toFieldTagRep TimeInForce_FILL_OR_KILL = '4'
        toFieldTagRep TimeInForce_GOOD_TILL_DATE = '6'
        toFieldTagRep TimeInForce_AT_THE_CLOSING = '7'
        toFieldTagRep TimeInForce_AUCTION = '8'
        fromFieldTagRep '0' = Just TimeInForce_DAY
        fromFieldTagRep '1' = Just TimeInForce_GOOD_TILL_CANCEL
        fromFieldTagRep '2' = Just TimeInForce_AT_THE_OPENING
        fromFieldTagRep '3' = Just TimeInForce_IMMEDIATE_OR_CANCEL
        fromFieldTagRep '4' = Just TimeInForce_FILL_OR_KILL
        fromFieldTagRep '6' = Just TimeInForce_GOOD_TILL_DATE
        fromFieldTagRep '7' = Just TimeInForce_AT_THE_CLOSING
        fromFieldTagRep '8' = Just TimeInForce_AUCTION
        fromFieldTagRep _ = Nothing
 
data Urgency = Urgency_NORMAL
             | Urgency_FLASH
             | Urgency_BACKGROUND
             deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag Urgency where
        type FieldTagRep Urgency = Char
        toFieldTagRep Urgency_NORMAL = '0'
        toFieldTagRep Urgency_FLASH = '1'
        toFieldTagRep Urgency_BACKGROUND = '2'
        fromFieldTagRep '0' = Just Urgency_NORMAL
        fromFieldTagRep '1' = Just Urgency_FLASH
        fromFieldTagRep '2' = Just Urgency_BACKGROUND
        fromFieldTagRep _ = Nothing
 
data SettlmntTyp = SettlmntTyp_REGULAR
                 deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag SettlmntTyp where
        type FieldTagRep SettlmntTyp = Char
        toFieldTagRep SettlmntTyp_REGULAR = '0'
        fromFieldTagRep '0' = Just SettlmntTyp_REGULAR
        fromFieldTagRep _ = Nothing
 
data OpenClose = OpenClose_CLOSE
               | OpenClose_OPEN
               deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag OpenClose where
        type FieldTagRep OpenClose = Char
        toFieldTagRep OpenClose_CLOSE = 'C'
        toFieldTagRep OpenClose_OPEN = 'O'
        fromFieldTagRep 'C' = Just OpenClose_CLOSE
        fromFieldTagRep 'O' = Just OpenClose_OPEN
        fromFieldTagRep _ = Nothing
 
data PossResend = PossResend_NO
                | PossResend_YES
                deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag PossResend where
        type FieldTagRep PossResend = Bool
        toFieldTagRep PossResend_NO = False
        toFieldTagRep PossResend_YES = True
        fromFieldTagRep False = Just PossResend_NO
        fromFieldTagRep True = Just PossResend_YES
 
data EncryptMethod = EncryptMethod_NONE
                   deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag EncryptMethod where
        type FieldTagRep EncryptMethod = Int
        toFieldTagRep EncryptMethod_NONE = 0
        fromFieldTagRep 0 = Just EncryptMethod_NONE
        fromFieldTagRep _ = Nothing
 
data CxlRejReason = CxlRejReason_TOO_LATE_TO_CANCEL
                  | CxlRejReason_UNKNOWN_ORDER
                  | CxlRejReason_BROKER_OPTION
                  | CxlRejReason_ORDER_ALREADY_IN_PENDING_CANCEL_OR_PENDING_REPLACE_STATUS
                  deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag CxlRejReason where
        type FieldTagRep CxlRejReason = Int
        toFieldTagRep CxlRejReason_TOO_LATE_TO_CANCEL = 0
        toFieldTagRep CxlRejReason_UNKNOWN_ORDER = 1
        toFieldTagRep CxlRejReason_BROKER_OPTION = 2
        toFieldTagRep
          CxlRejReason_ORDER_ALREADY_IN_PENDING_CANCEL_OR_PENDING_REPLACE_STATUS
          = 3
        fromFieldTagRep 0 = Just CxlRejReason_TOO_LATE_TO_CANCEL
        fromFieldTagRep 1 = Just CxlRejReason_UNKNOWN_ORDER
        fromFieldTagRep 2 = Just CxlRejReason_BROKER_OPTION
        fromFieldTagRep 3
          = Just
              CxlRejReason_ORDER_ALREADY_IN_PENDING_CANCEL_OR_PENDING_REPLACE_STATUS
        fromFieldTagRep _ = Nothing
 
data OrdRejReason = OrdRejReason_BROKER_OPTION
                  | OrdRejReason_ORDER_EXCEEDS_LIMIT
                  deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag OrdRejReason where
        type FieldTagRep OrdRejReason = Int
        toFieldTagRep OrdRejReason_BROKER_OPTION = 0
        toFieldTagRep OrdRejReason_ORDER_EXCEEDS_LIMIT = 3
        fromFieldTagRep 0 = Just OrdRejReason_BROKER_OPTION
        fromFieldTagRep 3 = Just OrdRejReason_ORDER_EXCEEDS_LIMIT
        fromFieldTagRep _ = Nothing
 
data LocateReqd = LocateReqd_NO
                | LocateReqd_YES
                deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag LocateReqd where
        type FieldTagRep LocateReqd = Bool
        toFieldTagRep LocateReqd_NO = False
        toFieldTagRep LocateReqd_YES = True
        fromFieldTagRep False = Just LocateReqd_NO
        fromFieldTagRep True = Just LocateReqd_YES
 
data GapFillFlag = GapFillFlag_NO
                 | GapFillFlag_YES
                 deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag GapFillFlag where
        type FieldTagRep GapFillFlag = Bool
        toFieldTagRep GapFillFlag_NO = False
        toFieldTagRep GapFillFlag_YES = True
        fromFieldTagRep False = Just GapFillFlag_NO
        fromFieldTagRep True = Just GapFillFlag_YES
 
data ResetSeqNumFlag = ResetSeqNumFlag_NO
                     | ResetSeqNumFlag_YES
                     deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag ResetSeqNumFlag where
        type FieldTagRep ResetSeqNumFlag = Bool
        toFieldTagRep ResetSeqNumFlag_NO = False
        toFieldTagRep ResetSeqNumFlag_YES = True
        fromFieldTagRep False = Just ResetSeqNumFlag_NO
        fromFieldTagRep True = Just ResetSeqNumFlag_YES
 
data ExecType = ExecType_NEW
              | ExecType_PARTIAL_FILL
              | ExecType_FILL
              | ExecType_CANCELED
              | ExecType_REPLACE
              | ExecType_PENDING_CANCEL
              | ExecType_REJECTED
              | ExecType_PENDING_NEW
              | ExecType_EXPIRED
              | ExecType_RESTATED
              | ExecType_PENDING_REPLACE
              deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag ExecType where
        type FieldTagRep ExecType = Char
        toFieldTagRep ExecType_NEW = '0'
        toFieldTagRep ExecType_PARTIAL_FILL = '1'
        toFieldTagRep ExecType_FILL = '2'
        toFieldTagRep ExecType_CANCELED = '4'
        toFieldTagRep ExecType_REPLACE = '5'
        toFieldTagRep ExecType_PENDING_CANCEL = '6'
        toFieldTagRep ExecType_REJECTED = '8'
        toFieldTagRep ExecType_PENDING_NEW = 'A'
        toFieldTagRep ExecType_EXPIRED = 'C'
        toFieldTagRep ExecType_RESTATED = 'D'
        toFieldTagRep ExecType_PENDING_REPLACE = 'E'
        fromFieldTagRep '0' = Just ExecType_NEW
        fromFieldTagRep '1' = Just ExecType_PARTIAL_FILL
        fromFieldTagRep '2' = Just ExecType_FILL
        fromFieldTagRep '4' = Just ExecType_CANCELED
        fromFieldTagRep '5' = Just ExecType_REPLACE
        fromFieldTagRep '6' = Just ExecType_PENDING_CANCEL
        fromFieldTagRep '8' = Just ExecType_REJECTED
        fromFieldTagRep 'A' = Just ExecType_PENDING_NEW
        fromFieldTagRep 'C' = Just ExecType_EXPIRED
        fromFieldTagRep 'D' = Just ExecType_RESTATED
        fromFieldTagRep 'E' = Just ExecType_PENDING_REPLACE
        fromFieldTagRep _ = Nothing
 
data SecurityType = SecurityType_COMMON_STOCK
                  | SecurityType_FUTURE
                  | SecurityType_OPTION
                  | SecurityType_FUTURE_OPTION
                  | SecurityType_WARRANT
                  | SecurityType_MULTILEG
                  | SecurityType_FOREX
                  | SecurityType_BOND
                  | SecurityType_CFD
                  | SecurityType_COMMODITY
                  | SecurityType_MUTUAL_FUND
                  deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag SecurityType where
        type FieldTagRep SecurityType = String
        toFieldTagRep SecurityType_COMMON_STOCK = "CS"
        toFieldTagRep SecurityType_FUTURE = "FUT"
        toFieldTagRep SecurityType_OPTION = "OPT"
        toFieldTagRep SecurityType_FUTURE_OPTION = "FOP"
        toFieldTagRep SecurityType_WARRANT = "WAR"
        toFieldTagRep SecurityType_MULTILEG = "MLEG"
        toFieldTagRep SecurityType_FOREX = "CASH"
        toFieldTagRep SecurityType_BOND = "BOND"
        toFieldTagRep SecurityType_CFD = "CFD"
        toFieldTagRep SecurityType_COMMODITY = "CMDTY"
        toFieldTagRep SecurityType_MUTUAL_FUND = "FUND"
        fromFieldTagRep "CS" = Just SecurityType_COMMON_STOCK
        fromFieldTagRep "FUT" = Just SecurityType_FUTURE
        fromFieldTagRep "OPT" = Just SecurityType_OPTION
        fromFieldTagRep "FOP" = Just SecurityType_FUTURE_OPTION
        fromFieldTagRep "WAR" = Just SecurityType_WARRANT
        fromFieldTagRep "MLEG" = Just SecurityType_MULTILEG
        fromFieldTagRep "CASH" = Just SecurityType_FOREX
        fromFieldTagRep "BOND" = Just SecurityType_BOND
        fromFieldTagRep "CFD" = Just SecurityType_CFD
        fromFieldTagRep "CMDTY" = Just SecurityType_COMMODITY
        fromFieldTagRep "FUND" = Just SecurityType_MUTUAL_FUND
        fromFieldTagRep _ = Nothing
 
data PutOrCall = PutOrCall_PUT
               | PutOrCall_CALL
               deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag PutOrCall where
        type FieldTagRep PutOrCall = Int
        toFieldTagRep PutOrCall_PUT = 0
        toFieldTagRep PutOrCall_CALL = 1
        fromFieldTagRep 0 = Just PutOrCall_PUT
        fromFieldTagRep 1 = Just PutOrCall_CALL
        fromFieldTagRep _ = Nothing
 
data CustomerOrFirm = CustomerOrFirm_CUSTOMER
                    | CustomerOrFirm_FIRM
                    deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag CustomerOrFirm where
        type FieldTagRep CustomerOrFirm = Int
        toFieldTagRep CustomerOrFirm_CUSTOMER = 0
        toFieldTagRep CustomerOrFirm_FIRM = 1
        fromFieldTagRep 0 = Just CustomerOrFirm_CUSTOMER
        fromFieldTagRep 1 = Just CustomerOrFirm_FIRM
        fromFieldTagRep _ = Nothing
 
data ExecRestatementReason = ExecRestatementReason_GT_CORPORATE_ACTION
                           | ExecRestatementReason_GT_RENEWAL
                           | ExecRestatementReason_VERBAL_CHANGE
                           | ExecRestatementReason_REPRICING_OF_ORDER
                           | ExecRestatementReason_BROKER_OPTION
                           | ExecRestatementReason_PARTIAL_DECLINE_OF_ORDERQTY
                           deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag ExecRestatementReason where
        type FieldTagRep ExecRestatementReason = Int
        toFieldTagRep ExecRestatementReason_GT_CORPORATE_ACTION = 0
        toFieldTagRep ExecRestatementReason_GT_RENEWAL = 1
        toFieldTagRep ExecRestatementReason_VERBAL_CHANGE = 2
        toFieldTagRep ExecRestatementReason_REPRICING_OF_ORDER = 3
        toFieldTagRep ExecRestatementReason_BROKER_OPTION = 4
        toFieldTagRep ExecRestatementReason_PARTIAL_DECLINE_OF_ORDERQTY = 5
        fromFieldTagRep 0 = Just ExecRestatementReason_GT_CORPORATE_ACTION
        fromFieldTagRep 1 = Just ExecRestatementReason_GT_RENEWAL
        fromFieldTagRep 2 = Just ExecRestatementReason_VERBAL_CHANGE
        fromFieldTagRep 3 = Just ExecRestatementReason_REPRICING_OF_ORDER
        fromFieldTagRep 4 = Just ExecRestatementReason_BROKER_OPTION
        fromFieldTagRep 5
          = Just ExecRestatementReason_PARTIAL_DECLINE_OF_ORDERQTY
        fromFieldTagRep _ = Nothing
 
data DiscretionInst = DiscretionInst_RELATED_TO_DISPLAYED_PRICE
                    deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag DiscretionInst where
        type FieldTagRep DiscretionInst = Char
        toFieldTagRep DiscretionInst_RELATED_TO_DISPLAYED_PRICE = '0'
        fromFieldTagRep '0'
          = Just DiscretionInst_RELATED_TO_DISPLAYED_PRICE
        fromFieldTagRep _ = Nothing
 
data CxlRejResponseTo = CxlRejResponseTo_ORDER_CANCEL_REQUEST
                      | CxlRejResponseTo_ORDER_CANCEL_REPLACE_REQUEST
                      deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag CxlRejResponseTo where
        type FieldTagRep CxlRejResponseTo = Char
        toFieldTagRep CxlRejResponseTo_ORDER_CANCEL_REQUEST = '1'
        toFieldTagRep CxlRejResponseTo_ORDER_CANCEL_REPLACE_REQUEST = '2'
        fromFieldTagRep '1' = Just CxlRejResponseTo_ORDER_CANCEL_REQUEST
        fromFieldTagRep '2'
          = Just CxlRejResponseTo_ORDER_CANCEL_REPLACE_REQUEST
        fromFieldTagRep _ = Nothing
 
data WorkingIndicator = WorkingIndicator_NO
                      | WorkingIndicator_YES
                      deriving (Read, Show, Eq)
 
instance AlphaHeavy.FIX.FieldTag WorkingIndicator where
        type FieldTagRep WorkingIndicator = Bool
        toFieldTagRep WorkingIndicator_NO = False
        toFieldTagRep WorkingIndicator_YES = True
        fromFieldTagRep False = Just WorkingIndicator_NO
        fromFieldTagRep True = Just WorkingIndicator_YES
 
newtype Account = Account String
                deriving (Generic, Show, Eq, Ord)
 
newtype BeginSeqNo = BeginSeqNo Int
                   deriving (Generic, Show, Eq, Num, Ord, Real, Enum, Integral)
 
newtype BeginString = BeginString String
                    deriving (Generic, Show, Eq, Ord)
 
newtype BodyLength = BodyLength Int
                   deriving (Generic, Show, Eq, Num, Ord, Real, Enum, Integral)
 
newtype CheckSum = CheckSum String
                 deriving (Generic, Show, Eq, Ord)
 
newtype ClOrdID = ClOrdID String
                deriving (Generic, Show, Eq, Ord)
 
newtype EndSeqNo = EndSeqNo Int
                 deriving (Generic, Show, Eq, Num, Ord, Real, Enum, Integral)
 
newtype ExecID = ExecID String
               deriving (Generic, Show, Eq, Ord)
 
newtype ExecRefID = ExecRefID String
                  deriving (Generic, Show, Eq, Ord)
 
newtype LastMkt = LastMkt Exchange
                deriving (Generic, Show, Eq)
 
newtype MsgSeqNum = MsgSeqNum Int
                  deriving (Generic, Show, Eq, Num, Ord, Real, Enum, Integral)
 
newtype NewSeqNo = NewSeqNo Int
                 deriving (Generic, Show, Eq, Num, Ord, Real, Enum, Integral)
 
newtype OrderID = OrderID String
                deriving (Generic, Show, Eq, Ord)
 
newtype OrigClOrdID = OrigClOrdID String
                    deriving (Generic, Show, Eq, Ord)
 
newtype RefSeqNum = RefSeqNum Int
                  deriving (Generic, Show, Eq, Num, Ord, Real, Enum, Integral)
 
newtype SecurityID = SecurityID String
                   deriving (Generic, Show, Eq, Ord)
 
newtype SenderCompID = SenderCompID String
                     deriving (Generic, Show, Eq, Ord)
 
newtype SenderSubID = SenderSubID String
                    deriving (Generic, Show, Eq, Ord)
 
newtype SendingDate = SendingDate MarketLocalTime
                    deriving (Generic, Show, Eq)
 
newtype SendingTime = SendingTime UTCTimeStamp
                    deriving (Generic, Show, Eq)
 
newtype Symbol = Symbol String
               deriving (Generic, Show, Eq, Ord)
 
newtype TargetCompID = TargetCompID String
                     deriving (Generic, Show, Eq, Ord)
 
newtype TargetSubID = TargetSubID String
                    deriving (Generic, Show, Eq, Ord)
 
newtype Text = Text String
             deriving (Generic, Show, Eq, Ord)
 
newtype TransactTime = TransactTime UTCTimeStamp
                     deriving (Generic, Show, Eq)
 
newtype ExDestination = ExDestination Exchange
                      deriving (Generic, Show, Eq)
 
newtype HeartBtInt = HeartBtInt Int
                   deriving (Generic, Show, Eq, Num, Ord, Real, Enum, Integral)
 
newtype ClientID = ClientID String
                 deriving (Generic, Show, Eq, Ord)
 
newtype TestReqID = TestReqID String
                  deriving (Generic, Show, Eq, Ord)
 
newtype OrigSendingTime = OrigSendingTime UTCTimeStamp
                        deriving (Generic, Show, Eq)
 
newtype ExpireTime = ExpireTime UTCTimeStamp
                   deriving (Generic, Show, Eq)
 
newtype Headline = Headline String
                 deriving (Generic, Show, Eq, Ord)
 
newtype EffectiveTime = EffectiveTime UTCTimeStamp
                      deriving (Generic, Show, Eq)
 
newtype MaturityMonthYear = MaturityMonthYear MonthYear
                          deriving (Generic, Show, Eq)
 
newtype MaturityDay = MaturityDay DayOfMonth
                    deriving (Generic, Show, Eq)
 
newtype SecurityExchange = SecurityExchange Exchange
                         deriving (Generic, Show, Eq)
 
newtype PegDifference = PegDifference Decimal
                      deriving (Generic, Show, Eq)
 
newtype ContractMultiplier = ContractMultiplier Double
                           deriving (Generic, Show, Eq, Enum, Floating, Fractional, Num, Ord,
                                     Real, RealFloat, RealFrac)
 
newtype DiscretionOffset = DiscretionOffset Decimal
                         deriving (Generic, Show, Eq)
 
newtype ExpireDate = ExpireDate MarketLocalTime
                   deriving (Generic, Show, Eq)
 
newtype ClearingFirm = ClearingFirm String
                     deriving (Generic, Show, Eq, Ord)
 
newtype ClearingAccount = ClearingAccount String
                        deriving (Generic, Show, Eq, Ord)
 
newtype MPID = MPID String
             deriving (Generic, Show, Eq, Ord)
 
newtype DailyNewID = DailyNewID Int
                   deriving (Generic, Show, Eq, Num, Ord, Real, Enum, Integral)
 
class AccountLens a where
         
        account :: Control.Lens.Lens' a Account
 
class AvgPxLens a where
         
        avgPx :: Control.Lens.Lens' a Price
 
class BeginSeqNoLens a where
         
        beginSeqNo :: Control.Lens.Lens' a BeginSeqNo
 
class BeginStringLens a where
         
        beginString :: Control.Lens.Lens' a BeginString
 
class BodyLengthLens a where
         
        bodyLength :: Control.Lens.Lens' a BodyLength
 
class CheckSumLens a where
         
        checkSum :: Control.Lens.Lens' a CheckSum
 
class ClOrdIDLens a where
         
        clOrdID :: Control.Lens.Lens' a ClOrdID
 
class CumQtyLens a where
         
        cumQty :: Control.Lens.Lens' a Quantity
 
class CurrencyLens a where
         
        currency :: Control.Lens.Lens' a Currency
 
class EndSeqNoLens a where
         
        endSeqNo :: Control.Lens.Lens' a EndSeqNo
 
class ExecIDLens a where
         
        execID :: Control.Lens.Lens' a ExecID
 
class ExecInstLens a where
         
        execInst :: Control.Lens.Lens' a ExecInst
 
class ExecRefIDLens a where
         
        execRefID :: Control.Lens.Lens' a ExecRefID
 
class ExecTransTypeLens a where
         
        execTransType :: Control.Lens.Lens' a ExecTransType
 
class HandlInstLens a where
         
        handlInst :: Control.Lens.Lens' a HandlInst
 
class IDSourceLens a where
         
        iDSource :: Control.Lens.Lens' a IDSource
 
class LastMktLens a where
         
        lastMkt :: Control.Lens.Lens' a LastMkt
 
class LastPxLens a where
         
        lastPx :: Control.Lens.Lens' a Price
 
class LastSharesLens a where
         
        lastShares :: Control.Lens.Lens' a Quantity
 
class MsgSeqNumLens a where
         
        msgSeqNum :: Control.Lens.Lens' a MsgSeqNum
 
class MsgTypeLens a where
         
        msgType :: Control.Lens.Lens' a MsgType
 
class NewSeqNoLens a where
         
        newSeqNo :: Control.Lens.Lens' a NewSeqNo
 
class OrderIDLens a where
         
        orderID :: Control.Lens.Lens' a OrderID
 
class OrderQtyLens a where
         
        orderQty :: Control.Lens.Lens' a Quantity
 
class OrdStatusLens a where
         
        ordStatus :: Control.Lens.Lens' a OrdStatus
 
class OrdTypeLens a where
         
        ordType :: Control.Lens.Lens' a OrdType
 
class OrigClOrdIDLens a where
         
        origClOrdID :: Control.Lens.Lens' a OrigClOrdID
 
class PossDupFlagLens a where
         
        possDupFlag :: Control.Lens.Lens' a PossDupFlag
 
class PriceLens a where
         
        price :: Control.Lens.Lens' a Price
 
class RefSeqNumLens a where
         
        refSeqNum :: Control.Lens.Lens' a RefSeqNum
 
class Rule80ALens a where
         
        rule80A :: Control.Lens.Lens' a Rule80A
 
class SecurityIDLens a where
         
        securityID :: Control.Lens.Lens' a SecurityID
 
class SenderCompIDLens a where
         
        senderCompID :: Control.Lens.Lens' a SenderCompID
 
class SenderSubIDLens a where
         
        senderSubID :: Control.Lens.Lens' a SenderSubID
 
class SendingDateLens a where
         
        sendingDate :: Control.Lens.Lens' a SendingDate
 
class SendingTimeLens a where
         
        sendingTime :: Control.Lens.Lens' a SendingTime
 
class SideLens a where
         
        side :: Control.Lens.Lens' a Side
 
class SymbolLens a where
         
        symbol :: Control.Lens.Lens' a Symbol
 
class TargetCompIDLens a where
         
        targetCompID :: Control.Lens.Lens' a TargetCompID
 
class TargetSubIDLens a where
         
        targetSubID :: Control.Lens.Lens' a TargetSubID
 
class TextLens a where
         
        text :: Control.Lens.Lens' a Text
 
class TimeInForceLens a where
         
        timeInForce :: Control.Lens.Lens' a TimeInForce
 
class TransactTimeLens a where
         
        transactTime :: Control.Lens.Lens' a TransactTime
 
class UrgencyLens a where
         
        urgency :: Control.Lens.Lens' a Urgency
 
class SettlmntTypLens a where
         
        settlmntTyp :: Control.Lens.Lens' a SettlmntTyp
 
class OpenCloseLens a where
         
        openClose :: Control.Lens.Lens' a OpenClose
 
class PossResendLens a where
         
        possResend :: Control.Lens.Lens' a PossResend
 
class EncryptMethodLens a where
         
        encryptMethod :: Control.Lens.Lens' a EncryptMethod
 
class StopPxLens a where
         
        stopPx :: Control.Lens.Lens' a Price
 
class ExDestinationLens a where
         
        exDestination :: Control.Lens.Lens' a ExDestination
 
class CxlRejReasonLens a where
         
        cxlRejReason :: Control.Lens.Lens' a CxlRejReason
 
class OrdRejReasonLens a where
         
        ordRejReason :: Control.Lens.Lens' a OrdRejReason
 
class HeartBtIntLens a where
         
        heartBtInt :: Control.Lens.Lens' a HeartBtInt
 
class ClientIDLens a where
         
        clientID :: Control.Lens.Lens' a ClientID
 
class MaxFloorLens a where
         
        maxFloor :: Control.Lens.Lens' a Quantity
 
class TestReqIDLens a where
         
        testReqID :: Control.Lens.Lens' a TestReqID
 
class LocateReqdLens a where
         
        locateReqd :: Control.Lens.Lens' a LocateReqd
 
class OrigSendingTimeLens a where
         
        origSendingTime :: Control.Lens.Lens' a OrigSendingTime
 
class GapFillFlagLens a where
         
        gapFillFlag :: Control.Lens.Lens' a GapFillFlag
 
class ExpireTimeLens a where
         
        expireTime :: Control.Lens.Lens' a ExpireTime
 
class ResetSeqNumFlagLens a where
         
        resetSeqNumFlag :: Control.Lens.Lens' a ResetSeqNumFlag
 
class HeadlineLens a where
         
        headline :: Control.Lens.Lens' a Headline
 
class ExecTypeLens a where
         
        execType :: Control.Lens.Lens' a ExecType
 
class LeavesQtyLens a where
         
        leavesQty :: Control.Lens.Lens' a Quantity
 
class SecurityTypeLens a where
         
        securityType :: Control.Lens.Lens' a SecurityType
 
class EffectiveTimeLens a where
         
        effectiveTime :: Control.Lens.Lens' a EffectiveTime
 
class MaturityMonthYearLens a where
         
        maturityMonthYear :: Control.Lens.Lens' a MaturityMonthYear
 
class PutOrCallLens a where
         
        putOrCall :: Control.Lens.Lens' a PutOrCall
 
class StrikePriceLens a where
         
        strikePrice :: Control.Lens.Lens' a Price
 
class CustomerOrFirmLens a where
         
        customerOrFirm :: Control.Lens.Lens' a CustomerOrFirm
 
class MaturityDayLens a where
         
        maturityDay :: Control.Lens.Lens' a MaturityDay
 
class SecurityExchangeLens a where
         
        securityExchange :: Control.Lens.Lens' a SecurityExchange
 
class PegDifferenceLens a where
         
        pegDifference :: Control.Lens.Lens' a PegDifference
 
class ContractMultiplierLens a where
         
        contractMultiplier :: Control.Lens.Lens' a ContractMultiplier
 
class ExecRestatementReasonLens a where
         
        execRestatementReason :: Control.Lens.Lens' a ExecRestatementReason
 
class DiscretionInstLens a where
         
        discretionInst :: Control.Lens.Lens' a DiscretionInst
 
class DiscretionOffsetLens a where
         
        discretionOffset :: Control.Lens.Lens' a DiscretionOffset
 
class ExpireDateLens a where
         
        expireDate :: Control.Lens.Lens' a ExpireDate
 
class CxlRejResponseToLens a where
         
        cxlRejResponseTo :: Control.Lens.Lens' a CxlRejResponseTo
 
class ClearingFirmLens a where
         
        clearingFirm :: Control.Lens.Lens' a ClearingFirm
 
class ClearingAccountLens a where
         
        clearingAccount :: Control.Lens.Lens' a ClearingAccount
 
class WorkingIndicatorLens a where
         
        workingIndicator :: Control.Lens.Lens' a WorkingIndicator
 
class MPIDLens a where
         
        mPID :: Control.Lens.Lens' a MPID
 
class DailyNewIDLens a where
         
        dailyNewID :: Control.Lens.Lens' a DailyNewID
 
class AccountMaybeLens a where
         
        optAccount :: Control.Lens.Lens' a (Maybe Account)
 
class AvgPxMaybeLens a where
         
        optAvgPx :: Control.Lens.Lens' a (Maybe Price)
 
class BeginSeqNoMaybeLens a where
         
        optBeginSeqNo :: Control.Lens.Lens' a (Maybe BeginSeqNo)
 
class BeginStringMaybeLens a where
         
        optBeginString :: Control.Lens.Lens' a (Maybe BeginString)
 
class BodyLengthMaybeLens a where
         
        optBodyLength :: Control.Lens.Lens' a (Maybe BodyLength)
 
class CheckSumMaybeLens a where
         
        optCheckSum :: Control.Lens.Lens' a (Maybe CheckSum)
 
class ClOrdIDMaybeLens a where
         
        optClOrdID :: Control.Lens.Lens' a (Maybe ClOrdID)
 
class CumQtyMaybeLens a where
         
        optCumQty :: Control.Lens.Lens' a (Maybe Quantity)
 
class CurrencyMaybeLens a where
         
        optCurrency :: Control.Lens.Lens' a (Maybe Currency)
 
class EndSeqNoMaybeLens a where
         
        optEndSeqNo :: Control.Lens.Lens' a (Maybe EndSeqNo)
 
class ExecIDMaybeLens a where
         
        optExecID :: Control.Lens.Lens' a (Maybe ExecID)
 
class ExecInstMaybeLens a where
         
        optExecInst :: Control.Lens.Lens' a (Maybe ExecInst)
 
class ExecRefIDMaybeLens a where
         
        optExecRefID :: Control.Lens.Lens' a (Maybe ExecRefID)
 
class ExecTransTypeMaybeLens a where
         
        optExecTransType :: Control.Lens.Lens' a (Maybe ExecTransType)
 
class HandlInstMaybeLens a where
         
        optHandlInst :: Control.Lens.Lens' a (Maybe HandlInst)
 
class IDSourceMaybeLens a where
         
        optIDSource :: Control.Lens.Lens' a (Maybe IDSource)
 
class LastMktMaybeLens a where
         
        optLastMkt :: Control.Lens.Lens' a (Maybe LastMkt)
 
class LastPxMaybeLens a where
         
        optLastPx :: Control.Lens.Lens' a (Maybe Price)
 
class LastSharesMaybeLens a where
         
        optLastShares :: Control.Lens.Lens' a (Maybe Quantity)
 
class MsgSeqNumMaybeLens a where
         
        optMsgSeqNum :: Control.Lens.Lens' a (Maybe MsgSeqNum)
 
class MsgTypeMaybeLens a where
         
        optMsgType :: Control.Lens.Lens' a (Maybe MsgType)
 
class NewSeqNoMaybeLens a where
         
        optNewSeqNo :: Control.Lens.Lens' a (Maybe NewSeqNo)
 
class OrderIDMaybeLens a where
         
        optOrderID :: Control.Lens.Lens' a (Maybe OrderID)
 
class OrderQtyMaybeLens a where
         
        optOrderQty :: Control.Lens.Lens' a (Maybe Quantity)
 
class OrdStatusMaybeLens a where
         
        optOrdStatus :: Control.Lens.Lens' a (Maybe OrdStatus)
 
class OrdTypeMaybeLens a where
         
        optOrdType :: Control.Lens.Lens' a (Maybe OrdType)
 
class OrigClOrdIDMaybeLens a where
         
        optOrigClOrdID :: Control.Lens.Lens' a (Maybe OrigClOrdID)
 
class PossDupFlagMaybeLens a where
         
        optPossDupFlag :: Control.Lens.Lens' a (Maybe PossDupFlag)
 
class PriceMaybeLens a where
         
        optPrice :: Control.Lens.Lens' a (Maybe Price)
 
class RefSeqNumMaybeLens a where
         
        optRefSeqNum :: Control.Lens.Lens' a (Maybe RefSeqNum)
 
class Rule80AMaybeLens a where
         
        optRule80A :: Control.Lens.Lens' a (Maybe Rule80A)
 
class SecurityIDMaybeLens a where
         
        optSecurityID :: Control.Lens.Lens' a (Maybe SecurityID)
 
class SenderCompIDMaybeLens a where
         
        optSenderCompID :: Control.Lens.Lens' a (Maybe SenderCompID)
 
class SenderSubIDMaybeLens a where
         
        optSenderSubID :: Control.Lens.Lens' a (Maybe SenderSubID)
 
class SendingDateMaybeLens a where
         
        optSendingDate :: Control.Lens.Lens' a (Maybe SendingDate)
 
class SendingTimeMaybeLens a where
         
        optSendingTime :: Control.Lens.Lens' a (Maybe SendingTime)
 
class SideMaybeLens a where
         
        optSide :: Control.Lens.Lens' a (Maybe Side)
 
class SymbolMaybeLens a where
         
        optSymbol :: Control.Lens.Lens' a (Maybe Symbol)
 
class TargetCompIDMaybeLens a where
         
        optTargetCompID :: Control.Lens.Lens' a (Maybe TargetCompID)
 
class TargetSubIDMaybeLens a where
         
        optTargetSubID :: Control.Lens.Lens' a (Maybe TargetSubID)
 
class TextMaybeLens a where
         
        optText :: Control.Lens.Lens' a (Maybe Text)
 
class TimeInForceMaybeLens a where
         
        optTimeInForce :: Control.Lens.Lens' a (Maybe TimeInForce)
 
class TransactTimeMaybeLens a where
         
        optTransactTime :: Control.Lens.Lens' a (Maybe TransactTime)
 
class UrgencyMaybeLens a where
         
        optUrgency :: Control.Lens.Lens' a (Maybe Urgency)
 
class SettlmntTypMaybeLens a where
         
        optSettlmntTyp :: Control.Lens.Lens' a (Maybe SettlmntTyp)
 
class OpenCloseMaybeLens a where
         
        optOpenClose :: Control.Lens.Lens' a (Maybe OpenClose)
 
class PossResendMaybeLens a where
         
        optPossResend :: Control.Lens.Lens' a (Maybe PossResend)
 
class EncryptMethodMaybeLens a where
         
        optEncryptMethod :: Control.Lens.Lens' a (Maybe EncryptMethod)
 
class StopPxMaybeLens a where
         
        optStopPx :: Control.Lens.Lens' a (Maybe Price)
 
class ExDestinationMaybeLens a where
         
        optExDestination :: Control.Lens.Lens' a (Maybe ExDestination)
 
class CxlRejReasonMaybeLens a where
         
        optCxlRejReason :: Control.Lens.Lens' a (Maybe CxlRejReason)
 
class OrdRejReasonMaybeLens a where
         
        optOrdRejReason :: Control.Lens.Lens' a (Maybe OrdRejReason)
 
class HeartBtIntMaybeLens a where
         
        optHeartBtInt :: Control.Lens.Lens' a (Maybe HeartBtInt)
 
class ClientIDMaybeLens a where
         
        optClientID :: Control.Lens.Lens' a (Maybe ClientID)
 
class MaxFloorMaybeLens a where
         
        optMaxFloor :: Control.Lens.Lens' a (Maybe Quantity)
 
class TestReqIDMaybeLens a where
         
        optTestReqID :: Control.Lens.Lens' a (Maybe TestReqID)
 
class LocateReqdMaybeLens a where
         
        optLocateReqd :: Control.Lens.Lens' a (Maybe LocateReqd)
 
class OrigSendingTimeMaybeLens a where
         
        optOrigSendingTime :: Control.Lens.Lens' a (Maybe OrigSendingTime)
 
class GapFillFlagMaybeLens a where
         
        optGapFillFlag :: Control.Lens.Lens' a (Maybe GapFillFlag)
 
class ExpireTimeMaybeLens a where
         
        optExpireTime :: Control.Lens.Lens' a (Maybe ExpireTime)
 
class ResetSeqNumFlagMaybeLens a where
         
        optResetSeqNumFlag :: Control.Lens.Lens' a (Maybe ResetSeqNumFlag)
 
class HeadlineMaybeLens a where
         
        optHeadline :: Control.Lens.Lens' a (Maybe Headline)
 
class ExecTypeMaybeLens a where
         
        optExecType :: Control.Lens.Lens' a (Maybe ExecType)
 
class LeavesQtyMaybeLens a where
         
        optLeavesQty :: Control.Lens.Lens' a (Maybe Quantity)
 
class SecurityTypeMaybeLens a where
         
        optSecurityType :: Control.Lens.Lens' a (Maybe SecurityType)
 
class EffectiveTimeMaybeLens a where
         
        optEffectiveTime :: Control.Lens.Lens' a (Maybe EffectiveTime)
 
class MaturityMonthYearMaybeLens a where
         
        optMaturityMonthYear ::
                             Control.Lens.Lens' a (Maybe MaturityMonthYear)
 
class PutOrCallMaybeLens a where
         
        optPutOrCall :: Control.Lens.Lens' a (Maybe PutOrCall)
 
class StrikePriceMaybeLens a where
         
        optStrikePrice :: Control.Lens.Lens' a (Maybe Price)
 
class CustomerOrFirmMaybeLens a where
         
        optCustomerOrFirm :: Control.Lens.Lens' a (Maybe CustomerOrFirm)
 
class MaturityDayMaybeLens a where
         
        optMaturityDay :: Control.Lens.Lens' a (Maybe MaturityDay)
 
class SecurityExchangeMaybeLens a where
         
        optSecurityExchange ::
                            Control.Lens.Lens' a (Maybe SecurityExchange)
 
class PegDifferenceMaybeLens a where
         
        optPegDifference :: Control.Lens.Lens' a (Maybe PegDifference)
 
class ContractMultiplierMaybeLens a where
         
        optContractMultiplier ::
                              Control.Lens.Lens' a (Maybe ContractMultiplier)
 
class ExecRestatementReasonMaybeLens a where
         
        optExecRestatementReason ::
                                 Control.Lens.Lens' a (Maybe ExecRestatementReason)
 
class DiscretionInstMaybeLens a where
         
        optDiscretionInst :: Control.Lens.Lens' a (Maybe DiscretionInst)
 
class DiscretionOffsetMaybeLens a where
         
        optDiscretionOffset ::
                            Control.Lens.Lens' a (Maybe DiscretionOffset)
 
class ExpireDateMaybeLens a where
         
        optExpireDate :: Control.Lens.Lens' a (Maybe ExpireDate)
 
class CxlRejResponseToMaybeLens a where
         
        optCxlRejResponseTo ::
                            Control.Lens.Lens' a (Maybe CxlRejResponseTo)
 
class ClearingFirmMaybeLens a where
         
        optClearingFirm :: Control.Lens.Lens' a (Maybe ClearingFirm)
 
class ClearingAccountMaybeLens a where
         
        optClearingAccount :: Control.Lens.Lens' a (Maybe ClearingAccount)
 
class WorkingIndicatorMaybeLens a where
         
        optWorkingIndicator ::
                            Control.Lens.Lens' a (Maybe WorkingIndicator)
 
class MPIDMaybeLens a where
         
        optMPID :: Control.Lens.Lens' a (Maybe MPID)
 
class DailyNewIDMaybeLens a where
         
        optDailyNewID :: Control.Lens.Lens' a (Maybe DailyNewID)