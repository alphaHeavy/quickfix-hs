{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module AlphaHeavy.FIX.FIX42.Factory where
import qualified Data.ByteString
import AlphaHeavy.FIX
import AlphaHeavy.FIX.FIX42.Types
 
logon :: EncryptMethod -> HeartBtInt -> Logon
logon encryptMethod heartBtInt
  = Logon (AlphaHeavy.FIX.Enumeration encryptMethod)
      (AlphaHeavy.FIX.Field heartBtInt)
      Nothing
 
heartbeat :: Heartbeat
heartbeat = Heartbeat Nothing
 
testRequest :: TestReqID -> TestRequest
testRequest testReqID
  = TestRequest (AlphaHeavy.FIX.Field testReqID)
 
resendRequest :: BeginSeqNo -> EndSeqNo -> ResendRequest
resendRequest beginSeqNo endSeqNo
  = ResendRequest (AlphaHeavy.FIX.Field beginSeqNo)
      (AlphaHeavy.FIX.Field endSeqNo)
 
sequenceReset :: NewSeqNo -> SequenceReset
sequenceReset newSeqNo
  = SequenceReset (AlphaHeavy.FIX.Field newSeqNo) Nothing
 
reject :: RefSeqNum -> Reject
reject refSeqNum = Reject (AlphaHeavy.FIX.Field refSeqNum) Nothing
 
logout :: Logout
logout = Logout Nothing
 
newOrderSingle ::
               ClOrdID ->
                 CustomerOrFirm ->
                   ExDestination ->
                     HandlInst ->
                       LocateReqd ->
                         MPID -> OrdType -> Quantity -> Side -> Symbol -> NewOrderSingle
newOrderSingle clOrdID customerOrFirm exDestination handlInst
  locateReqd mPID ordType orderQty side symbol
  = NewOrderSingle (AlphaHeavy.FIX.Field clOrdID)
      (AlphaHeavy.FIX.Enumeration customerOrFirm)
      (AlphaHeavy.FIX.Field exDestination)
      (AlphaHeavy.FIX.Enumeration handlInst)
      (AlphaHeavy.FIX.Enumeration locateReqd)
      (AlphaHeavy.FIX.Field mPID)
      (AlphaHeavy.FIX.Enumeration ordType)
      (AlphaHeavy.FIX.Field orderQty)
      (AlphaHeavy.FIX.Enumeration side)
      (AlphaHeavy.FIX.Field symbol)
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
 
executionReport ::
                Price ->
                  Quantity ->
                    ExecID ->
                      ExecTransType ->
                        ExecType ->
                          Price ->
                            Quantity ->
                              Quantity ->
                                OrdStatus ->
                                  OrderID -> Quantity -> Side -> Symbol -> ExecutionReport
executionReport avgPx cumQty execID execTransType execType lastPx
  lastShares leavesQty ordStatus orderID orderQty side symbol
  = ExecutionReport (AlphaHeavy.FIX.Field avgPx)
      (AlphaHeavy.FIX.Field cumQty)
      (AlphaHeavy.FIX.Field execID)
      (AlphaHeavy.FIX.Enumeration execTransType)
      (AlphaHeavy.FIX.Enumeration execType)
      (AlphaHeavy.FIX.Field lastPx)
      (AlphaHeavy.FIX.Field lastShares)
      (AlphaHeavy.FIX.Field leavesQty)
      (AlphaHeavy.FIX.Enumeration ordStatus)
      (AlphaHeavy.FIX.Field orderID)
      (AlphaHeavy.FIX.Field orderQty)
      (AlphaHeavy.FIX.Enumeration side)
      (AlphaHeavy.FIX.Field symbol)
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
 
orderCancelReplaceRequest ::
                          ClOrdID ->
                            HandlInst ->
                              OrdType ->
                                Quantity ->
                                  OrigClOrdID -> Side -> Symbol -> OrderCancelReplaceRequest
orderCancelReplaceRequest clOrdID handlInst ordType orderQty
  origClOrdID side symbol
  = OrderCancelReplaceRequest (AlphaHeavy.FIX.Field clOrdID)
      (AlphaHeavy.FIX.Enumeration handlInst)
      (AlphaHeavy.FIX.Enumeration ordType)
      (AlphaHeavy.FIX.Field orderQty)
      (AlphaHeavy.FIX.Field origClOrdID)
      (AlphaHeavy.FIX.Enumeration side)
      (AlphaHeavy.FIX.Field symbol)
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
      Nothing
 
orderCancelRequest ::
                   ClOrdID ->
                     Quantity -> OrigClOrdID -> Side -> Symbol -> OrderCancelRequest
orderCancelRequest clOrdID orderQty origClOrdID side symbol
  = OrderCancelRequest (AlphaHeavy.FIX.Field clOrdID)
      (AlphaHeavy.FIX.Field orderQty)
      (AlphaHeavy.FIX.Field origClOrdID)
      (AlphaHeavy.FIX.Enumeration side)
      (AlphaHeavy.FIX.Field symbol)
      Nothing
      Nothing
      Nothing
 
orderCancelReject ::
                  ClOrdID ->
                    CxlRejResponseTo -> OrderID -> OrigClOrdID -> OrderCancelReject
orderCancelReject clOrdID cxlRejResponseTo orderID origClOrdID
  = OrderCancelReject (AlphaHeavy.FIX.Field clOrdID)
      (AlphaHeavy.FIX.Enumeration cxlRejResponseTo)
      (AlphaHeavy.FIX.Field orderID)
      (AlphaHeavy.FIX.Field origClOrdID)
      Nothing
      Nothing
 
orderStatusRequest :: ClOrdID -> OrderStatusRequest
orderStatusRequest clOrdID
  = OrderStatusRequest (AlphaHeavy.FIX.Field clOrdID)
 
news :: DailyNewID -> Headline -> Urgency -> News
news dailyNewID headline urgency
  = News (AlphaHeavy.FIX.Field dailyNewID)
      (AlphaHeavy.FIX.Field headline)
      (AlphaHeavy.FIX.Enumeration urgency)
      Nothing