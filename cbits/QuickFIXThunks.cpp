#include <quickfix/Application.h>
#include <quickfix/FileStore.h>
#include <quickfix/FileLog.h>
#include <quickfix/Log.h>
#include <quickfix/Message.h>
#include <quickfix/fix42/Message.h>
#include <quickfix/SessionSettings.h>
#include <quickfix/SocketInitiator.h>
#include <quickfix/SocketAcceptor.h>
#include <HsFFI.h>
#include "AlphaHeavy/QuickFIX/Foreign_stub.h"

class ApplicationThunk :
    public FIX::Application
{
public:
    ApplicationThunk(HsStablePtr app)
        : _app(app)
    {
    }

    virtual void onCreate(const FIX::SessionID& sess)
    {
        applicationOnCreate(_app, reinterpret_cast<HsInt>(&sess));
    }

    virtual void onLogon(const FIX::SessionID& sess)
    {
        applicationOnLogon(_app, reinterpret_cast<HsInt>(&sess));
    }

    virtual void onLogout(const FIX::SessionID& sess)
    {
        applicationOnLogout(_app, reinterpret_cast<HsInt>(&sess));
    }

    virtual void toAdmin(FIX::Message& msg, const FIX::SessionID& sess)
    {
        applicationToAdmin(_app, reinterpret_cast<HsInt>(&sess), &msg);
    }

    virtual void toApp(FIX::Message& msg, const FIX::SessionID& sess)
        throw (FIX::DoNotSend)
    {
        applicationToApp(_app, reinterpret_cast<HsInt>(&sess), &msg);
    }

    virtual void fromAdmin(const FIX::Message& msg, const FIX::SessionID& sess)
        throw (FIX::FieldNotFound, FIX::IncorrectDataFormat, FIX::IncorrectTagValue, FIX::RejectLogon)
    {
        applicationFromAdmin(_app, reinterpret_cast<HsInt>(&sess), const_cast<FIX::Message*>(&msg));
    }

    virtual void fromApp(const FIX::Message& msg, const FIX::SessionID& sess)
        throw (FIX::FieldNotFound, FIX::IncorrectDataFormat, FIX::IncorrectTagValue, FIX::UnsupportedMessageType)
    {
        applicationFromApp(_app, reinterpret_cast<HsInt>(&sess), const_cast<FIX::Message*>(&msg));
    }

private:
    HsStablePtr _app;
};

extern "C"
HsChar getMessageType(const FIX::Message& msg)
{
    FIX::MsgType msgType;
    msg.getHeader().getField(msgType);
    return msgType.getValue()[0];
}

extern "C"
HsBool getBoolField(const FIX::Message& msg, int fieldId)
{
    FIX::BoolField field(fieldId);
    msg.getField(field);
    return field.getValue();
}

extern "C"
HsChar getCharField(const FIX::Message& msg, int fieldId)
{
    FIX::CharField field(fieldId);
    msg.getField(field);
    return field.getValue();
}

extern "C"
HsDouble getDoubleField(const FIX::Message& msg, int fieldId)
{
    FIX::DoubleField field(fieldId);
    msg.getField(field);
    return field.getValue();
}

extern "C"
HsInt32 getIntField(const FIX::Message& msg, int fieldId)
{
    FIX::IntField field(fieldId);
    msg.getField(field);
    return field.getValue();
}

extern "C"
void getStringField(const FIX::Message& msg, int fieldId, void (*cont)(const char*))
{
    FIX::StringField field(fieldId);
    msg.getField(field);
    cont(field.getValue().c_str());
}

extern "C"
HsBool isFieldSet(const FIX::Message& msg, int fieldId)
{
    return msg.isSetField(fieldId);
}

extern "C"
char* decodeMessageWith(
    const char* messageStr,
    void (*cont)(FIX::Message&))
{
    try
    {
        const std::string& str(messageStr);
        FIX::Message msg(str, false);
        cont(msg);
        return NULL;
    }
    catch (const std::exception& e)
    {
        return strdup(e.what());
    }
}

extern "C"
char* sendMessageWith(
    const char* senderCompStr,
    const char* targetCompStr,
    HsChar msgTypeChar,
    void (*cont)(FIX::Message&))
{
    try
    {
        const char msgTypeStr[2] = { msgTypeChar, '\0' };
        FIX::MsgType msgtype(msgTypeStr);
        FIX42::Message msg(msgtype);
        cont(msg);

        const FIX::SenderCompID senderCompID(senderCompStr);
        const FIX::TargetCompID targetCompID(targetCompStr);

        FIX::Session::sendToTarget(msg, senderCompID, targetCompID);
        return NULL;
    }
    catch (const std::exception& e)
    {
        return strdup(e.what());
    }
}

extern "C"
void setBoolField(FIX::Message& msg, int fieldId, HsBool value)
{
    FIX::BoolField field(fieldId, value);
    msg.setField(field);
}

extern "C"
void setCharField(FIX::Message& msg, int fieldId, HsChar value)
{
    FIX::CharField field(fieldId, value);
    msg.setField(field);
}

extern "C"
void setDoubleField(FIX::Message& msg, int fieldId, HsDouble value)
{
    FIX::DoubleField field(fieldId, value);
    msg.setField(field);
}

extern "C"
void setIntField(FIX::Message& msg, int fieldId, HsInt32 value)
{
    FIX::IntField field(fieldId, value);
    msg.setField(field);
}

extern "C"
void setStringField(FIX::Message& msg, int fieldId, const char* value)
{
    FIX::StringField field(fieldId, value);
    msg.setField(field);
}

template <class T>
char* runSocketApp(HsStablePtr app, const char* configPath)
{
    try
    {
        const std::string configPathStr(configPath);
        FIX::SessionSettings settings(configPathStr);
        ApplicationThunk thunk(app);
        FIX::FileStoreFactory storeFactory(settings);
        // FIX::ScreenLogFactory logFactory(settings);
        FIX::FileLogFactory logFactory(settings);
        T initiator(thunk, storeFactory, settings, logFactory);
        initiator.start();
        applicationBlock(app);
        initiator.stop();
        return NULL;
    }
    catch (const std::exception& e)
    {
        return strdup(e.what());
    }
}

extern "C"
char* runApplication(HsStablePtr app, const char* configPath)
{
    return runSocketApp<FIX::SocketInitiator>(app, configPath);
}

extern "C"
char* runAcceptor(HsStablePtr app, const char* configPath)
{
    return runSocketApp<FIX::SocketAcceptor>(app, configPath);
}

extern "C"
char* sessionLogon(const FIX::SessionID& sid)
{
    FIX::Session* session = FIX::Session::lookupSession(sid);
    if (session == NULL)
    {
        char* res;
        asprintf(&res, "Logon for session %s failed", sid.toStringFrozen().c_str());
        return res;
    }

    session->logon();
    return NULL;
}

extern "C"
char* sessionLogout(const FIX::SessionID& sid, const char* reason)
{
    FIX::Session* session = FIX::Session::lookupSession(sid);
    if (session == NULL)
    {
        char* res;
        asprintf(&res, "Logout for session %s failed", sid.toStringFrozen().c_str());
        return res;
    }

    session->logout(reason ? reason : "");
    return NULL;
}

extern "C"
char* sessionDisconnect(const FIX::SessionID& sid)
{
    FIX::Session* session = FIX::Session::lookupSession(sid);
    if (session == NULL)
    {
        char* res;
        asprintf(&res, "Disconnect for session %s failed", sid.toStringFrozen().c_str());
        return res;
    }

    session->disconnect();
    return NULL;
}

extern "C"
char* sessionString(const FIX::SessionID& sid)
{
    return strdup(sid.toStringFrozen().c_str());
}
