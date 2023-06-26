/* update checked 100% with 2.8.9 manual */
#include "wxc/wrapper.h"

extern "C"
{
#if wxUSE_SOCKETS
/* wxSocket errors */
EWXWCONSTANTINT(wxSOCKET_NOERROR,wxSOCKET_NOERROR)
EWXWCONSTANTINT(wxSOCKET_INVOP,wxSOCKET_INVOP)
EWXWCONSTANTINT(wxSOCKET_IOERR,wxSOCKET_IOERR)
EWXWCONSTANTINT(wxSOCKET_INVADDR,wxSOCKET_INVADDR)
EWXWCONSTANTINT(wxSOCKET_INVSOCK,wxSOCKET_INVSOCK)
EWXWCONSTANTINT(wxSOCKET_NOHOST,wxSOCKET_NOHOST)
EWXWCONSTANTINT(wxSOCKET_INVPORT,wxSOCKET_INVPORT)
EWXWCONSTANTINT(wxSOCKET_WOULDBLOCK,wxSOCKET_WOULDBLOCK)
EWXWCONSTANTINT(wxSOCKET_TIMEDOUT,wxSOCKET_TIMEDOUT)
EWXWCONSTANTINT(wxSOCKET_MEMERR,wxSOCKET_MEMERR)
/* wxSocket events */
EWXWCONSTANTINT(wxSOCKET_INPUT,wxSOCKET_INPUT)
EWXWCONSTANTINT(wxSOCKET_OUTPUT,wxSOCKET_OUTPUT)
EWXWCONSTANTINT(wxSOCKET_CONNECTION,wxSOCKET_CONNECTION)
EWXWCONSTANTINT(wxSOCKET_LOST,wxSOCKET_LOST)
/* wxSocketFlags */
EWXWCONSTANTINT(wxSOCKET_NONE,wxSOCKET_NONE)
EWXWCONSTANTINT(wxSOCKET_NOWAIT,wxSOCKET_NOWAIT)
EWXWCONSTANTINT(wxSOCKET_WAITALL,wxSOCKET_WAITALL)
EWXWCONSTANTINT(wxSOCKET_BLOCK,wxSOCKET_BLOCK)
EWXWCONSTANTINT(wxSOCKET_REUSEADDR,wxSOCKET_REUSEADDR)
/* wxSocketEventFlags */
EWXWCONSTANTINT(wxSOCKET_INPUT_FLAG,wxSOCKET_INPUT_FLAG)
EWXWCONSTANTINT(wxSOCKET_OUTPUT_FLAG,wxSOCKET_OUTPUT_FLAG)
EWXWCONSTANTINT(wxSOCKET_CONNECTION_FLAG,wxSOCKET_CONNECTION_FLAG)
EWXWCONSTANTINT(wxSOCKET_LOST_FLAG,wxSOCKET_LOST_FLAG)

/* wxSocketBase */
/* wxSocketBase::wxSocketBase */
EWXWEXPORT(void,wxSocketBase_Delete)(wxSocketBase* self)
{
  delete self;
}
EWXWEXPORT(void,wxSocketBase_Close)(wxSocketBase* self)
{
  self->Close();
}
EWXWEXPORT(bool,wxSocketBase_Destroy)(wxSocketBase* self)
{
  return self->Destroy();
}
EWXWEXPORT(wxSocketBase*,wxSocketBase_Discard)(wxSocketBase* self)
{
  return &self->Discard();
}
EWXWEXPORT(bool,wxSocketBase_Error)(wxSocketBase* self)
{
  return self->Error();
}
EWXWEXPORT(void*,wxSocketBase_GetClientData)(wxSocketBase* self)
{
  return self->GetClientData();
}
EWXWEXPORT(bool,wxSocketBase_GetLocal)(wxSocketBase* self,wxSockAddress* addr)
{
  return self->GetLocal(*addr);
}
/*note gsocket = int */
EWXWEXPORT(int,wxSocketBase_GetFlags)(wxSocketBase* self)
{
  return self->GetFlags();
}
EWXWEXPORT(bool,wxSocketBase_GetPeer)(wxSocketBase* self,wxSockAddress* addr)
{
  return self->GetPeer(*addr);
}
EWXWEXPORT(void,wxSocketBase_InterruptWait)(wxSocketBase* self)
{
  self->InterruptWait();
}
EWXWEXPORT(bool,wxSocketBase_IsConnected)(wxSocketBase* self)
{
  return self->IsConnected();
}
EWXWEXPORT(bool,wxSocketBase_IsData)(wxSocketBase* self)
{
  return self->IsData();
}
EWXWEXPORT(bool,wxSocketBase_IsDisconnected)(wxSocketBase* self)
{
  return self->IsDisconnected();
}
EWXWEXPORT(wxUint32,wxSocketBase_LastCount)(wxSocketBase* self)
{
  return self->LastCount();
}
EWXWEXPORT(int,wxSocketBase_LastError)(wxSocketBase* self)
{
  return self->LastError();
}
EWXWEXPORT(void,wxSocketBase_Notify)(wxSocketBase* self,bool notify)
{
  self->Notify(notify);
}
EWXWEXPORT(bool,wxSocketBase_IsOk)(wxSocketBase* self)
{
  return self->IsOk();
}
EWXWEXPORT(void,wxSocketBase_RestoreState)(wxSocketBase* self)
{
  self->RestoreState();
}
EWXWEXPORT(void,wxSocketBase_SaveState)(wxSocketBase* self)
{
  self->SaveState();
}
EWXWEXPORT(void,wxSocketBase_SetClientData)(wxSocketBase* self,void* data)
{
  self->SetClientData(data);
}
EWXWEXPORT(void,wxSocketBase_SetEventHandler)(wxSocketBase* self,wxEvtHandler* handler,int id)
{
  self->SetEventHandler(*handler,id);
}
EWXWEXPORT(void,wxSocketBase_SetFlags)(wxSocketBase* self,int flags)
{
  self->SetFlags(flags);
}
EWXWEXPORT(bool,wxSocketBase_SetLocal)(wxSocketBase* self,wxIPV4address* local)
{
  return self->SetLocal(*local);
}
EWXWEXPORT(void,wxSocketBase_SetNotify)(wxSocketBase* self,int flags)
{
  self->SetNotify(flags);
}
EWXWEXPORT(void,wxSocketBase_SetTimeout)(wxSocketBase* self,int seconds)
{
  self->SetTimeout(seconds);
}
EWXWEXPORT(wxSocketBase*,wxSocketBase_Peek)(wxSocketBase* self,void* buffer,wxUint32 nbytes)
{
  return &self->Peek(buffer,nbytes);
}
EWXWEXPORT(wxSocketBase*,wxSocketBase_Read)(wxSocketBase* self,void* buffer,wxUint32 nbytes)
{
  return &self->Read(buffer,nbytes);
}
EWXWEXPORT(wxSocketBase*,wxSocketBase_ReadMsg)(wxSocketBase* self,void* buffer,wxUint32 nbytes)
{
  return &self->ReadMsg(buffer,nbytes);
}
EWXWEXPORT(wxSocketBase*,wxSocketBase_Unread)(wxSocketBase* self,void* buffer,wxUint32 nbytes)
{
  return &self->Unread(buffer,nbytes);
}
EWXWEXPORT(bool,wxSocketBase_Wait)(wxSocketBase* self,long seconds,long millisecond)
{
  return self->Wait(seconds,millisecond);
}
EWXWEXPORT(bool,wxSocketBase_WaitForLost)(wxSocketBase* self,long seconds,long millisecond)
{
  return self->WaitForLost(seconds,millisecond);
}
EWXWEXPORT(bool,wxSocketBase_WaitForRead)(wxSocketBase* self,long seconds,long millisecond)
{
  return self->WaitForRead(seconds,millisecond);
}
EWXWEXPORT(bool,wxSocketBase_WaitForWrite)(wxSocketBase* self,long seconds,long millisecond)
{
  return self->WaitForWrite(seconds,millisecond);
}
EWXWEXPORT(wxSocketBase*,wxSocketBase_Write)(wxSocketBase* self,void* buffer,wxUint32 nbytes)
{
  return &self->Write(buffer,nbytes);
}
EWXWEXPORT(wxSocketBase*,wxSocketBase_WriteMsg)(wxSocketBase* self,void* buffer,wxUint32 nbytes)
{
  return &self->WriteMsg(buffer,nbytes);
}
/* wxSocketEvent */
EWXWEXPORT(wxSocketEvent*,wxSocketEvent_Create)(int id)
{
  return new wxSocketEvent(id);
}
EWXWEXPORT(void,wxSocketEvent_Delete)(wxSocketEvent* self)
{
  delete self;
}
EWXWEXPORT(void*,wxSocketEvent_GetClientData)(wxSocketEvent* self)
{
  return self->GetClientData();
}
EWXWEXPORT(wxSocketBase*,wxSocketEvent_GetSocket)(wxSocketEvent* self)
{
  return self->GetSocket();
}
EWXWEXPORT(int,wxSocketEvent_GetSocketEvent)(wxSocketEvent* self)
{
  return self->GetSocketEvent();
}
/* wxSocketClient */
EWXWEXPORT(wxSocketClient*,wxSocketClient_Create)(int flags)
{
  return new wxSocketClient(flags);
}
EWXWEXPORT(void,wxSocketClient_Delete)(wxSocketClient* self)
{
  delete self;
}
EWXWEXPORT(bool,wxSocketClient_Connect)(wxSocketClient* self,wxSockAddress* address,bool wait)
{
  return self->Connect(*address,wait);
}
EWXWEXPORT(bool,wxSocketClient_ConnectLocal)(wxSocketClient* self,wxSockAddress* address,wxSockAddress* local,bool wait)
{
  return self->Connect(*address,*local,wait);
}
EWXWEXPORT(bool,wxSocketClient_WaitOnConnect)(wxSocketClient* self,long seconds,long millisecond)
{
  return self->WaitOnConnect(seconds,millisecond);
}

/* wxSocketServer */
EWXWEXPORT(wxSocketServer*,wxSocketServer_Create)(wxSockAddress* address,int flags)
{
  return new wxSocketServer(*address,flags);
}
EWXWEXPORT(void,wxSocketServer_Delete)(wxSocketServer* self)
{
  delete self;
}
EWXWEXPORT(wxSocketBase*,wxSocketServer_Accept)(wxSocketServer* self,bool wait)
{
  return self->Accept(wait);
}
EWXWEXPORT(bool,wxSocketServer_AcceptWith)(wxSocketServer* self,wxSocketBase* socket,bool wait)
{
  return self->AcceptWith(*socket,wait);
}
EWXWEXPORT(bool,wxSocketServer_WaitForAccept)(wxSocketServer* self,long seconds,long millisecond)
{
  return self->WaitForAccept(seconds,millisecond);
}

#endif /* wxUSE_SOCKETS */
}
