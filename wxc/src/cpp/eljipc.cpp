#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,ELJConnection_CreateDefault)()
{
	return new ELJConnection();
}

EWXWEXPORT(void*,ELJConnection_Create)(wxChar* buffer,int size)
{
	return new ELJConnection(buffer, size);
}

EWXWEXPORT(void,ELJConnection_Delete)(void* self)
{
	delete (ELJConnection*)self;
}

EWXWEXPORT(bool,ELJConnection_Execute)(ELJConnection* self,wxString* data,int size,int format)
{
	return self->Execute(*data, size, (wxIPCFormat)format);
}
	
EWXWEXPORT(void*,ELJConnection_Request)(void* self,wxString* item,void* size,int format)
{
	return (void*)((ELJConnection*)self)->Request(*item, (size_t*)size, (wxIPCFormat)format);
}
	
EWXWEXPORT(bool,ELJConnection_Poke)(ELJConnection* self,wxString* item,wxChar* data,int size,int format)
{
	return self->Poke(*item, data, size, (wxIPCFormat)format);
}
	
EWXWEXPORT(bool,ELJConnection_StartAdvise)(ELJConnection* self,wxString* item)
{
	return self->StartAdvise(*item);
}
	
EWXWEXPORT(bool,ELJConnection_StopAdvise)(ELJConnection* self,wxString* item)
{
	return self->StopAdvise(*item);
}
	
EWXWEXPORT(bool,ELJConnection_Advise)(ELJConnection* self,wxString* item,wxChar* data,int size,int format)
{
	return self->Advise(*item, data, size, (wxIPCFormat)format);
}
	
EWXWEXPORT(bool,ELJConnection_Disconnect)(ELJConnection* self)
{
	return self->Disconnect();
}
	
EWXWEXPORT(void,ELJConnection_Compress)(void* self,bool on)
{
	((ELJConnection*)self)->Compress(on);
}
	
EWXWEXPORT(void,ELJConnection_SetOnAdvise)(void* self,void* _fnc)
{
	((ELJConnection*)self)->SetOnAdvise(_fnc);
}
	
EWXWEXPORT(void,ELJConnection_SetOnExecute)(void* self,void* _fnc)
{
	((ELJConnection*)self)->SetOnExecute(_fnc);
}
	
EWXWEXPORT(void,ELJConnection_SetOnRequest)(void* self,void* _fnc)
{
	((ELJConnection*)self)->SetOnRequest(_fnc);
}
	
EWXWEXPORT(void,ELJConnection_SetOnPoke)(void* self,void* _fnc)
{
	((ELJConnection*)self)->SetOnPoke(_fnc);
}
	
EWXWEXPORT(void,ELJConnection_SetOnStartAdvise)(void* self,void* _fnc)
{
	((ELJConnection*)self)->SetOnStartAdvise(_fnc);
}
	
EWXWEXPORT(void,ELJConnection_SetOnStopAdvise)(void* self,void* _fnc)
{
	((ELJConnection*)self)->SetOnStopAdvise(_fnc);
}

EWXWEXPORT(void,ELJConnection_SetOnDisconnect)(void* self,void* _fnc)
{
	((ELJConnection*)self)->SetOnDisconnect(_fnc);
}

EWXWEXPORT(void*,ELJServer_Create)(void* _eobj,void* _cnct)
{
	return new ELJServer(_eobj, _cnct);
}

EWXWEXPORT(void,ELJServer_Delete)(void* self)
{
	delete (ELJServer*)self;
}

EWXWEXPORT(int,ELJServer_Initialize)(void* self,wxString* name)
{
	return ((ELJServer*)self)->Create(*name);
}
	
EWXWEXPORT(void*,ELJClient_Create)(void* _eobj,void* _cnct)
{
	return new ELJClient(_eobj, _cnct);
}

EWXWEXPORT(void,ELJClient_Delete)(void* self)
{
	delete (ELJClient*)self;
}

EWXWEXPORT(void,ELJClient_MakeConnection)(void* self,wxString* host,wxString* server,wxString* topic)
{
	((ELJClient*)self)->MakeConnection(*host,*server,*topic);
}
	
}
