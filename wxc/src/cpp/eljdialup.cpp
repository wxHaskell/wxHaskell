#include "wxc/wrapper.h"
#include "wx/dialup.h"

extern "C"
{

EWXWEXPORT(wxDialUpManager*,wxDialUpManager_Create)()
{
	return wxDialUpManager::Create();
}

EWXWEXPORT(void,wxDialUpManager_Delete)(wxDialUpManager* self)
{
	delete self;
}

EWXWEXPORT(bool,wxDialUpManager_IsOk)(wxDialUpManager* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(int,wxDialUpManager_GetISPNames)(wxDialUpManager* self,void* _lst)
{
	wxArrayString arr;
	self->GetISPNames(arr);

	if (_lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((wxChar**)_lst)[i] = wxStrdup(arr.Item(i).c_str());
	}

	return arr.GetCount();
}
	
EWXWEXPORT(bool,wxDialUpManager_Dial)(wxDialUpManager* self,wxString* nameOfISP,wxString* username,wxString* password,bool async)
{
	return self->Dial(*nameOfISP,*username,*password, async);
}
	
EWXWEXPORT(bool,wxDialUpManager_IsDialing)(wxDialUpManager* self)
{
	return self->IsDialing();
}
	
EWXWEXPORT(bool,wxDialUpManager_CancelDialing)(wxDialUpManager* self)
{
	return self->CancelDialing();
}
	
EWXWEXPORT(bool,wxDialUpManager_HangUp)(wxDialUpManager* self)
{
	return self->HangUp();
}
	
EWXWEXPORT(bool,wxDialUpManager_IsAlwaysOnline)(wxDialUpManager* self)
{
	return self->IsAlwaysOnline();
}
	
EWXWEXPORT(bool,wxDialUpManager_IsOnline)(wxDialUpManager* self)
{
	return self->IsOnline();
}
	
EWXWEXPORT(void,wxDialUpManager_SetOnlineStatus)(wxDialUpManager* self,bool isOnline)
{
	self->SetOnlineStatus(isOnline);
}
	
EWXWEXPORT(bool,wxDialUpManager_EnableAutoCheckOnlineStatus)(wxDialUpManager* self,size_t nSeconds)
{
	return self->EnableAutoCheckOnlineStatus(nSeconds);
}
	
EWXWEXPORT(void,wxDialUpManager_DisableAutoCheckOnlineStatus)(wxDialUpManager* self)
{
	self->DisableAutoCheckOnlineStatus();
}
	
EWXWEXPORT(void,wxDialUpManager_SetWellKnownHost)(wxDialUpManager* self,wxString* hostname,int portno)
{
	self->SetWellKnownHost(*hostname, portno);
}
	
EWXWEXPORT(void,wxDialUpManager_SetConnectCommand)(wxDialUpManager* self,wxString* commandDial,wxString* commandHangup)
{
	self->SetConnectCommand(*commandDial,*commandHangup);
}
	

EWXWEXPORT(bool,wxDialUpEvent_IsConnectedEvent)(wxDialUpEvent* self)
{
	return self->IsConnectedEvent();
}
	
EWXWEXPORT(bool,wxDialUpEvent_IsOwnEvent)(wxDialUpEvent* self)
{
	return self->IsOwnEvent();
}

}
