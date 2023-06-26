#include "wxc/wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/cshelp.h"

extern "C"
{

EWXWEXPORT(wxContextHelp*,wxContextHelp_Create)(wxWindow* win,bool beginHelp)
{
	return new wxContextHelp(win, beginHelp);
}
	
EWXWEXPORT(void,wxContextHelp_Delete)(wxContextHelp* self)
{
	delete self;
}
	
EWXWEXPORT(bool,wxContextHelp_BeginContextHelp)(wxContextHelp* self,wxWindow* win)
{
	return self->BeginContextHelp(win);
}
	
EWXWEXPORT(bool,wxContextHelp_EndContextHelp)(wxContextHelp* self)
{
	return self->EndContextHelp();
}


EWXWEXPORT(wxContextHelpButton*,wxContextHelpButton_Create)(wxWindow* parent,int id,int x,int y,int w,int h,long style)
{
	return new wxContextHelpButton(parent, (wxWindowID)id,wxPoint(x, y), wxSize(w, h), style);
}


EWXWEXPORT(wxHelpProvider*,wxHelpProvider_Get)()
{
	return wxHelpProvider::Get();
}
	
EWXWEXPORT(wxHelpProvider*,wxHelpProvider_Set)(wxHelpProvider* helpProvider)
{
	return wxHelpProvider::Set(helpProvider);
}
	
EWXWEXPORT(wxString*,wxHelpProvider_GetHelp)(void* _obj,void* window)
{
	wxString *result = new wxString();
	*result = ((wxHelpProvider*)_obj)->GetHelp((wxWindowBase*)window);
	return result;
}
	
EWXWEXPORT(bool,wxHelpProvider_ShowHelp)(wxHelpProvider* self,wxWindowBase* window)
{
	return self->ShowHelp(window);
}
	
EWXWEXPORT(void,wxHelpProvider_AddHelp)(wxHelpProvider* self,wxWindowBase* window,wxString* text)
{
	self->AddHelp(window,*text);
}
	
EWXWEXPORT(void,wxHelpProvider_AddHelpById)(wxHelpProvider* self,int id,wxString* text)
{
	self->AddHelp((wxWindowID)id,*text);
}
	
EWXWEXPORT(void,wxHelpProvider_RemoveHelp)(wxHelpProvider* self,wxWindowBase* window)
{
	self->RemoveHelp(window);
}
	
EWXWEXPORT(void,wxHelpProvider_Delete)(wxHelpProvider* self)
{
	delete self;
}


EWXWEXPORT(wxSimpleHelpProvider*,wxSimpleHelpProvider_Create)()
{
	return new wxSimpleHelpProvider();
}
	

EWXWEXPORT(wxHelpControllerHelpProvider*,wxHelpControllerHelpProvider_Create)(wxHelpControllerBase* ctr)
{
	return new wxHelpControllerHelpProvider(ctr);
}
	
EWXWEXPORT(void,wxHelpControllerHelpProvider_SetHelpController)(wxHelpControllerHelpProvider* self,wxHelpControllerBase* hc)
{
	self->SetHelpController(hc);
}
	
EWXWEXPORT(wxHelpControllerBase*,wxHelpControllerHelpProvider_GetHelpController)(wxHelpControllerHelpProvider* self)
{
	return self->GetHelpController();
}
	
}
#endif
