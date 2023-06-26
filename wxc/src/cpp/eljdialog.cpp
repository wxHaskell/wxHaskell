#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxDialog*,wxDialog_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return new wxDialog (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(bool,wxDialog_IsModal)(wxDialog* self)
{
	return self->IsModal();
}
	
EWXWEXPORT(int,wxDialog_ShowModal)(wxDialog* self)
{
	return self->ShowModal();
}
	
EWXWEXPORT(void,wxDialog_EndModal)(wxDialog* self,int retCode)
{
	self->EndModal(retCode);
}
	
EWXWEXPORT(void,wxDialog_SetReturnCode)(wxDialog* self,int returnCode)
{
	self->SetReturnCode(returnCode);
}
	
EWXWEXPORT(int,wxDialog_GetReturnCode)(wxDialog* self)
{
	return self->GetReturnCode();
}
	
#if (wxVERSION_NUMBER >= 2400) && (wxVERSION_NUMBER < 2800)
EWXWEXPORT(void,wxDialog_SetIcons)(wxDialog* self,wxIconBundle* _icons)
{
	self->SetIcons(*_icons);
}
#endif

}
