#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxSimplebook_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxSimplebook (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int,wxSimplebook_SetSelection)(wxSimplebook* self,int nPage)
{
	return self->SetSelection(nPage);
}

EWXWEXPORT(bool,wxSimplebook_AddPage)(wxSimplebook* self,wxWindow* pPage,wxString* strText,bool bSelect)
{
	return self->AddPage( pPage,* strText, bSelect, wxBookCtrlBase::NO_IMAGE);
}

}
