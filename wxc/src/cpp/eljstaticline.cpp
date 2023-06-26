#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxStaticLine_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*)new wxStaticLine ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(bool,wxStaticLine_IsVertical)(wxStaticLine* self)
{
	return self->IsVertical ();
}
	
EWXWEXPORT(int,wxStaticLine_GetDefaultSize)(wxStaticLine* self)
{
	return self->GetDefaultSize ();
}

}
