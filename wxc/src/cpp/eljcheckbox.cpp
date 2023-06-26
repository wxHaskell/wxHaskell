#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxCheckBox_Create)(wxWindow* _prt,int _id,wxString* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxCheckBox (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(void,wxCheckBox_Delete)(wxCheckBox* self)
{
	delete self;
}

EWXWEXPORT(void,wxCheckBox_SetValue)(wxCheckBox* self,bool value)
{
	self->SetValue(value);
}
	
EWXWEXPORT(bool,wxCheckBox_GetValue)(wxCheckBox* self)
{
	return self->GetValue();
}

} 
