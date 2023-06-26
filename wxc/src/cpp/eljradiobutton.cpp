#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxRadioButton_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxRadioButton (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(void,wxRadioButton_SetValue)(wxRadioButton* self,bool value)
{
	self->SetValue(value);
}
	
EWXWEXPORT(bool,wxRadioButton_GetValue)(wxRadioButton* self)
{
	return self->GetValue();
}

} 
