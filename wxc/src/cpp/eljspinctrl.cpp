#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxSpinCtrl_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,long _stl,int _min,int _max,int _init)
{
	return (void*) new wxSpinCtrl (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, _min, _max, _init);
}

EWXWEXPORT(void,wxSpinCtrl_SetValue)(void* _obj,int val)
{
	((wxSpinCtrl*)_obj)->SetValue(val);
}
	
EWXWEXPORT(int,wxSpinCtrl_GetValue)(void* _obj)
{
	return ((wxSpinCtrl*)_obj)->GetValue();
}
	
EWXWEXPORT(void,wxSpinCtrl_SetRange)(void* _obj,int min_val,int max_val)
{
	((wxSpinCtrl*)_obj)->SetRange(min_val, max_val);
}
	
EWXWEXPORT(int,wxSpinCtrl_GetMin)(void* _obj)
{
	return ((wxSpinCtrl*)_obj)->GetMin();
}
	
EWXWEXPORT(int,wxSpinCtrl_GetMax)(void* _obj)
{
	return ((wxSpinCtrl*)_obj)->GetMax();
}

EWXWEXPORT(void*,wxSpinButton_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,long _stl)
{
	return (void*) new wxSpinButton (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int,wxSpinButton_GetValue)(void* _obj)
{
	return ((wxSpinButton*)_obj)->GetValue();
}
	
EWXWEXPORT(int,wxSpinButton_GetMin)(void* _obj)
{
	return ((wxSpinButton*)_obj)->GetMin();
}
	
EWXWEXPORT(int,wxSpinButton_GetMax)(void* _obj)
{
	return ((wxSpinButton*)_obj)->GetMax();
}
	
EWXWEXPORT(void,wxSpinButton_SetValue)(void* _obj,int val)
{
	((wxSpinButton*)_obj)->SetValue(val);
}
	
EWXWEXPORT(void,wxSpinButton_SetRange)(void* _obj,int minVal,int maxVal)
{
	((wxSpinButton*)_obj)->SetRange(minVal, maxVal);
}
	
}
