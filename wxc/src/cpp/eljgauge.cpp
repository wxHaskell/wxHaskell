#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxGauge_Create) (void* _prt, int _id, int _rng, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxGauge ((wxWindow*)_prt, _id, _rng, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void, wxGauge_SetShadowWidth)(void* _obj, int w)
{
	((wxGauge*)_obj)->SetShadowWidth(w);
}
	
EWXWEXPORT(void, wxGauge_SetBezelFace)(void* _obj, int w)
{
	((wxGauge*)_obj)->SetBezelFace(w);
}
	
EWXWEXPORT(void, wxGauge_SetRange)(void* _obj, int r)
{
	((wxGauge*)_obj)->SetRange(r);
}
	
EWXWEXPORT(void, wxGauge_SetValue)(void* _obj, int pos)
{
	((wxGauge*)_obj)->SetValue(pos);
}
	
EWXWEXPORT(int, wxGauge_GetShadowWidth)(void* _obj)
{
	return ((wxGauge*)_obj)->GetShadowWidth();
}
	
EWXWEXPORT(int, wxGauge_GetBezelFace)(void* _obj)
{
	return ((wxGauge*)_obj)->GetBezelFace();
}
	
EWXWEXPORT(int, wxGauge_GetRange)(void* _obj)
{
	return ((wxGauge*)_obj)->GetRange();
}
	
EWXWEXPORT(int, wxGauge_GetValue)(void* _obj)
{
	return ((wxGauge*)_obj)->GetValue();
}

EWXWEXPORT(void, wxGauge_Pulse)(void* _obj)
{
	((wxGauge*)_obj)->Pulse();
}

}
