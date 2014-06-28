#include "wrapper.h"
#include "wx/slider.h"

extern "C"
{

EWXWEXPORT(void*, wxSlider_Create) (void* _prt, int _id, int _init, int _min, int _max, int _lft, int _top, int _wdt, int _hgt, long _stl)
{
	return (void*) new wxSlider ((wxWindow*)_prt, _id, _init, _min, _max, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int,wxSlider_GetValue)(void* _obj)
{
	return ((wxSlider*)_obj)->GetValue();
}
	
EWXWEXPORT(void,wxSlider_SetValue)(void* _obj, int value)
{
	((wxSlider*)_obj)->SetValue(value);
}
	
EWXWEXPORT(void,wxSlider_SetRange)(void* _obj, int minValue, int maxValue)
{
	((wxSlider*)_obj)->SetRange(minValue, maxValue);
}
	
EWXWEXPORT(int,wxSlider_GetMin)(void* _obj)
{
	return ((wxSlider*)_obj)->GetMin();
}
	
EWXWEXPORT(int,wxSlider_GetMax)(void* _obj)
{
	return ((wxSlider*)_obj)->GetMax();
}
	
// Obsolete
// EWXWEXPORT(void,wxSlider_SetTickFreq)(void* _obj, int n, int pos)
	
EWXWEXPORT(int,wxSlider_GetTickFreq)(void* _obj)
{
	return ((wxSlider*)_obj)->GetTickFreq();
}
	
EWXWEXPORT(void,wxSlider_SetPageSize)(void* _obj, int pageSize)
{
	((wxSlider*)_obj)->SetPageSize(pageSize);
}
	
EWXWEXPORT(int,wxSlider_GetPageSize)(void* _obj)
{
	return ((wxSlider*)_obj)->GetPageSize();
}
	
EWXWEXPORT(void,wxSlider_ClearSel)(void* _obj)
{
	((wxSlider*)_obj)->ClearSel();
}
	
EWXWEXPORT(void,wxSlider_ClearTicks)(void* _obj)
{
	((wxSlider*)_obj)->ClearTicks();
}
	
EWXWEXPORT(void,wxSlider_SetLineSize)(void* _obj, int lineSize)
{
	((wxSlider*)_obj)->SetLineSize(lineSize);
}
	
EWXWEXPORT(int,wxSlider_GetLineSize)(void* _obj)
{
	return ((wxSlider*)_obj)->GetLineSize();
}
	
EWXWEXPORT(int,wxSlider_GetSelEnd)(void* _obj)
{
	return ((wxSlider*)_obj)->GetSelEnd();
}
	
EWXWEXPORT(int,wxSlider_GetSelStart)(void* _obj)
{
	return ((wxSlider*)_obj)->GetSelStart();
}
	
EWXWEXPORT(void,wxSlider_SetSelection)(void* _obj, int minPos, int maxPos)
{
	((wxSlider*)_obj)->SetSelection(minPos, maxPos);
}
	
EWXWEXPORT(void,wxSlider_SetThumbLength)(void* _obj, int len)
{
	((wxSlider*)_obj)->SetThumbLength(len);
}
	
EWXWEXPORT(int,wxSlider_GetThumbLength)(void* _obj)
{
	return ((wxSlider*)_obj)->GetThumbLength();
}
	
EWXWEXPORT(void,wxSlider_SetTick)(void* _obj, int tickPos)
{
	((wxSlider*)_obj)->SetTick(tickPos);
}
	
} 
