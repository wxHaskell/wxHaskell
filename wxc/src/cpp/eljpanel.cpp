#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxPanel_Create) (void* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxPanel ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void, wxPanel_InitDialog)(void* _obj)
{
	((wxPanel*)_obj)->InitDialog();
}
	
EWXWEXPORT(void*, wxPanel_GetDefaultItem)(void* _obj)
{
#if (wxVERSION_NUMBER <= 2800)
	return (void*)((wxPanel*)_obj)->GetDefaultItem();
#else
	return (void*)((wxTopLevelWindow*)_obj)->GetDefaultItem();
#endif
}
	
EWXWEXPORT(void, wxPanel_SetDefaultItem)(void* _obj, void* btn)
{
#if (wxVERSION_NUMBER <= 2800)
	((wxPanel*)_obj)->SetDefaultItem((wxButton*) btn);
#else
	((wxTopLevelWindow*)_obj)->SetDefaultItem((wxButton*) btn);
#endif
}

#if (wxVERSION_NUMBER >= 2800)
EWXWEXPORT(void, wxPanel_SetFocus)(void* _obj)
{
	((wxPanel*)_obj)->SetFocus();
}
#endif
}
