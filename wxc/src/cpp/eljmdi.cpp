#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxMDIParentFrame_Create) (wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxMDIParentFrame (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void*,wxMDIParentFrame_GetActiveChild)(void* _obj)
{
	return (void*)((wxMDIParentFrame*)_obj)->GetActiveChild();
}
	
EWXWEXPORT(void*,wxMDIParentFrame_GetClientWindow)(void* _obj)
{
	return (void*)((wxMDIParentFrame*)_obj)->GetClientWindow();
}
	
EWXWEXPORT(void*,wxMDIParentFrame_OnCreateClient)(void* _obj)
{
	return (void*)((wxMDIParentFrame*)_obj)->OnCreateClient();
}
	
EWXWEXPORT(void*,wxMDIParentFrame_GetWindowMenu)(void* _obj)
{
#ifdef __WIN32__
	return (void*)((wxMDIParentFrame*)_obj)->GetWindowMenu();
#else
	return NULL;
#endif
}
	
EWXWEXPORT(void,wxMDIParentFrame_SetWindowMenu)(void* _obj,void* menu)
{
#ifdef __WIN32__
	((wxMDIParentFrame*)_obj)->SetWindowMenu((wxMenu*) menu);
#endif
}
	
EWXWEXPORT(void,wxMDIParentFrame_Cascade)(void* _obj)
{
	((wxMDIParentFrame*)_obj)->Cascade();
}
	
EWXWEXPORT(void,wxMDIParentFrame_Tile)(void* _obj)
{
	((wxMDIParentFrame*)_obj)->Tile();
}
	
EWXWEXPORT(void,wxMDIParentFrame_ArrangeIcons)(void* _obj)
{
	((wxMDIParentFrame*)_obj)->ArrangeIcons();
}
	
EWXWEXPORT(void,wxMDIParentFrame_ActivateNext)(void* _obj)
{
	((wxMDIParentFrame*)_obj)->ActivateNext();
}
	
EWXWEXPORT(void,wxMDIParentFrame_ActivatePrevious)(void* _obj)
{
	((wxMDIParentFrame*)_obj)->ActivatePrevious();
}
	
EWXWEXPORT(void*,wxMDIChildFrame_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxMDIChildFrame ((wxMDIParentFrame *)_prt, _id, *_txt,wxPoint(_lft, _top),wxSize(_wdt, _hgt),_stl);
}

EWXWEXPORT(void,wxMDIChildFrame_Activate)(void* _obj)
{
	((wxMDIChildFrame*)_obj)->Activate();
}
	
}
