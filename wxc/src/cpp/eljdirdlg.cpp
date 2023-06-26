#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxDirDialog_Create)(wxWindow* _prt,wxString* _msg,wxString* _dir, int _lft, int _top, int _stl)
{
	return (void*) new wxDirDialog (_prt, *_msg, * _dir, _stl, wxPoint(_lft, _top));
}

EWXWEXPORT(void,wxDirDialog_SetMessage)(void* _obj,wxString* msg)
{
	((wxDirDialog*)_obj)->SetMessage(*msg);
}
	
EWXWEXPORT(void,wxDirDialog_SetPath)(void* _obj,wxString* pth)
{
	((wxDirDialog*)_obj)->SetPath(* pth);
}
	
EWXWEXPORT(void,wxDirDialog_SetStyle)(void* _obj,int style)
{
#if WXWIN_COMPATIBILITY_2_6
	((wxDirDialog*)_obj)->SetStyle((long)style);
#endif
}
	
EWXWEXPORT(wxString*,wxDirDialog_GetMessage)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxDirDialog*)_obj)->GetMessage();
	return result;
}
	
EWXWEXPORT(wxString*,wxDirDialog_GetPath)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxDirDialog*)_obj)->GetPath();
	return result;
}
	
EWXWEXPORT(int,wxDirDialog_GetStyle)(void* _obj)
{
#if WXWIN_COMPATIBILITY_2_6
	return (int)((wxDirDialog*)_obj)->GetStyle();
#else
	return 0;
#endif
}
	
}
