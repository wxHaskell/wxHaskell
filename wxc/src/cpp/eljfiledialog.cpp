#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxFileDialog_Create) (wxWindow* _prt,wxString* _msg,wxString* _dir,wxString* _fle,wxString* _wcd, int _lft, int _top, int _stl)
{
	return (void*) new wxFileDialog (_prt,*_msg,*_dir,*_fle,*_wcd, _stl, wxPoint(_lft, _top));
}

EWXWEXPORT(void,wxFileDialog_SetMessage)(void* _obj,wxString* message)
{
	((wxFileDialog*)_obj)->SetMessage(*message);
}
	
EWXWEXPORT(void,wxFileDialog_SetPath)(void* _obj,wxString* path)
{
	((wxFileDialog*)_obj)->SetPath(*path);
}
	
EWXWEXPORT(void,wxFileDialog_SetDirectory)(void* _obj,wxString* dir)
{
	((wxFileDialog*)_obj)->SetDirectory(*dir);
}
	
EWXWEXPORT(void,wxFileDialog_SetFilename)(void* _obj,wxString* name)
{
	((wxFileDialog*)_obj)->SetFilename(*name);
}
	
EWXWEXPORT(void,wxFileDialog_SetWildcard)(void* _obj,wxString* wildCard)
{
	((wxFileDialog*)_obj)->SetWildcard(*wildCard);
}
	
EWXWEXPORT(void,wxFileDialog_SetStyle)(void* _obj,int style)
{
#if WXWIN_COMPATIBILITY_2_6
	((wxFileDialog*)_obj)->SetStyle((long)style);
#endif
}
	
EWXWEXPORT(void,wxFileDialog_SetFilterIndex)(void* _obj,int filterIndex)
{
	((wxFileDialog*)_obj)->SetFilterIndex(filterIndex);
}
	
EWXWEXPORT(wxString*,wxFileDialog_GetMessage)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxFileDialog*)_obj)->GetMessage();
	return result;
}
	
EWXWEXPORT(wxString*,wxFileDialog_GetPath)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxFileDialog*)_obj)->GetPath();
	return result;
}
	
EWXWEXPORT(int,wxFileDialog_GetPaths)(void* _obj,void* paths)
{
	wxArrayString arr;
	((wxFileDialog*)_obj)->GetPaths(arr);
	if (paths)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
		    ((const wxChar**)paths)[i] = wxStrdup (arr.Item(i).wchar_str());
	}
	return arr.GetCount();
}
	
EWXWEXPORT(wxString*,wxFileDialog_GetDirectory)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxFileDialog*)_obj)->GetDirectory();
	return result;
}
	
EWXWEXPORT(wxString*,wxFileDialog_GetFilename)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxFileDialog*)_obj)->GetFilename();
	return result;
}
	
EWXWEXPORT(int,wxFileDialog_GetFilenames)(void* _obj,void* paths)
{
	wxArrayString arr;
	((wxFileDialog*)_obj)->GetFilenames(arr);
	if (paths)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((const wxChar**)paths)[i] = wxStrdup (arr.Item(i).wchar_str());
	}
	return arr.GetCount();
}
	
EWXWEXPORT(wxString*,wxFileDialog_GetWildcard)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxFileDialog*)_obj)->GetWildcard();
	return result;
}
	
EWXWEXPORT(int,wxFileDialog_GetStyle)(void* _obj)
{
#if WXWIN_COMPATIBILITY_2_6
	return (int)((wxFileDialog*)_obj)->GetStyle();
#else
	return 0;
#endif
}
	
EWXWEXPORT(int,wxFileDialog_GetFilterIndex)(void* _obj)
{
	return ((wxFileDialog*)_obj)->GetFilterIndex();
}
	
}
