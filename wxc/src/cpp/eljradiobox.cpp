#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxRadioBox_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _n,void* _str,int _dim,int _stl)
{
	wxString items[256];

	for (int i = 0; i < _n; i++)
		items[i] = ((wxChar**)_str)[i];

	return (void*) new wxRadioBox (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _n, items, _dim, _stl, wxDefaultValidator);
}

EWXWEXPORT(int,wxRadioBox_FindString)(void* _obj,wxString* s)
{
	return ((wxRadioBox*)_obj)->FindString(* s);
}

EWXWEXPORT(void,wxRadioBox_SetSelection)(void* _obj,int _n)
{
	((wxRadioBox*)_obj)->SetSelection(_n);
}

EWXWEXPORT(int,wxRadioBox_GetSelection)(void* _obj)
{
	return ((wxRadioBox*)_obj)->GetSelection();
}

EWXWEXPORT(void,wxRadioBox_SetItemLabel)(void* _obj,int item,wxString* label)
{
#if wxVERSION_NUMBER >= 2400
	((wxRadioBoxBase*)_obj)->SetString(item, *label);
#else
	((wxRadioBox*)_obj)->SetLabel(item, *label);
#endif
}

EWXWEXPORT(void,wxRadioBox_SetItemBitmap)(void* _obj,int item,void* bitmap)
{
#if wxVERSION_NUMBER < 2400
	((wxRadioBox*)_obj)->SetLabel(item, (wxBitmap*) bitmap);
#endif
}

EWXWEXPORT(wxString*,wxRadioBox_GetItemLabel)(void* _obj,int item)
{
  wxString *result = new wxString();

#if wxVERSION_NUMBER >= 2400
    *result = ((wxRadioBox*)_obj)->GetString(item);
#else
    *result = ((wxRadioBox*)_obj)->GetLabel(item);
#endif

  return result;
}

EWXWEXPORT(void,wxRadioBox_EnableItem)(void* self,int item,bool enable)
{
	((wxRadioBox*)self)->Enable(item, enable);
}

EWXWEXPORT(void,wxRadioBox_ShowItem)(void* self,int item,bool show)
{
	((wxRadioBox*)self)->Show(item, show);
}

EWXWEXPORT(wxString*,wxRadioBox_GetStringSelection)(void* _obj)
{
  wxString *result = new wxString();
  *result = ((wxRadioBox*)_obj)->GetStringSelection();
  return result;
}

EWXWEXPORT(void,wxRadioBox_SetStringSelection)(void* _obj,wxString* s)
{
	((wxRadioBox*)_obj)->SetStringSelection(* s);
}

EWXWEXPORT(int,wxRadioBox_Number)(void* _obj)
{
#if wxVERSION_NUMBER >= 2400
	return ((wxRadioBox*)_obj)->GetCount();
#else
	return ((wxRadioBox*)_obj)->Number();
#endif
}

EWXWEXPORT(int,wxRadioBox_GetNumberOfRowsOrCols)(void* _obj)
{
#if wxVERSION_NUMBER >= 2600
	return ((wxRadioBox*)_obj)->GetCount();
#else
	return ((wxRadioBox*)_obj)->GetNumberOfRowsOrCols();
#endif
}

EWXWEXPORT(void,wxRadioBox_SetNumberOfRowsOrCols)(void* _obj,int n)
{
#if wxVERSION_NUMBER >= 2600
	return;
#else
	((wxRadioBox*)_obj)->SetNumberOfRowsOrCols(n);
#endif
}

}
