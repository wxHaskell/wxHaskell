#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxChoice*,wxChoice_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _n,void* _str,int _stl)
{
	wxString* list = new wxString[_n];

	for (int i = 0; i < _n; i++)
		list[i] = ((wxChar**)_str)[i];

	wxChoice* result = new wxChoice (_prt, _id, wxPoint(_lft, _top),wxSize(_wdt, _hgt), _n, list, _stl, wxDefaultValidator);

	delete [] list;

	return result;
}

EWXWEXPORT(void,wxChoice_Append)(wxChoice* self,wxString* item)
{
	self->Append(*item);
}
	
EWXWEXPORT(void,wxChoice_Delete)(wxChoice* self,int n)
{
	self->Delete(n);
}
	
EWXWEXPORT(void,wxChoice_Clear)(wxChoice* self)
{
	self->Clear();
}
	
EWXWEXPORT(int,wxChoice_GetCount)(wxChoice* self)
{
	return self->GetCount();
}
	
EWXWEXPORT(int,wxChoice_GetSelection)(wxChoice* self)
{
	return self->GetSelection();
}
	
EWXWEXPORT(void,wxChoice_SetSelection)(wxChoice* self,int n)
{
	self->SetSelection(n);
}
	
EWXWEXPORT(int,wxChoice_FindString)(wxChoice* self,wxString* s)
{
	return self->FindString(*s);
}
	
EWXWEXPORT(wxString*,wxChoice_GetString)(wxChoice* self,int n)
{
	wxString *result = new wxString();
	*result = self->GetString(n);
	return result;
}
	
EWXWEXPORT(void,wxChoice_SetString)(wxChoice* self,int n,wxString* s)
{
	self->SetString(n,*s);
}
	
} 
