#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxListBox_Create)(wxWindow* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _n, void* _str, int _stl)
{
	wxListBox* result = new wxListBox ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), 0, NULL, _stl, wxDefaultValidator);

	for (int i = 0; i < _n; i++)
		result->Append(((wxChar**)_str)[i]);

	return (void*) result;
}

EWXWEXPORT(void,wxListBox_Clear)(wxListBox* self)
{
	self->Clear();
}
	
EWXWEXPORT(void,wxListBox_Delete)(wxListBox* self,int n)
{
	self->Delete(n);
}
	
EWXWEXPORT(int,wxListBox_GetCount)(wxListBox* self)
{
	return self->GetCount();
}
	
EWXWEXPORT(wxString*,wxListBox_GetString)(wxListBox* self,int n)
{
	wxString *result = new wxString();
	*result = self->GetString(n);
	return result;
}
	
EWXWEXPORT(void,wxListBox_SetString)(wxListBox* self,int n,wxString* s)
{
	self->SetString(n,*s);
}
	
EWXWEXPORT(int,wxListBox_FindString)(wxListBox* self,wxString* s)
{
	return self->FindString(*s);
}
	
EWXWEXPORT(bool,wxListBox_IsSelected)(wxListBox* self,int n)
{
	return self->IsSelected(n);
}
	
EWXWEXPORT(void,wxListBox_SetSelection)(wxListBox* self,int n,bool select)
{
	self->SetSelection(n, select);
}
	
EWXWEXPORT(int,wxListBox_GetSelection)(wxListBox* self)
{
	return self->GetSelection();
}
	
EWXWEXPORT(int,wxListBox_GetSelections)(wxListBox* self,int* aSelections,int allocated)
{
	wxArrayInt sel;
	int result = self->GetSelections(sel);
	
	if (allocated < result) return -result;
	
	for (int i = 0; i < result; i++) aSelections[i] = sel[i];
	return result;
}
	
EWXWEXPORT(void,wxListBox_Append)(wxListBox* self,wxString* item)
{
	self->Append(*item);
}
	
EWXWEXPORT(void,wxListBox_AppendData)(wxListBox* self,wxString* item,void* _data)
{
	self->Append(*item, _data);
}
	
EWXWEXPORT(void,wxListBox_InsertItems)(wxListBox* self,void* items,int pos,int count)
{
	wxArrayString array;
	
	for (int i = 0; i< count; i++)
		array[i] = ((wxChar**)items)[i];
	
	self->InsertItems(array, pos);
}
	
EWXWEXPORT(void,wxListBox_SetFirstItem)(wxListBox* self,int n)
{
	self->SetFirstItem(n);
}
	
EWXWEXPORT(void,wxListBox_SetClientData)(wxListBox* self,int n,void* clientData)
{
	self->SetClientData(n, clientData);
}
	
EWXWEXPORT(void*,wxListBox_GetClientData)(wxListBox* self,int n)
{
	return (void*) self->GetClientData(n);
}
	
EWXWEXPORT(void,wxListBox_SetStringSelection)(wxListBox* self,wxString* str,bool sel)
{
	self->SetStringSelection(*str, sel);
}

}
