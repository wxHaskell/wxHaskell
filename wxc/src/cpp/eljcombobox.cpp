#include "wrapper.h"

extern "C"
{

EWXWEXPORT(wxComboBox*,wxComboBox_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _n,void* _str,int _stl)
{
	wxString* list = new wxString[_n];

	for (int i = 0; i < _n; i++)
		list[i] = ((wxChar**)_str)[i];

	wxComboBox* result = new wxComboBox (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _n, list, _stl, wxDefaultValidator);

	delete [] list;
#if wxVERSION_NUMBER < 2400
	if ((result->Number()) && (result->GetSelection() == -1)) result->SetSelection(0);
#else
	if ((result->GetCount()) && (result->GetSelection() == -1)) result->SetSelection(0);
#endif
	
	return result;
}

EWXWEXPORT(void,wxComboBox_Copy)(wxComboBox* self)
{
	self->Copy();
}
	
EWXWEXPORT(void,wxComboBox_Cut)(wxComboBox* self)
{
	self->Cut();
}
	
EWXWEXPORT(void,wxComboBox_Paste)(wxComboBox* self)
{
	self->Paste();
}
	
EWXWEXPORT(void,wxComboBox_SetInsertionPoint)(wxComboBox* self,int pos)
{
	self->SetInsertionPoint(pos);
}
	
EWXWEXPORT(void,wxComboBox_SetInsertionPointEnd)(wxComboBox* self)
{
	self->SetInsertionPointEnd();
}
	
EWXWEXPORT(int,wxComboBox_GetInsertionPoint)(wxComboBox* self)
{
	return self->GetInsertionPoint();
}
	
EWXWEXPORT(int,wxComboBox_GetLastPosition)(wxComboBox* self)
{
	return self->GetLastPosition();
}
	
EWXWEXPORT(void,wxComboBox_Replace)(wxComboBox* self,int from,int to,wxString* value)
{
	self->Replace(from, to,*value);
}
	
EWXWEXPORT(void,wxComboBox_Remove)(wxComboBox* self,int from,int to)
{
	self->Remove(from, to);
#if wxVERSION_NUMBER < 2400
	if ((self->Number()) && (self->GetSelection() == -1)) self->SetSelection(0);
#else
	if ((self->GetCount()) && (self->GetSelection() == -1)) self->SetSelection(0);
#endif
}
	
EWXWEXPORT(void,wxComboBox_SetTextSelection)(wxComboBox* self,int from,int to)
{
	self->SetSelection(from, to);
}
	
EWXWEXPORT(void,wxComboBox_SetEditable)(wxComboBox* self,bool editable)
{
	self->SetEditable(editable);
}
	
EWXWEXPORT(wxString*,wxComboBox_GetStringSelection)(wxComboBox* self)
{
	return new wxString(self->GetStringSelection());
}
	
EWXWEXPORT(wxString*,wxComboBox_GetValue)(wxComboBox* self)
{
	return new wxString(self->GetValue());
}

EWXWEXPORT(void,wxComboBox_SetValue)(wxComboBox* self,wxString* value)
{
	self->SetValue(*value);
}

	
EWXWEXPORT(void,wxComboBox_Append)(wxComboBox* self,wxString* item)
{
	self->Append(*item);
#if wxVERSION_NUMBER < 2400
	if (self->Number() && (self->GetSelection() == -1)) self->SetSelection(0);
#else
	if (self->GetCount() && (self->GetSelection() == -1)) self->SetSelection(0);
#endif
}
	
EWXWEXPORT(void,wxComboBox_AppendData)(wxComboBox* self,wxString* item,void* d)
{
#if defined(__WXMAC__)
	self->Append(*item);
#else
	self->Append(*item, d);
#endif

#if wxVERSION_NUMBER < 2400
	if ((self->Number()) && (self->GetSelection() == -1)) self->SetSelection(0);
#else
	if ((self->GetCount()) && (self->GetSelection() == -1)) self->SetSelection(0);
#endif
}
	
EWXWEXPORT(void,wxComboBox_Delete)(wxComboBox* self,int n)
{
	self->Delete(n);
#if wxVERSION_NUMBER < 2400
	if ((self->Number()) && (self->GetSelection() == -1)) self->SetSelection(0);
#else
	if ((self->GetCount()) && (self->GetSelection() == -1)) self->SetSelection(0);
#endif
}
	
EWXWEXPORT(void,wxComboBox_Clear)(wxComboBox* self)
{
	self->Clear();
}
	
EWXWEXPORT(int,wxComboBox_GetCount)(wxComboBox* self)
{
#if wxVERSION_NUMBER < 2400
	return self->Number();
#else
	return self->GetCount();
#endif
}
	
EWXWEXPORT(int,wxComboBox_GetSelection)(wxComboBox* self)
{
	return self->GetSelection();
}
	
EWXWEXPORT(void,wxComboBox_SetSelection)(wxComboBox* self,int n)
{
	self->SetSelection(n);
}
	
EWXWEXPORT(int,wxComboBox_FindString)(wxComboBox* self,wxString* s)
{
	return self->FindString(*s);
}
	
EWXWEXPORT(wxString*,wxComboBox_GetString)(wxComboBox* self,int n)
{
	return new wxString(self->GetString(n));
}
	
EWXWEXPORT(void,wxComboBox_SetString)(wxComboBox* self,int n,wxString* s)
{
	self->SetString(n,*s);
}
	
EWXWEXPORT(void,wxComboBox_SetClientData)(wxComboBox* self,int n,void* clientData)
{
	((wxItemContainer*) self)->SetClientData( n, clientData );
}
	
EWXWEXPORT(void*,wxComboBox_GetClientData)(wxComboBox* self,int n)
{
	return ((wxItemContainer*) self)->GetClientData(n);
}

}
