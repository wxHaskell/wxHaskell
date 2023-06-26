#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxTextCtrl_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,long _stl)
{
	return (void*) new wxTextCtrl (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(wxString*,wxTextCtrl_GetValue)(void* self)
{
	wxString *result = new wxString();
	*result = ((wxTextCtrl*)self)->GetValue();
	return result;
}
	
EWXWEXPORT(void,wxTextCtrl_SetValue)(wxTextCtrl* self,wxString* value)
{
	self->SetValue(*value);
}

EWXWEXPORT(void,wxTextCtrl_ChangeValue)(wxTextCtrl* self, wxString* value)
{
        self->ChangeValue(*value);
}
	
EWXWEXPORT(int,wxTextCtrl_GetLineLength)(wxTextCtrl* self,long lineNo)
{
	return self->GetLineLength(lineNo);
}
	
EWXWEXPORT(wxString*,wxTextCtrl_GetLineText)(wxTextCtrl* self,long lineNo)
{
	wxString *result = new wxString();
	*result = self->GetLineText(lineNo);
	return result;
}
	
EWXWEXPORT(int,wxTextCtrl_GetNumberOfLines)(wxTextCtrl* self)
{
	return self->GetNumberOfLines();
}
	
EWXWEXPORT(bool,wxTextCtrl_IsModified)(wxTextCtrl* self)
{
	return self->IsModified();
}
	
EWXWEXPORT(bool,wxTextCtrl_IsEditable)(wxTextCtrl* self)
{
	return self->IsEditable();
}
	
EWXWEXPORT(void,wxTextCtrl_GetSelection)(wxTextCtrl* self,void* from,void* to)
{
	self->GetSelection((long*)from, (long*)to);
}
	
EWXWEXPORT(void,wxTextCtrl_Clear)(wxTextCtrl* self)
{
	self->Clear();
}
	
EWXWEXPORT(void,wxTextCtrl_Replace)(wxTextCtrl* self,long from,long to,wxString* value)
{
	self->Replace(from, to,*value);
}
	
EWXWEXPORT(void,wxTextCtrl_Remove)(wxTextCtrl* self,long from,long to)
{
	self->Remove(from, to);
}
	
EWXWEXPORT(bool,wxTextCtrl_LoadFile)(wxTextCtrl* self,wxString* file)
{
	return self->LoadFile(*file);
}
	
EWXWEXPORT(bool,wxTextCtrl_SaveFile)(wxTextCtrl* self,wxString* file)
{
	return self->SaveFile(*file);
}
	
EWXWEXPORT(void,wxTextCtrl_DiscardEdits)(wxTextCtrl* self)
{
	self->DiscardEdits();
}
	
EWXWEXPORT(void,wxTextCtrl_WriteText)(wxTextCtrl* self,wxString* text)
{
	self->WriteText(*text);
}
	
EWXWEXPORT(void,wxTextCtrl_AppendText)(wxTextCtrl* self,wxString* text)
{
	self->AppendText(*text);
}
	
EWXWEXPORT(long,wxTextCtrl_XYToPosition)(wxTextCtrl* self,long x,long y)
{
	return self->XYToPosition(x, y);
}
	
EWXWEXPORT(int,wxTextCtrl_PositionToXY)(wxTextCtrl* self,long pos,long* x,long* y)
{
	return (int)self->PositionToXY(pos, x, y);
}
	
EWXWEXPORT(void,wxTextCtrl_ShowPosition)(wxTextCtrl* self,long pos)
{
	self->ShowPosition(pos);
}
	
EWXWEXPORT(void,wxTextCtrl_Copy)(wxTextCtrl* self)
{
	self->Copy();
}
	
EWXWEXPORT(void,wxTextCtrl_Cut)(wxTextCtrl* self)
{
	self->Cut();
}
	
EWXWEXPORT(void,wxTextCtrl_Paste)(wxTextCtrl* self)
{
	self->Paste();
}
	
EWXWEXPORT(bool,wxTextCtrl_CanCopy)(wxTextCtrl* self)
{
	return self->CanCopy();
}
	
EWXWEXPORT(bool,wxTextCtrl_CanCut)(wxTextCtrl* self)
{
	return self->CanCut();
}
	
EWXWEXPORT(bool,wxTextCtrl_CanPaste)(wxTextCtrl* self)
{
	return self->CanPaste();
}
	
EWXWEXPORT(void,wxTextCtrl_Undo)(wxTextCtrl* self)
{
	self->Undo();
}
	
EWXWEXPORT(void,wxTextCtrl_Redo)(wxTextCtrl* self)
{
	self->Redo();
}
	
EWXWEXPORT(bool,wxTextCtrl_CanUndo)(wxTextCtrl* self)
{
	return self->CanUndo();
}
	
EWXWEXPORT(bool,wxTextCtrl_CanRedo)(wxTextCtrl* self)
{
	return self->CanRedo();
}
	
EWXWEXPORT(void,wxTextCtrl_SetInsertionPoint)(wxTextCtrl* self,long pos)
{
	self->SetInsertionPoint(pos);
}
	
EWXWEXPORT(void,wxTextCtrl_SetInsertionPointEnd)(wxTextCtrl* self)
{
	self->SetInsertionPointEnd();
}
	
EWXWEXPORT(long,wxTextCtrl_GetInsertionPoint)(wxTextCtrl* self)
{
	return self->GetInsertionPoint();
}
	
EWXWEXPORT(long,wxTextCtrl_GetLastPosition)(wxTextCtrl* self)
{
	return self->GetLastPosition();
}
	
EWXWEXPORT(void,wxTextCtrl_SetSelection)(wxTextCtrl* self,long from,long to)
{
	self->SetSelection(from, to);
}
	
EWXWEXPORT(void,wxTextCtrl_SetEditable)(wxTextCtrl* self,bool editable)
{
	self->SetEditable(editable);
}
	
}
