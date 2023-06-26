#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxButton*,wxButton_Create)(wxWindow* _prt,int _id,wxString* _txt, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return new wxButton (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(bool,wxButton_SetBackgroundColour)(wxButton* self,wxColour* colour)
{
	return self->SetBackgroundColour(*colour);
}
	
EWXWEXPORT(void,wxButton_SetDefault)(wxButton* self)
{
	self->SetDefault();
}

EWXWEXPORT(wxBitmapButton*,wxBitmapButton_Create)(wxWindow* _prt,int _id,wxBitmap* _bmp, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return new wxBitmapButton (_prt, _id, *_bmp, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(void,wxBitmapButton_GetBitmapLabel)(wxBitmapButton* self,wxBitmap* _ref)
{
	*_ref = self->GetBitmapLabel();
}
	
EWXWEXPORT(void,wxBitmapButton_GetBitmapSelected)(wxBitmapButton* self,wxBitmap* _ref)
{
	*_ref = self->GetBitmapSelected();
}
	
EWXWEXPORT(void,wxBitmapButton_GetBitmapFocus)(wxBitmapButton* self,wxBitmap* _ref)
{
	*_ref = self->GetBitmapFocus();
}
	
EWXWEXPORT(void,wxBitmapButton_GetBitmapDisabled)(wxBitmapButton* self,wxBitmap* _ref)
{
	*_ref = self->GetBitmapDisabled();
}
	
EWXWEXPORT(void,wxBitmapButton_SetBitmapSelected)(wxBitmapButton* self,wxBitmap* sel)
{
	self->SetBitmapSelected(*sel);
}
	
EWXWEXPORT(void,wxBitmapButton_SetBitmapFocus)(wxBitmapButton* self,wxBitmap* focus)
{
	self->SetBitmapFocus(*focus);
}
	
EWXWEXPORT(void,wxBitmapButton_SetBitmapDisabled)(wxBitmapButton* self,wxBitmap* disabled)
{
	self->SetBitmapDisabled(*disabled);
}
	
EWXWEXPORT(void,wxBitmapButton_SetBitmapLabel)(wxBitmapButton* self,wxBitmap* bitmap)
{
	self->SetBitmapLabel(*bitmap);
}
	
EWXWEXPORT(void,wxBitmapButton_SetMargins)(wxBitmapButton* self,int x,int y)
{
	self->SetMargins(x, y);
}
	
EWXWEXPORT(int,wxBitmapButton_GetMarginX)(wxBitmapButton* self)
{
	return self->GetMarginX();
}
	
EWXWEXPORT(int,wxBitmapButton_GetMarginY)(wxBitmapButton* self)
{
	return self->GetMarginY();
}
	
}
