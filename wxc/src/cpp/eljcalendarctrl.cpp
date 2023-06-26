#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxCalendarCtrl*,wxCalendarCtrl_Create)(wxWindow* _prt,int _id,wxDateTime* _dat,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return new wxCalendarCtrl (_prt, _id, *_dat, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void,wxCalendarCtrl_SetDate)(wxCalendarCtrl* self,wxDateTime* date)
{
	self->SetDate(*date);
}
	
EWXWEXPORT(void,wxCalendarCtrl_GetDate)(wxCalendarCtrl* self,wxDateTime* date)
{
	*date = self->GetDate();
}
	
EWXWEXPORT(void,wxCalendarCtrl_EnableMonthChange)(wxCalendarCtrl* self,bool enable)
{
	self->EnableMonthChange(enable);
}
	
EWXWEXPORT(void,wxCalendarCtrl_EnableHolidayDisplay)(wxCalendarCtrl* self,bool display)
{
	self->EnableHolidayDisplay(display);
}
	
EWXWEXPORT(void,wxCalendarCtrl_SetHeaderColours)(wxCalendarCtrl* self,wxColour* colFg,wxColour* colBg)
{
	self->SetHeaderColours(*colFg,*colBg);
}
	
EWXWEXPORT(void,wxCalendarCtrl_GetHeaderColourFg)(wxCalendarCtrl* self,wxColour* colour)
{
	*colour = self->GetHeaderColourFg();
}
	
EWXWEXPORT(void,wxCalendarCtrl_GetHeaderColourBg)(wxCalendarCtrl* self,wxColour* colour)
{
	*colour = self->GetHeaderColourBg();
}
	
EWXWEXPORT(void,wxCalendarCtrl_SetHighlightColours)(wxCalendarCtrl* self,wxColour* colFg,wxColour* colBg)
{
	self->SetHighlightColours(*colFg,*colBg);
}
	
EWXWEXPORT(void,wxCalendarCtrl_GetHighlightColourFg)(wxCalendarCtrl* self,wxColour* colour)
{
	*colour = self->GetHighlightColourFg();
}
	
EWXWEXPORT(void,wxCalendarCtrl_GetHighlightColourBg)(wxCalendarCtrl* self,wxColour* colour)
{
	*colour = self->GetHighlightColourBg();
}
	
EWXWEXPORT(void,wxCalendarCtrl_SetHolidayColours)(wxCalendarCtrl* self,wxColour* colFg,wxColour* colBg)
{
	self->SetHolidayColours(*colFg,*colBg);
}
	
EWXWEXPORT(void,wxCalendarCtrl_GetHolidayColourFg)(wxCalendarCtrl* self,wxColour* colour)
{
	*colour = self->GetHolidayColourFg();
}
	
EWXWEXPORT(void,wxCalendarCtrl_GetHolidayColourBg)(wxCalendarCtrl* self,wxColour* colour)
{
	*colour = self->GetHolidayColourBg();
}
	
EWXWEXPORT(wxCalendarDateAttr*,wxCalendarCtrl_GetAttr)(wxCalendarCtrl* self,size_t day)
{
	return self->GetAttr(day);
}
	
EWXWEXPORT(void,wxCalendarCtrl_SetAttr)(wxCalendarCtrl* self,size_t day,wxCalendarDateAttr* attr)
{
	self->SetAttr(day, attr);
}
	
EWXWEXPORT(void,wxCalendarCtrl_SetHoliday)(wxCalendarCtrl* self,size_t day)
{
	self->SetHoliday(day);
}
	
EWXWEXPORT(void,wxCalendarCtrl_ResetAttr)(wxCalendarCtrl* self,size_t day)
{
	self->ResetAttr(day);
}
	
EWXWEXPORT(int,wxCalendarCtrl_HitTest)(wxCalendarCtrl* self,int x,int y,wxDateTime* date,void* wd)
{
	return (int)self->HitTest(wxPoint(x, y), date, (wxDateTime::WeekDay*)wd);
}
	

EWXWEXPORT(wxCalendarDateAttr*,wxCalendarDateAttr_Create)(wxColour* _ctxt,wxColour* _cbck,wxColour* _cbrd,wxFont* _fnt,int _brd)
{
	return new wxCalendarDateAttr(*_ctxt,*_cbck,*_cbrd,*_fnt, (wxCalendarDateBorder)_brd);
}

EWXWEXPORT(wxCalendarDateAttr*,wxCalendarDateAttr_CreateDefault)()
{
	return new wxCalendarDateAttr();
}

EWXWEXPORT(void,wxCalendarDateAttr_Delete)(wxCalendarDateAttr* self)
{
	delete self;
}

EWXWEXPORT(void,wxCalendarDateAttr_SetTextColour)(wxCalendarDateAttr* self,wxColour* col)
{
	self->SetTextColour(*col);
}
	
EWXWEXPORT(void,wxCalendarDateAttr_SetBackgroundColour)(wxCalendarDateAttr* self,wxColour* col)
{
	self->SetBackgroundColour(*col);
}
	
EWXWEXPORT(void,wxCalendarDateAttr_SetBorderColour)(wxCalendarDateAttr* self,wxColour* col)
{
	self->SetBorderColour(*col);
}
	
EWXWEXPORT(void,wxCalendarDateAttr_SetFont)(wxCalendarDateAttr* self,wxFont* font)
{
	self->SetFont(*font);
}
	
EWXWEXPORT(void,wxCalendarDateAttr_SetBorder)(wxCalendarDateAttr* self,int border)
{
	self->SetBorder((wxCalendarDateBorder)border);
}
	
EWXWEXPORT(void,wxCalendarDateAttr_SetHoliday)(wxCalendarDateAttr* self,bool holiday)
{
	self->SetHoliday(holiday);
}
	
EWXWEXPORT(bool,wxCalendarDateAttr_HasTextColour)(wxCalendarDateAttr* self)
{
	return self->HasTextColour();
}
	
EWXWEXPORT(bool,wxCalendarDateAttr_HasBackgroundColour)(wxCalendarDateAttr* self)
{
	return self->HasBackgroundColour();
}
	
EWXWEXPORT(bool,wxCalendarDateAttr_HasBorderColour)(wxCalendarDateAttr* self)
{
	return self->HasBorderColour();
}
	
EWXWEXPORT(bool,wxCalendarDateAttr_HasFont)(wxCalendarDateAttr* self)
{
	return self->HasFont();
}
	
EWXWEXPORT(bool,wxCalendarDateAttr_HasBorder)(wxCalendarDateAttr* self)
{
	return self->HasBorder();
}
	
EWXWEXPORT(bool,wxCalendarDateAttr_IsHoliday)(wxCalendarDateAttr* self)
{
	return self->IsHoliday();
}
	
EWXWEXPORT(void,wxCalendarDateAttr_GetTextColour)(wxCalendarDateAttr* self,wxColour* _ref)
{
	*_ref = self->GetTextColour();
}
	
EWXWEXPORT(void,wxCalendarDateAttr_GetBackgroundColour)(wxCalendarDateAttr* self,wxColour* _ref)
{
	*_ref = self->GetBackgroundColour();
}
	
EWXWEXPORT(void,wxCalendarDateAttr_GetBorderColour)(wxCalendarDateAttr* self,wxColour* _ref)
{
	*_ref = self->GetBorderColour();
}
	
EWXWEXPORT(void,wxCalendarDateAttr_GetFont)(wxCalendarDateAttr* self,wxFont* _ref)
{
	*_ref = self->GetFont();
}
	
EWXWEXPORT(int,wxCalendarDateAttr_GetBorder)(wxCalendarDateAttr* self)
{
	return (int)self->GetBorder();
}

}
