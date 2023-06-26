#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxScrollBar_Create)(wxWindow* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*)new wxScrollBar ((wxWindow*)_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int,wxScrollBar_GetThumbPosition)(wxScrollBar* self)
{
	return self->GetThumbPosition();
}

EWXWEXPORT(int,wxScrollBar_GetThumbSize)(wxScrollBar* self)
{
	return self->GetThumbSize();
}

EWXWEXPORT(int,wxScrollBar_GetPageSize)(wxScrollBar* self)
{
	return self->GetPageSize();
}

EWXWEXPORT(int,wxScrollBar_GetRange)(wxScrollBar* self)
{
	return self->GetRange();
}

EWXWEXPORT(void,wxScrollBar_SetThumbPosition)(wxScrollBar* self,int viewStart)
{
	self->SetThumbPosition(viewStart);
}

EWXWEXPORT(void,wxScrollBar_SetScrollbar)(wxScrollBar* self,int position,int thumbSize,int range,int pageSize,bool refresh)
{
	self->SetScrollbar(position, thumbSize, range, pageSize, refresh);
}

}
