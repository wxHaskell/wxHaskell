#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxBrush*,wxBrush_CreateDefault)()
{
	return  new wxBrush();
}

EWXWEXPORT(wxBrush*,wxBrush_CreateFromBitmap)(wxBitmap* bitmap)
{
	return new wxBrush(*bitmap);
}

EWXWEXPORT(wxBrush*,wxBrush_CreateFromColour)(wxColour* col,int style)
{
	return new wxBrush(*col, style);
}

EWXWEXPORT(void*,wxBrush_CreateFromStock)(int id)
{
	switch (id)
	{
		case 0:
			return (void*)wxBLUE_BRUSH;
		case 1:
			return (void*)wxGREEN_BRUSH;
		case 2:
			return (void*)wxWHITE_BRUSH;
		case 3:
			return (void*)wxBLACK_BRUSH;
		case 4:
			return (void*)wxGREY_BRUSH;
		case 5:
			return (void*)wxMEDIUM_GREY_BRUSH;
		case 6:
			return (void*)wxLIGHT_GREY_BRUSH;
		case 7:
			return (void*)wxTRANSPARENT_BRUSH;
		case 8:
			return (void*)wxCYAN_BRUSH;
		case 9:
			return (void*)wxRED_BRUSH;
	}
	
	return NULL;
}

EWXWEXPORT(void,wxBrush_Delete)(wxBrush* self)
{
	delete self;
}

EWXWEXPORT(void,wxBrush_SetColour)(wxBrush* self,wxColour* col)
{
	self->SetColour(*col);
}
	
EWXWEXPORT(void,wxBrush_SetColourSingle)(wxBrush* self,wxUint8 r,wxUint8 g,wxUint8 b)
{
	self->SetColour(r,g,b);
}
	
EWXWEXPORT(void,wxBrush_SetStyle)(wxBrush* self,int style)
{
	self->SetStyle(style);
}
	
EWXWEXPORT(void,wxBrush_SetStipple)(wxBrush* self,wxBitmap* stipple)
{
	self->SetStipple(*stipple);
}
	
EWXWEXPORT(void,wxBrush_Assign)(wxBrush* self,wxBrush* brush)
{
	*self = *brush;
}
	
EWXWEXPORT(bool,wxBrush_IsEqual)(wxBrush* self,wxBrush* brush)
{
	return *self == *brush;
}
	
EWXWEXPORT(void,wxBrush_GetColour)(wxBrush* self,wxColour* _ref)
{
	*_ref = self->GetColour();
}
	
EWXWEXPORT(int,wxBrush_GetStyle)(wxBrush* self)
{
	return self->GetStyle();
}
	
EWXWEXPORT(void,wxBrush_GetStipple)(wxBrush* self,wxBitmap* _ref)
{
	*_ref = *(self->GetStipple());
}
	
EWXWEXPORT(bool,wxBrush_IsOk)(wxBrush* self)
{
	return self->IsOk();
}
	
}
