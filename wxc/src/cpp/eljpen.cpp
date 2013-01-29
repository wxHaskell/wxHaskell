#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxPen_CreateDefault)()
{
	return new wxPen();
}

EWXWEXPORT(void*,wxPen_CreateFromColour)(wxColour* col,int width,int style)
{
	return new wxPen(*col, width, style);
}

EWXWEXPORT(void*,wxPen_CreateFromBitmap)(wxBitmap* stipple,int width)
{
#ifdef __WXGTK__
        return 0;
#else
	return new wxPen(*stipple, width);
#endif
}

EWXWEXPORT(void*,wxPen_CreateFromStock)(int id)
{
	switch (id)
	{
		case 0:
			return (void*)wxRED_PEN;
		case 1:
			return (void*)wxCYAN_PEN;
		case 2:
			return (void*)wxGREEN_PEN;
		case 3:
			return (void*)wxBLACK_PEN;
		case 4:
			return (void*)wxWHITE_PEN;
		case 5:
			return (void*)wxTRANSPARENT_PEN;
		case 6:
			return (void*)wxBLACK_DASHED_PEN;
		case 7:
			return (void*)wxGREY_PEN;
		case 8:
			return (void*)wxMEDIUM_GREY_PEN;
		case 9:
			return (void*)wxLIGHT_GREY_PEN;
	}
	
	return NULL;
}

EWXWEXPORT(void,wxPen_Delete)(void* self)
{
	delete (wxPen*)self;
}

EWXWEXPORT(void,wxPen_Assign)(void* self,void* pen)
{
	*((wxPen*)self) = *((wxPen*)pen);
}
	
EWXWEXPORT(bool,wxPen_IsEqual)(wxPen* self,wxPen* pen)
{
	return *self == *pen;
}
	
EWXWEXPORT(bool,wxPen_IsOk)(wxPen* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(void,wxPen_SetColour)(void* self,wxColour* col)
{
	((wxPen*)self)->SetColour(*col);
}
	
EWXWEXPORT(void,wxPen_SetColourSingle)(void* self,wxUint8 r,wxUint8 g,wxUint8 b)
{
	((wxPen*)self)->SetColour(r,g,b);
}
	
EWXWEXPORT(void,wxPen_SetWidth)(void* self,int width)
{
	((wxPen*)self)->SetWidth(width);
}
	
EWXWEXPORT(void,wxPen_SetStyle)(void* self,int style)
{
	((wxPen*)self)->SetStyle(style);
}
	
EWXWEXPORT(void,wxPen_SetStipple)(void* self,wxBitmap* stipple)
{
#if !defined(__WXGTK__)
    ((wxPen*)self)->SetStipple(*stipple);
#endif
}
	
EWXWEXPORT(void,wxPen_SetDashes)(void* self,int nb_dashes,void* dash)
{
	((wxPen*)self)->SetDashes(nb_dashes, (wxDash*)dash);
}
	
EWXWEXPORT(void,wxPen_SetJoin)(void* self,int join)
{
#if (wxVERSION_NUMBER < 2900)
    int _join = join;
#else
    wxPenJoin _join = (wxPenJoin) join;
#endif
	((wxPen*)self)->SetJoin(_join);
}
	
EWXWEXPORT(void,wxPen_SetCap)(void* self,int cap)
{
#if (wxVERSION_NUMBER < 2900)
    int _cap = cap;
#else
    wxPenCap _cap = (wxPenCap) cap;
#endif
	((wxPen*)self)->SetCap(_cap);
}
	
EWXWEXPORT(void,wxPen_GetColour)(void* self,wxColour* _ref)
{
	*_ref = ((wxPen*)self)->GetColour();
}
	
EWXWEXPORT(int,wxPen_GetWidth)(void* self)
{
	return ((wxPen*)self)->GetWidth();
}
	
EWXWEXPORT(int,wxPen_GetStyle)(void* self)
{
	return ((wxPen*)self)->GetStyle();
}
	
EWXWEXPORT(int,wxPen_GetJoin)(void* self)
{
  return (int) ((wxPen*)self)->GetJoin();
}
	
EWXWEXPORT(int,wxPen_GetCap)(void* self)
{
  return (int) ((wxPen*)self)->GetCap();
}
	
EWXWEXPORT(int,wxPen_GetDashes)(void* self,void* ptr)
{
	return ((wxPen*)self)->GetDashes((wxDash**)ptr);
}
	
EWXWEXPORT(void,wxPen_GetStipple)(void* self,wxBitmap* _ref)
{
#if defined(__WXGTK__)
    *_ref = (GdkPixbuf*)NULL;
#else
    *_ref = *(((wxPen*)self)->GetStipple());
#endif
}
	
}
