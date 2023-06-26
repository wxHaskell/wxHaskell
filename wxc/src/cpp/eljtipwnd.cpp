#include "wxc/wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/tipwin.h"

extern "C"
{

EWXWEXPORT(void*,wxTipWindow_Create)(wxWindow* parent,wxString* text,int maxLength)
{
	return (void*)new wxTipWindow(parent, *text, (wxCoord)maxLength);
}
	
EWXWEXPORT(void,wxTipWindow_SetTipWindowPtr)(void* _obj,void* windowPtr)
{
	((wxTipWindow*)_obj)->SetTipWindowPtr((wxTipWindow**)windowPtr);
}
	
EWXWEXPORT(void,wxTipWindow_SetBoundingRect)(void* _obj,int x,int y,int w,int h)
{
	((wxTipWindow*)_obj)->SetBoundingRect(wxRect(x, y, w, h));
}
	
EWXWEXPORT(void,wxTipWindow_Close)(void* _obj)
{
	((wxTipWindow*)_obj)->Close();
}

}
#endif
