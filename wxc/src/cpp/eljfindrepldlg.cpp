/*==============================================================================
 * eljfindrepdlg.cpp
 *
 * C wrapper for wxFindReplaceData
 *
 * (C) 2002-2011 wxEiffel and wxHaskell contributors. See contributors.txt
 * 
 *==============================================================================*/

#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/fdrepdlg.h"

extern "C"
{

////////////////////////////////////////////////////////////////////////////////
// Event Handlers
////////////////////////////////////////////////////////////////////////////////

/* #pragma message "wxWidgets find/replace dialog event wrappers generated" */
MAKE_EVENT_WRAPPER(EVT_COMMAND_FIND)
MAKE_EVENT_WRAPPER(EVT_COMMAND_FIND_NEXT)
MAKE_EVENT_WRAPPER(EVT_COMMAND_FIND_REPLACE)
MAKE_EVENT_WRAPPER(EVT_COMMAND_FIND_REPLACE_ALL)
MAKE_EVENT_WRAPPER(EVT_COMMAND_FIND_CLOSE)

////////////////////////////////////////////////////////////////////////////////
// Wrappers
////////////////////////////////////////////////////////////////////////////////

EWXWEXPORT(void*,wxFindReplaceData_CreateDefault)()
{
	return (void*)new wxFindReplaceData();
}
	
EWXWEXPORT(void*,wxFindReplaceData_Create)(int flags)
{
	return (void*)new wxFindReplaceData((wxUint32)flags);
}

EWXWEXPORT(void,wxFindReplaceData_Delete)(void* _obj)
{
	delete (wxFindReplaceData*)_obj;
}
	
EWXWEXPORT(wxString*,wxFindReplaceData_GetFindString)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxFindReplaceData*)_obj)->GetFindString();
	return result;
}
	
EWXWEXPORT(wxString*,wxFindReplaceData_GetReplaceString)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxFindReplaceData*)_obj)->GetReplaceString();
	return result;
}
	
EWXWEXPORT(int,wxFindReplaceData_GetFlags)(void* _obj)
{
	return ((wxFindReplaceData*)_obj)->GetFlags();
}
	
EWXWEXPORT(void,wxFindReplaceData_SetFlags)(void* _obj,int flags)
{
	((wxFindReplaceData*)_obj)->SetFlags((wxUint32)flags);
}
	
EWXWEXPORT(void,wxFindReplaceData_SetFindString)(void* _obj,wxString* str)
{
	((wxFindReplaceData*)_obj)->SetFindString(*str);
}
	
EWXWEXPORT(void,wxFindReplaceData_SetReplaceString)(void* _obj,wxString* str)
{
	((wxFindReplaceData*)_obj)->SetReplaceString(*str);
}
	

EWXWEXPORT(int,wxFindDialogEvent_GetFlags)(void* _obj)
{
	return ((wxFindDialogEvent*)_obj)->GetFlags();
}
	
EWXWEXPORT(wxString*,wxFindDialogEvent_GetFindString)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxFindReplaceData*)_obj)->GetFindString();
	return result;
}
	
EWXWEXPORT(wxString*,wxFindDialogEvent_GetReplaceString)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxFindReplaceData*)_obj)->GetFindString();
	return result;
}
	

EWXWEXPORT(void*,wxFindReplaceDialog_Create)(wxWindow* parent,void* data,wxString* title,int style)
{
	return (void*)new wxFindReplaceDialog(parent, (wxFindReplaceData*)data, *title, style);
}
	
EWXWEXPORT(void*,wxFindReplaceDialog_GetData)(void* _obj)
{
	return (void*)((wxFindReplaceDialog*)_obj)->GetData();
}
	
EWXWEXPORT(void,wxFindReplaceDialog_SetData)(void* _obj,void* data)
{
	((wxFindReplaceDialog*)_obj)->SetData((wxFindReplaceData*)data);
}
	
}
#endif
