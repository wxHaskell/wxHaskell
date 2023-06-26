#include "wxc/wrapper.h"
#include "wx/busyinfo.h"

extern "C"
{

EWXWEXPORT(void*,wxBusyInfo_Create)(wxString* _txt)
{
	return (void*) new wxBusyInfo (*_txt);
}

EWXWEXPORT(void,wxBusyInfo_Delete)(wxBusyInfo* _obj)
{
	delete _obj;
}

EWXWEXPORT(void*,wxBusyCursor_Create)()
{
	return (void*) new wxBusyCursor ();
}

EWXWEXPORT(void*,wxBusyCursor_CreateWithCursor)(void* _cur)
{
	return (void*) new wxBusyCursor ((wxCursor*)_cur);
}

EWXWEXPORT(void,wxBusyCursor_Delete)(wxBusyCursor* _obj)
{
	delete _obj;
}

}
