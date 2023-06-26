#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxMessageDialog_Create)(wxWindow* _prt,wxString* _msg,wxString* _cap,int _stl)
{
	return (void*) new wxMessageDialog(_prt, *_msg, *_cap, (long)_stl);
}

EWXWEXPORT(void,wxMessageDialog_Delete)(void* _obj)
{
	delete (wxMessageDialog*)_obj;
}

EWXWEXPORT(int,wxMessageDialog_ShowModal)(void* _obj)
{
	return ((wxMessageDialog*)_obj)->ShowModal();
}

}
