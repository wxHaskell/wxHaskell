#include "wxc/wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/snglinst.h"

extern "C"
{

EWXWEXPORT(void*,wxSingleInstanceChecker_CreateDefault)()
{
	return (void*)new wxSingleInstanceChecker();
}
	
EWXWEXPORT(bool,wxSingleInstanceChecker_Create)(wxSingleInstanceChecker* self,wxString* name,wxString* path)
{
	return self->Create(*name,*path);
}
	
EWXWEXPORT(bool,wxSingleInstanceChecker_IsAnotherRunning)(wxSingleInstanceChecker* self)
{
	return self->IsAnotherRunning();
}
	
EWXWEXPORT(void,wxSingleInstanceChecker_Delete)(void* self)
{
	delete (wxSingleInstanceChecker*)self;
}
	
}
#endif
