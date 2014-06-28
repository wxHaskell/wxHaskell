#include "wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/artprov.h"

extern "C"
{

EWXWEXPORT(void*,wxIconBundle_CreateDefault)()
{
	return (void*)new wxIconBundle();
}
	
EWXWEXPORT(void*,wxIconBundle_CreateFromFile)(wxString* file,int type)
{
	return (void*)new wxIconBundle(*file, (wxBitmapType)type);
}
	
EWXWEXPORT(void*,wxIconBundle_CreateFromIcon)(void* icon)
{
	return (void*) new wxIconBundle(*((wxIcon*)icon));
}
	
EWXWEXPORT(void,wxIconBundle_Assign)(void* _obj,void* _ref)
{
	*((wxIconBundle*)_ref) = *((wxIconBundle*)_obj);
}
	
EWXWEXPORT(void,wxIconBundle_Delete)(void* _obj)
{
	delete (wxIconBundle*)_obj;
}
	
EWXWEXPORT(void,wxIconBundle_AddIconFromFile)(void* _obj,wxString* file,int type)
{
	((wxIconBundle*)_obj)->AddIcon(*file, (wxBitmapType)type);
}
	
EWXWEXPORT(void,wxIconBundle_AddIcon)(void* _obj,void* icon)
{
	((wxIconBundle*)_obj)->AddIcon(*((wxIcon*)icon));
}
	
EWXWEXPORT(void,wxIconBundle_GetIcon)(void* _obj,int w,int h,void* _ref)
{
	*((wxIcon*)_ref) = ((wxIconBundle*)_obj)->GetIcon(wxSize(w, h));
}

}
#endif
