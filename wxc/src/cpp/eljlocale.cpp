#include "wxc/wrapper.h"
#include "wx/intl.h"

extern "C"
{

EWXWEXPORT(void*,wxLocale_Create)(int _lang,int _flags)
{
	return (void*)new wxLocale(_lang, _flags);
}

EWXWEXPORT(void,wxLocale_Delete)(wxLocale* self)
{
	delete self;
}

EWXWEXPORT(bool,wxLocale_IsOk)(wxLocale* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(void*,wxLocale_GetLocale)(wxLocale* self)
{
#if (wxVERSION_NUMBER < 2900)
	return (void*)self->GetLocale();
#else
    wxString retVal = self->GetLocale();
    return (void*) retVal.wchar_str();
#endif
}
	
EWXWEXPORT(void,wxLocale_AddCatalogLookupPathPrefix)(wxLocale* self,void* prefix)
{
	self->AddCatalogLookupPathPrefix((const wxChar*)prefix);
}
	
EWXWEXPORT(bool,wxLocale_AddCatalog)(wxLocale* self,void* szDomain)
{
	return self->AddCatalog((const wxChar*)szDomain);
}
	
EWXWEXPORT(bool,wxLocale_IsLoaded)(wxLocale* self,void* szDomain)
{
	return self->IsLoaded((const wxChar*)szDomain);
}
	
EWXWEXPORT(void*,wxLocale_GetString)(wxLocale* self,void* szOrigString,void* szDomain)
{
#if (wxVERSION_NUMBER < 2900)
	return (void*)self->GetString((const wxChar*)szOrigString, (const wxChar*)szDomain);
#else
    wxString retVal = self->GetString((const wxChar*)szOrigString, (const wxChar*)szDomain);
    return (void*) retVal.wchar_str();
#endif
}
	
EWXWEXPORT(wxString*,wxLocale_GetName)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxLocale*)_obj)->GetName();
	return result;
}
	

EWXWEXPORT(void*,wxGetELJLocale)()
{
	return (void*)wxGetLocale();
}
	
EWXWEXPORT(void*,wxGetELJTranslation)(void* sz)
{
#if (wxVERSION_NUMBER < 2900)
    return (void*)wxGetTranslation((const wxChar*)sz);
#else
    wxString retVal = wxGetTranslation((const wxChar*) sz);
    return (void*) retVal.wchar_str();
#endif
}
	
}
