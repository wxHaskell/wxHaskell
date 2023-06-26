#include "wxc/wrapper.h"
#include "wx/mimetype.h"

extern "C"
{

EWXWEXPORT(void*,wxMimeTypesManager_Create)()
{
        return (void*)wxTheMimeTypesManager;
}

EWXWEXPORT(void*,wxMimeTypesManager_GetFileTypeFromExtension)(wxMimeTypesManager* self,wxString* _ext)
{
        return (void*)self->GetFileTypeFromExtension(*_ext);
}

EWXWEXPORT(void*,wxMimeTypesManager_GetFileTypeFromMimeType)(wxMimeTypesManager* self,wxString* _name)
{
        return (void*)self->GetFileTypeFromMimeType(*_name);
}

EWXWEXPORT(int,wxMimeTypesManager_EnumAllFileTypes)(wxMimeTypesManager* self,void* _lst)
{
        wxArrayString arr;
        int result = (int)self->EnumAllFileTypes(arr);

        if (_lst)
        {
                for (unsigned int i = 0; i < arr.GetCount(); i++)
                        ((const wxChar**)_lst)[i] = wxStrdup (arr.Item(i).wchar_str());
        }

        return result;
}

EWXWEXPORT(void,wxMimeTypesManager_AddFallbacks)(wxMimeTypesManager* self,void* _types)
{
        self->AddFallbacks((const wxFileTypeInfo*)_types);
}

EWXWEXPORT(bool,wxMimeTypesManager_IsOfType)(wxMimeTypesManager* self,wxString* _type,wxString* _wildcard)
{
        return self->IsOfType (*_type, *_wildcard);
}


EWXWEXPORT(wxString*,wxFileType_GetMimeType)(wxFileType* self)
{
        wxString *result = new wxString();
        if (self->GetMimeType(result)!=true)
          result->Clear();
        return result;
}

EWXWEXPORT(int,wxFileType_GetMimeTypes)(void* self,void* _lst)
{
        wxArrayString arr;

        if (((wxFileType*)self)->GetMimeTypes(arr) && _lst)
        {
                for (unsigned int i = 0; i < arr.GetCount(); i++)
                        ((const wxChar**)_lst)[i] = wxStrdup (arr.Item(i).wchar_str());
        }

        return arr.GetCount();
}

EWXWEXPORT(int,wxFileType_GetExtensions)(void* self,void* _lst)
{
        wxArrayString arr;

        if (((wxFileType*)self)->GetExtensions(arr) && _lst)
        {
            for (unsigned int i = 0; i < arr.GetCount(); i++)                
                 ((const wxChar**)_lst)[i] = wxStrdup (arr.Item(i).wchar_str());
        }

        return arr.GetCount();
}

EWXWEXPORT(bool,wxFileType_GetIcon)(wxFileType* self,wxIcon* icon)
{
#if wxCHECK_VERSION(2,5,0)
	wxIconLocation iconLoc;
	bool res = self->GetIcon(&iconLoc);
	*icon = wxIcon(iconLoc);
	return res;
#else
	return self->GetIcon(icon);
#endif
}

EWXWEXPORT(wxString*,wxFileType_GetDescription)(wxFileType* self)
{
        wxString *result = new wxString();
        if (self->GetDescription(result) != true)
          result->Clear();
        return result;
}

EWXWEXPORT(wxString*,wxFileType_GetOpenCommand)(wxFileType* self,void* _params)
{
        wxString *result = new wxString();
        if (self->GetOpenCommand(result, *((wxFileType::MessageParameters*)_params)) != true)
          result->Clear();
        return result;
}

EWXWEXPORT(wxString*,wxFileType_GetPrintCommand)(wxFileType* self,void* _params)
{
        wxString *result = new wxString();
        if (self->GetPrintCommand(result, *((wxFileType::MessageParameters*)_params)) != true)
          result->Clear();
        return result;
}

EWXWEXPORT(wxString*,wxFileType_ExpandCommand)(wxFileType* self,void* _cmd,void* _params)
{
        wxString *result = new wxString();
        *result = self->ExpandCommand((const wxChar*)_cmd, *((wxFileType::MessageParameters*)_params));
        return result;
}

EWXWEXPORT(void,wxFileType_Delete)(void* self)
{
        delete (wxFileType*)self;
}


}
