#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxConfigBase*,wxConfigBase_Create)()
{
	return wxConfigBase::Create();
}
	
EWXWEXPORT(void,wxConfigBase_Delete)(wxConfigBase* self)
{
	delete self;
}

EWXWEXPORT(void,wxConfigBase_SetPath)(wxConfigBase* self,wxString* strPath)
{
	self->SetPath(*strPath);
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetPath)(wxConfigBase* self)
{
	wxString *result = new wxString();
	*result = self->GetPath();
	return result;
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetFirstGroup)(wxConfigBase* self,long* lIndex)
{
	wxString* tmp;
        tmp = new wxString(wxT(""));
        if (self->GetFirstGroup(*tmp,*lIndex)) {
          *lIndex = -1;
        }         
	return tmp;
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetNextGroup)(wxConfigBase* self,long* lIndex)
{
	wxString* tmp;
        tmp = new wxString(wxT(""));
        if (self->GetNextGroup(*tmp,*lIndex)) {
          *lIndex = -1;
        }         
	return tmp;
}

	
EWXWEXPORT(wxString*,wxConfigBase_GetFirstEntry)(wxConfigBase* self,long* lIndex)
{
	wxString* tmp;
        tmp = new wxString(wxT(""));
        if (self->GetFirstEntry(*tmp,*lIndex)) {
          *lIndex = -1;
        }         
	return tmp;
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetNextEntry)(wxConfigBase* self,long* lIndex)
{
	wxString* tmp;
        tmp = new wxString(wxT(""));
        if (self->GetNextEntry(*tmp,*lIndex)) {
          *lIndex = -1;
        }         
	return tmp;
}
	
EWXWEXPORT(size_t,wxConfigBase_GetNumberOfEntries)(wxConfigBase* self,bool bRecursive)
{
	return self->GetNumberOfEntries(bRecursive);
}
	
EWXWEXPORT(size_t,wxConfigBase_GetNumberOfGroups)(wxConfigBase* self,bool bRecursive)
{
	return self->GetNumberOfGroups(bRecursive);
}
	
EWXWEXPORT(bool,wxConfigBase_HasGroup)(wxConfigBase* self,wxString* strName)
{
	return self->HasGroup(*strName);
}
	
EWXWEXPORT(bool,wxConfigBase_HasEntry)(wxConfigBase* self,wxString* strName)
{
	return self->HasEntry(*strName);
}
	
EWXWEXPORT(bool,wxConfigBase_Exists)(wxConfigBase* self,wxString* strName)
{
	return self->Exists(*strName);
}
	
EWXWEXPORT(int,wxConfigBase_GetEntryType)(wxConfigBase* self,wxString* name)
{
	return (int)self->GetEntryType(*name);
}
	
EWXWEXPORT(wxString*,wxConfigBase_ReadString)(wxConfigBase* self,wxString* key,wxString* defVal)
{
	wxString tmp;
        tmp = self->Read(*key,*defVal);
	return new wxString(tmp);
}
	
EWXWEXPORT(int,wxConfigBase_ReadInteger)(wxConfigBase* self,wxString* key,int defVal)
{
	return self->Read(*key, defVal);
}
	
EWXWEXPORT(double,wxConfigBase_ReadDouble)(wxConfigBase* self,wxString* key,double defVal)
{
	double val;
	if (self->Read(*key, &val, defVal))
 		return val;
 	return 0.0;
}
	
EWXWEXPORT(bool,wxConfigBase_ReadBool)(wxConfigBase* self,wxString* key,bool defVal)
{
	bool val;
	if (self->Read(*key, &val, defVal))
		return val;
	return false;
}
	
EWXWEXPORT(bool,wxConfigBase_WriteString)(wxConfigBase* self,wxString* key,wxString* value)
{
	return self->Write(*key,*value);
}
	
// FIXME: just left for backward-compatibiliry. wxHaskell uses int as long now.
EWXWEXPORT(bool,wxConfigBase_WriteInteger)(wxConfigBase* self,wxString* key,int value)
{
	return self->Write(*key, (long)value);
}
	
EWXWEXPORT(bool,wxConfigBase_WriteLong)(wxConfigBase* self,wxString* key,long value)
{
	return self->Write(*key, value);
}
	
EWXWEXPORT(bool,wxConfigBase_WriteDouble)(wxConfigBase* self,wxString* key,double value)
{
	return self->Write(*key, value);
}
	
EWXWEXPORT(bool,wxConfigBase_WriteBool)(wxConfigBase* self,wxString* key,bool value)
{
	return self->Write(*key, value);
}
	
EWXWEXPORT(bool,wxConfigBase_Flush)(wxConfigBase* self,bool bCurrentOnly)
{
	return self->Flush(bCurrentOnly);
}
	
EWXWEXPORT(bool,wxConfigBase_RenameEntry)(wxConfigBase* self,wxString* oldName,wxString* newName)
{
	return self->RenameEntry(*oldName,*newName);
}
	
EWXWEXPORT(bool,wxConfigBase_RenameGroup)(wxConfigBase* self,wxString* oldName,wxString* newName)
{
	return self->RenameGroup(*oldName,*newName);
}
	
EWXWEXPORT(bool,wxConfigBase_DeleteEntry)(wxConfigBase* self,wxString* key,bool bDeleteGroupIfEmpty)
{
	return self->DeleteEntry(*key, bDeleteGroupIfEmpty);
}
	
EWXWEXPORT(bool,wxConfigBase_DeleteGroup)(wxConfigBase* self,wxString* key)
{
	return self->DeleteGroup(*key);
}
	
EWXWEXPORT(bool,wxConfigBase_DeleteAll)(wxConfigBase* self)
{
	return self->DeleteAll();
}
	
EWXWEXPORT(bool,wxConfigBase_IsExpandingEnvVars)(wxConfigBase* self)
{
	return self->IsExpandingEnvVars();
}
	
EWXWEXPORT(void,wxConfigBase_SetExpandEnvVars)(wxConfigBase* self,bool bDoIt)
{
	self->SetExpandEnvVars(bDoIt);
}
	
EWXWEXPORT(void,wxConfigBase_SetRecordDefaults)(wxConfigBase* self,bool bDoIt)
{
	self->SetRecordDefaults(bDoIt);
}
	
EWXWEXPORT(bool,wxConfigBase_IsRecordingDefaults)(wxConfigBase* self)
{
	return self->IsRecordingDefaults();
}
	
EWXWEXPORT(wxString*,wxConfigBase_ExpandEnvVars)(wxConfigBase* self,wxString* str)
{
	wxString *result = new wxString();
	*result = self->ExpandEnvVars(*str);
	return result;
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetAppName)(wxConfigBase* self)
{
	wxString *result = new wxString();
	*result = self->GetAppName();
	return result;
}
	
EWXWEXPORT(wxString*,wxConfigBase_GetVendorName)(wxConfigBase* self)
{
	wxString *result = new wxString();
	*result = self->GetVendorName();
	return result;
}
	
EWXWEXPORT(void,wxConfigBase_SetAppName)(wxConfigBase* self,wxString* appName)
{
	self->SetAppName(*appName);
}
	
EWXWEXPORT(void,wxConfigBase_SetVendorName)(wxConfigBase* self,wxString* vendorName)
{
	self->SetVendorName(*vendorName);
}
	
EWXWEXPORT(void,wxConfigBase_SetStyle)(wxConfigBase* self,int style)
{
	self->SetStyle((long)style);
}
	
EWXWEXPORT(long,wxConfigBase_GetStyle)(wxConfigBase* self)
{
	return self->GetStyle();
}
	
}
