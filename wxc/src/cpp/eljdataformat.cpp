#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxDataFormat*,wxDataFormat_CreateFromId)(wxString* name)
{
	return new wxDataFormat (*name);
}

EWXWEXPORT(wxDataFormat*,wxDataFormat_CreateFromType)(int typ)
{
	return new wxDataFormat ((wxDataFormat::NativeFormat)typ);
}

EWXWEXPORT(void,wxDataFormat_Delete)(wxDataFormat* self)
{
	delete self;
}

EWXWEXPORT(bool,wxDataFormat_IsEqual)(wxDataFormat* self,wxDataFormat* other)
{
	return  *self == *other;
}

EWXWEXPORT(wxString*,wxDataFormat_GetId)(wxDataFormat* self)
{
	wxString *result = new wxString();
	*result = self->GetId();
	return result;
}

EWXWEXPORT(int,wxDataFormat_GetType)(wxDataFormat* self)
{
	return (int)self->GetType();
}

EWXWEXPORT(void,wxDataFormat_SetId)(wxDataFormat* self,wxString* id)
{
	self->SetId(*id);
}

EWXWEXPORT(void,wxDataFormat_SetType)(wxDataFormat* self,int typ)
{
#ifdef __WIN32__
	self->SetType((wxDataFormat::NativeFormat)typ);
#else
	self->SetType((wxDataFormatId)typ);
#endif
}

}
