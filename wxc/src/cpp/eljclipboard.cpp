#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxClipboard*,wxClipboard_Create)()
{
	return wxTheClipboard;
}

EWXWEXPORT(void,wxClipboard_Delete)(wxClipboard* self)
{
	// delete _obj;
}

EWXWEXPORT(bool,wxClipboard_Open)(wxClipboard* self)
{
	return self->Open();
}

EWXWEXPORT(void,wxClipboard_Close)(wxClipboard* self)
{
	self->Close();
}

EWXWEXPORT(bool,wxClipboard_IsOpened)(wxClipboard* self)
{
	return self->IsOpened();
}

EWXWEXPORT(bool,wxClipboard_SetData)(wxClipboard* self,wxDataObject* data)
{
	return self->SetData(data);
}

EWXWEXPORT(bool,wxClipboard_AddData)(wxClipboard* self,wxDataObject* data)
{
	return self->AddData(data);
}

EWXWEXPORT(bool,wxClipboard_IsSupported)(wxClipboard* self,wxDataFormat* format)
{
	return self->IsSupported(*format);
}

EWXWEXPORT(bool,wxClipboard_GetData)(wxClipboard* self,wxDataObject* data)
{
	return self->GetData(*data);
}

EWXWEXPORT(void,wxClipboard_Clear)(wxClipboard* self)
{
	self->Clear();
}

EWXWEXPORT(bool,wxClipboard_Flush)(wxClipboard* self)
{
	return self->Flush();
}

EWXWEXPORT(void,wxClipboard_UsePrimarySelection)(wxClipboard* self,bool primary)
{
	self->UsePrimarySelection (primary);
}

}
