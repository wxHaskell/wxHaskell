#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void,wxControl_SetLabel)(wxControl* self,wxString* text)
{
	self->SetLabel(*text);
}
	
EWXWEXPORT(wxString*,wxControl_GetLabel)(wxControl* self)
{
	wxString *result = new wxString();
	*result = self->GetLabel();
	return result;
}

EWXWEXPORT(void,wxControl_Command)(wxControl* self,wxCommandEvent* event)
{
	self->Command(*event);
}

}
