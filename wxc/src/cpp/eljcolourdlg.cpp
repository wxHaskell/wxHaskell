#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxColourDialog*,wxColourDialog_Create)(wxWindow* _prt,wxColourData* col)
{
	return new wxColourDialog (_prt, col);
}

EWXWEXPORT(void,wxColourDialog_GetColourData)(wxColourDialog* self,wxColourData* col)
{
	*col = self->GetColourData();
}

}
