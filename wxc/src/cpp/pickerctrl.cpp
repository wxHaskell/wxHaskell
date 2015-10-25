#include "wrapper.h"
#include <wx/clrpicker.h>


extern "C"
{

EWXWEXPORT( wxColourPickerCtrl*,wxColorPickerCtrl_Create)(wxWindow* parent,int id,const wxColour& colour, int x, int y, int w, int h, int style)
{
	return new wxColourPickerCtrl(parent, id, colour, wxPoint(x, y), wxSize(w, h), style);
}

EWXWEXPORT( void,wxColorPickerCtrl_GetColour)(wxColourPickerCtrl* self, wxColour* colour)
{
	*colour = self->GetColour();
}
 
EWXWEXPORT( void,wxColorPickerCtrl_SetColour)(wxColourPickerCtrl* self,const wxColour &colour)
{
	self->SetColour(colour);
}

} 

