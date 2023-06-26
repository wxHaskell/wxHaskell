#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxColourData*,wxColourData_Create)()
{
	return new wxColourData();
}

EWXWEXPORT(void,wxColourData_Delete)(wxColourData* self)
{
	delete self;
}

EWXWEXPORT(void,wxColourData_SetChooseFull)(wxColourData* self,bool flag)
{
	self->SetChooseFull(flag);
}
	
EWXWEXPORT(bool,wxColourData_GetChooseFull)(wxColourData* self)
{
	return self->GetChooseFull();
}
	
EWXWEXPORT(void,wxColourData_SetColour)(wxColourData* self,wxColour* colour)
{
	self->SetColour(*colour);
}
	
EWXWEXPORT(void,wxColourData_GetColour)(wxColourData* self,wxColour* _ref)
{
	*_ref = self->GetColour();
}
	
EWXWEXPORT(void,wxColourData_SetCustomColour)(wxColourData* self,int i,wxColour* colour)
{
	self->SetCustomColour(i,*colour);
}
	
EWXWEXPORT(void,wxColourData_GetCustomColour)(wxColourData* self,int i,wxColour* _ref)
{
	*_ref = self->GetCustomColour(i);
}
	
} 
