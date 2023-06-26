#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxFontData_Create)()
{
	return (void*)new wxFontData();
}

EWXWEXPORT(void,wxFontData_Delete)(void* self)
{
	delete (wxFontData*)self;
}

EWXWEXPORT(void,wxFontData_SetAllowSymbols)(void* self,bool flag)
{
	((wxFontData*)self)->SetAllowSymbols(flag);
}
	
EWXWEXPORT(bool,wxFontData_GetAllowSymbols)(wxFontData* self)
{
	return self->GetAllowSymbols();
}
	
EWXWEXPORT(void,wxFontData_SetColour)(void* self,wxColour* colour)
{
	((wxFontData*)self)->SetColour(*colour);
}
	
EWXWEXPORT(void,wxFontData_GetColour)(void* self,wxColour* _ref)
{
	*_ref = ((wxFontData*)self)->GetColour();
}
	
EWXWEXPORT(void,wxFontData_SetShowHelp)(void* self,bool flag)
{
	((wxFontData*)self)->SetShowHelp(flag);
}
	
EWXWEXPORT(bool,wxFontData_GetShowHelp)(wxFontData* self)
{
	return self->GetShowHelp();
}
	
EWXWEXPORT(void,wxFontData_EnableEffects)(void* self,bool flag)
{
	((wxFontData*)self)->EnableEffects(flag);
}
	
EWXWEXPORT(bool,wxFontData_GetEnableEffects)(wxFontData* self)
{
	return self->GetEnableEffects();
}
	
EWXWEXPORT(void,wxFontData_SetInitialFont)(void* self,wxFont* font)
{
	((wxFontData*)self)->SetInitialFont(*font);
}
	
EWXWEXPORT(void,wxFontData_GetInitialFont)(void* self,wxFont* ref)
{
	*ref = ((wxFontData*)self)->GetInitialFont();
}
	
EWXWEXPORT(void,wxFontData_SetChosenFont)(void* self,wxFont* font)
{
	((wxFontData*)self)->SetChosenFont(*font);
}
	
EWXWEXPORT(void,wxFontData_GetChosenFont)(void* self,wxFont* ref)
{
	*ref = ((wxFontData*)self)->GetChosenFont();
}
	
EWXWEXPORT(void,wxFontData_SetRange)(void* self,int minRange,int maxRange)
{
	((wxFontData*)self)->SetRange(minRange, maxRange);
}
	
EWXWEXPORT(int,wxFontData_GetEncoding)(wxFontData* self)
{
	return (int)self->GetEncoding();
}
	
EWXWEXPORT(void,wxFontData_SetEncoding)(void* self,int encoding)
{
	((wxFontData*)self)->SetEncoding((wxFontEncoding)encoding);
}
	
}
