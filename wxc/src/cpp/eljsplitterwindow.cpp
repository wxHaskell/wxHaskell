#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxSplitterWindow_Create)(wxWindow* _prt, int _id, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*)new wxSplitterWindow (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void*,wxSplitterWindow_GetWindow1)(void* self)
{
	return (void*)((wxSplitterWindow*)self)->GetWindow1();
}
	
EWXWEXPORT(void*,wxSplitterWindow_GetWindow2)(void* self)
{
	return (void*)((wxSplitterWindow*)self)->GetWindow2();
}
	
EWXWEXPORT(void,wxSplitterWindow_SetSplitMode)(void* self,int mode)
{
	((wxSplitterWindow*)self)->SetSplitMode(mode);
}
	
EWXWEXPORT(int,wxSplitterWindow_GetSplitMode)(void* self)
{
	return ((wxSplitterWindow*)self)->GetSplitMode();
}
	
EWXWEXPORT(void,wxSplitterWindow_Initialize)(void* self,wxWindow* window)
{
	((wxSplitterWindow*)self)->Initialize(window);
}
	
EWXWEXPORT(bool,wxSplitterWindow_SplitVertically)(wxSplitterWindow* self,wxWindow* window1,wxWindow* window2,int sashPosition)
{
	return self->SplitVertically(window1, window2, sashPosition);
}
	
EWXWEXPORT(int,wxSplitterWindow_SplitHorizontally)(void* self,wxWindow* window1,wxWindow* window2,int sashPosition)
{
	return (int)((wxSplitterWindow*)self)->SplitHorizontally(window1, window2, sashPosition);
}
	
EWXWEXPORT(int,wxSplitterWindow_Unsplit)(void* self,wxWindow* toRemove)
{
	return (int)((wxSplitterWindow*)self)->Unsplit(toRemove);
}
	
EWXWEXPORT(int,wxSplitterWindow_ReplaceWindow)(void* self,wxWindow* winOld,wxWindow* winNew)
{
	return (int)((wxSplitterWindow*)self)->ReplaceWindow(winOld, winNew);
}
	
EWXWEXPORT(bool,wxSplitterWindow_IsSplit)(wxSplitterWindow* self)
{
	return self->IsSplit();
}
	
EWXWEXPORT(void,wxSplitterWindow_SetSashSize)(void* self,int width)
{
	((wxSplitterWindow*)self)->SetSashSize(width);
}
	
EWXWEXPORT(void,wxSplitterWindow_SetBorderSize)(void* self,int width)
{
	((wxSplitterWindow*)self)->SetBorderSize(width);
}
	
EWXWEXPORT(int,wxSplitterWindow_GetSashSize)(void* self)
{
	return ((wxSplitterWindow*)self)->GetSashSize();
}
	
EWXWEXPORT(int,wxSplitterWindow_GetBorderSize)(void* self)
{
	return ((wxSplitterWindow*)self)->GetBorderSize();
}
	
EWXWEXPORT(void,wxSplitterWindow_SetSashPosition)(void* self,int position,bool redraw)
{
	((wxSplitterWindow*)self)->SetSashPosition(position, redraw);
}
	
EWXWEXPORT(int,wxSplitterWindow_GetSashPosition)(void* self)
{
	return ((wxSplitterWindow*)self)->GetSashPosition();
}
	
EWXWEXPORT(void,wxSplitterWindow_SetMinimumPaneSize)(void* self,int min)
{
	((wxSplitterWindow*)self)->SetMinimumPaneSize(min);
}
	
EWXWEXPORT(int,wxSplitterWindow_GetMinimumPaneSize)(void* self)
{
	return ((wxSplitterWindow*)self)->GetMinimumPaneSize();
}

EWXWEXPORT(double,wxSplitterWindow_GetSashGravity)(void* self)
{
	return ((wxSplitterWindow*)self)->GetSashGravity();
}
	
EWXWEXPORT(void,wxSplitterWindow_SetSashGravity)(void* self, double gravity)
{
	return ((wxSplitterWindow*)self)->SetSashGravity(gravity);
}
	
}
