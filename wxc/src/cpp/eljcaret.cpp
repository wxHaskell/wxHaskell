#include "wxc/wrapper.h"
#include "wx/caret.h"

extern "C"
{

EWXWEXPORT(wxCaret*,wxCaret_Create)(wxWindow* _wnd,int _wth,int _hgt)
{
	return new wxCaret(_wnd, _wth, _hgt);
}

EWXWEXPORT(bool,wxCaret_IsOk)(wxCaret* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(bool,wxCaret_IsVisible)(wxCaret* self)
{
	return self->IsVisible();
}
	
EWXWEXPORT(wxPoint*,wxCaret_GetPosition)(wxCaret* self)
{
	wxPoint* p = new wxPoint();
	*p = self->GetPosition();
	return p;
}
	
EWXWEXPORT(wxSize*,wxCaret_GetSize)(wxCaret* self)
{
	wxSize* s = new wxSize();
	*s = self->GetSize();
	return s;
}
	
EWXWEXPORT(wxWindow*,wxCaret_GetWindow)(wxCaret* self)
{
	return self->GetWindow();
}
	
EWXWEXPORT(void,wxCaret_SetSize)(wxCaret* self,int width,int height)
{
	self->SetSize(width, height);
}
	
EWXWEXPORT(void,wxCaret_Move)(wxCaret* self,int x,int y)
{
	self->Move(x, y);
}
	
EWXWEXPORT(void,wxCaret_Show)(wxCaret* self)
{
	self->Show();
}
	
EWXWEXPORT(void,wxCaret_Hide)(wxCaret* self)
{
	self->Hide();
}
	
EWXWEXPORT(int,wxCaret_GetBlinkTime)()
{
	return wxCaret::GetBlinkTime();
}
	
EWXWEXPORT(void,wxCaret_SetBlinkTime)(int milliseconds)
{
	wxCaret::SetBlinkTime(milliseconds);
}
	
}
