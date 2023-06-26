#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxTimer*,wxTimer_Create)(wxEvtHandler* _prt,int _id)
{
	return  new wxTimer (_prt, _id);
}

EWXWEXPORT(void,wxTimer_Delete)(wxTimer* self)
{
	delete self;
}

EWXWEXPORT(bool,wxTimer_Start)(wxTimer* self,int _int,bool _one)
{
	return self->Start (_int, _one);
}

EWXWEXPORT(void,wxTimer_Stop)(wxTimer* self)
{
	self->Stop();
}

EWXWEXPORT(bool,wxTimer_IsRuning)(wxTimer* self)
{
	return self->IsRunning();
}

EWXWEXPORT(bool,wxTimer_IsOneShot)(wxTimer* self)
{
	return self->IsOneShot();
}

EWXWEXPORT(int,wxTimer_GetInterval)(wxTimer* self)
{
	return self->GetInterval();
}


EWXWEXPORT(wxStopWatch*,wxStopWatch_Create)()
{
	return new wxStopWatch();
}
	
EWXWEXPORT(void,wxStopWatch_Delete)(wxStopWatch* self)
{
	delete self;
}
	
EWXWEXPORT(void,wxStopWatch_Start)(wxStopWatch* self,int _t)
{
	self->Start((long)_t);
}
	
EWXWEXPORT(void,wxStopWatch_Pause)(wxStopWatch* self)
{
	self->Pause();
}
	
EWXWEXPORT(void,wxStopWatch_Resume)(wxStopWatch* self)
{
	self->Resume();
}
	
EWXWEXPORT(long,wxStopWatch_Time)(wxStopWatch* self)
{
	return self->Time();
}
	
}
