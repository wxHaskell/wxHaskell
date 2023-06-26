#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxCriticalSection_Create)()
{
	return (void*)new wxCriticalSection ();
}

EWXWEXPORT(void,wxCriticalSection_Delete)(void* self)
{
	delete (wxCriticalSection*)self;
}

EWXWEXPORT(void,wxCriticalSection_Enter)(void* self)
{
	((wxCriticalSection*)self)->Enter();
}

EWXWEXPORT(void,wxCriticalSection_Leave)(void* self)
{
	((wxCriticalSection*)self)->Leave();
}


EWXWEXPORT(wxMutex*,wxMutex_Create)()
{
	return new wxMutex ();
}

EWXWEXPORT(void,wxMutex_Delete)(wxMutex* self)
{
	delete self;
}

EWXWEXPORT(int,wxMutex_Lock)(wxMutex* self)
{
	return (int)self->Lock();
}

EWXWEXPORT(int,wxMutex_TryLock)(wxMutex* self)
{
	return (int)self->TryLock();
}

EWXWEXPORT(int,wxMutex_Unlock)(wxMutex* self)
{
	return (int)self->Unlock();
}

EWXWEXPORT(bool,wxMutex_IsLocked)(wxMutex* self)
{
#if wxVERSION_NUMBER >= 2400
	return false;
#else
	return self->IsLocked();
#endif
}

EWXWEXPORT(void*,wxCondition_Create)(void* _mutex)
{
#if wxVERSION_NUMBER < 2400
	return (void*)new wxCondition ();
#else
	return (void*)new wxCondition (*((wxMutex*)_mutex));
#endif
}

EWXWEXPORT(void,wxCondition_Delete)(void* self)
{
	delete (wxCondition*)self;
}

EWXWEXPORT(void,wxCondition_Wait)(void* self)
{
	((wxCondition*)self)->Wait();
}

EWXWEXPORT(int,wxCondition_WaitFor)(void* self,int sec,int nsec)
{
#if wxVERSION_NUMBER >= 2400
	return (int)((wxCondition*)self)->WaitTimeout((unsigned long)nsec);
#else
	return (int)((wxCondition*)self)->Wait((unsigned long)sec, (unsigned long)nsec);
#endif
}

EWXWEXPORT(void,wxCondition_Signal)(void* self)
{
	((wxCondition*)self)->Signal();
}

EWXWEXPORT(void,wxCondition_Broadcast)(void* self)
{
	((wxCondition*)self)->Broadcast();
}


EWXWEXPORT(void,wxMutexGui_Enter)()
{
	wxMutexGuiEnter();
}

EWXWEXPORT(void,wxMutexGui_Leave)()
{
	wxMutexGuiLeave();
}

}
