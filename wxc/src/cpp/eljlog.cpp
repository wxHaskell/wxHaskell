#include "wrapper.h"
#include "wx/log.h"

extern "C"
{

typedef void (*TLogFunc) (void*, int, void*, int);

}

class ELJLog : public wxLog
{
	private:
		TLogFunc func;
		void*    EiffelObject;
		
    protected:
		virtual void DoLog(wxLogLevel level, const wxChar *szString, time_t t)
                  {
                    wxString s(szString);
                    func (EiffelObject, (int)level, (void*)&s , (int)t);
                  }

	public:
		ELJLog (void* _obj, void* _fnc) : wxLog()
		{
			func = (TLogFunc)_fnc;
			EiffelObject = _obj;
		}
};

extern "C"
{

EWXWEXPORT(void*,ELJLog_Create)(void* self,void* _fnc)
{
	return (void*)new ELJLog(self, _fnc);
}

EWXWEXPORT(void,ELJLog_Delete)(ELJLog* self)
{
	delete self;
}

EWXWEXPORT(bool,ELJLog_IsEnabled)(ELJLog* self)
{
	return self->IsEnabled();
}
	
EWXWEXPORT(int,ELJLog_EnableLogging)(ELJLog* self,bool doIt)
{
	return (int)self->EnableLogging(doIt);
}
	
EWXWEXPORT(void,ELJLog_OnLog)(ELJLog* self,int level,void* szString,int t)
{
	self->OnLog((wxLogLevel)level, (const wxChar*)szString, (time_t)t);
}
	
EWXWEXPORT(void,ELJLog_Flush)(ELJLog* self)
{
	self->Flush();
}
	
EWXWEXPORT(int,ELJLog_HasPendingMessages)(ELJLog* self)
{
	return (int)self->HasPendingMessages();
}
	
EWXWEXPORT(void,ELJLog_FlushActive)(ELJLog* self)
{
	self->FlushActive();
}
	
EWXWEXPORT(void*,ELJLog_GetActiveTarget)()
{
	return (void*)ELJLog::GetActiveTarget();
}
	
EWXWEXPORT(void*,ELJLog_SetActiveTarget)(wxLog* pLogger)
{
	return (void*)ELJLog::SetActiveTarget(pLogger);
}
	
EWXWEXPORT(void,ELJLog_Suspend)(ELJLog* self)
{
	self->Suspend();
}
	
EWXWEXPORT(void,ELJLog_Resume)(ELJLog* self)
{
	self->Resume();
}
	
EWXWEXPORT(void,ELJLog_SetVerbose)(ELJLog* self,bool bVerbose)
{
	self->SetVerbose(bVerbose);
}
	
EWXWEXPORT(void,ELJLog_DontCreateOnDemand)(ELJLog* self)
{
	self->DontCreateOnDemand();
}
	
EWXWEXPORT(void,ELJLog_AddTraceMask)(ELJLog* self,void* str)
{
	self->AddTraceMask((const wxChar*)str);
}
	
EWXWEXPORT(void,ELJLog_RemoveTraceMask)(ELJLog* self,void* str)
{
	self->RemoveTraceMask((const wxChar*)str);
}
	
EWXWEXPORT(void,ELJLog_SetTimestamp)(ELJLog* self,void* ts)
{
	self->SetTimestamp((const wxChar*)ts);
}
	
EWXWEXPORT(int,ELJLog_GetVerbose)(ELJLog* self)
{
	return (int)self->GetVerbose();
}
	
#if !wxCHECK_VERSION(3, 1, 0)
EWXWEXPORT(int,ELJLog_GetTraceMask)(ELJLog* self)
{
	return (int)self->GetTraceMask();
}
#endif
	
EWXWEXPORT(bool,ELJLog_IsAllowedTraceMask)(ELJLog* self,void* mask)
{
	return self->IsAllowedTraceMask((const wxChar*)mask);
}
	
EWXWEXPORT(void*,ELJLog_GetTimestamp)(ELJLog* self)
{
#if (wxVERSION_NUMBER < 2900)
	return (void*)self->GetTimestamp();
#else
    wxString retVal = self->GetTimestamp();
    return (void*) retVal.wchar_str();
#endif
}

EWXWEXPORT(int,ELJSysErrorCode)()
{
	return (int)wxSysErrorCode();
}

EWXWEXPORT(void*,ELJSysErrorMsg)(int nErrCode)
{
	return (void*)wxSysErrorMsg((unsigned long)nErrCode);
}

EWXWEXPORT(void,LogErrorMsg)(wxString* _msg)
{
	wxLogError(*_msg);
}

EWXWEXPORT(void,LogFatalErrorMsg)(wxString* _msg)
{
	wxLogFatalError(*_msg);
}

EWXWEXPORT(void,LogWarningMsg)(wxString* _msg)
{
	wxLogWarning(*_msg);
}

EWXWEXPORT(void,LogMessageMsg)(wxString* _msg)
{
	wxLogMessage(*_msg);
}


EWXWEXPORT(void*,wxLogChain_Create)(void* logger)
{
	return new wxLogChain ((wxLog*)logger);
}

EWXWEXPORT(void,wxLogChain_Delete)(wxLogChain* self)
{
	delete self;
}

EWXWEXPORT(void,wxLogChain_SetLog)(wxLogChain* self,wxLog* logger)
{
	self->SetLog(logger);
}
	
EWXWEXPORT(void,wxLogChain_PassMessages)(wxLogChain* self,bool bDoPass)
{
	self->PassMessages(bDoPass);
}
	
EWXWEXPORT(bool,wxLogChain_IsPassingMessages)(wxLogChain* self)
{
	return self->IsPassingMessages();
}
	
EWXWEXPORT(void*,wxLogChain_GetOldLog)(wxLogChain* self)
{
	return (void*)self->GetOldLog();
}
	
}
