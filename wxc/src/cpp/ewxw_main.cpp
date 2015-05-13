#include "wrapper.h"

extern int APPTerminating;


#ifdef _WIN32

#if (defined(__WXDEBUG__) && defined(_MSC_VER))
 #include <crtdbg.h>
#endif

#include <windows.h>
#include <locale.h>

#if wxCHECK_VERSION(2,5,0)
 #define wxHANDLE  HINSTANCE
 extern int WXDLLIMPEXP_BASE wxEntry(wxHANDLE hInstance, wxHANDLE hPrevInstance, char *pCmdLine, int nCmdShow);
#else
 #define wxHANDLE  WXHINSTANCE
#endif


extern "C"
{
EWXWEXPORT(void, ELJApp_InitializeC) (wxClosure* closure, int _argc, char** _argv)
{
  wxHANDLE wxhInstance = GetModuleHandle(NULL);

  /*
   * Set the locale to "C",
   * to prevent a wxWidgets assert failure in intl.cpp
   */
  setlocale(LC_ALL, "C");

/* check memory leaks with visual C++ */
#if (defined(__WXDEBUG__) && defined(_MSC_VER))
  _CrtMemState memStart,memEnd,memDif;
  _CrtMemCheckpoint( &memStart );
  _CrtSetReportMode( _CRT_WARN, _CRTDBG_MODE_FILE );
  _CrtSetReportFile( _CRT_WARN, _CRTDBG_FILE_STDOUT );
  _CrtSetReportMode( _CRT_ERROR, _CRTDBG_MODE_FILE );
  _CrtSetReportFile( _CRT_ERROR, _CRTDBG_FILE_STDOUT );
  _CrtSetReportMode( _CRT_ASSERT, _CRTDBG_MODE_FILE );
  _CrtSetReportFile( _CRT_ASSERT, _CRTDBG_FILE_STDOUT );
#endif

  initClosure = closure;
  APPTerminating = 0;
  
  /* Pretty lame way to detect if we are running in GHCI */
  /* TODO: detect when running program with runhaskell as well */
  if(_argc > 0 && !wcscmp(((wchar_t**)_argv)[0], L"<interactive>")) {
    /* we are in GHCI */
    wxEntry(_argc, _argv);
  }
  else {
    wxEntry(wxhInstance, NULL, (_argc > 0 ? _argv[0] : NULL), SW_SHOWNORMAL);
  }
  
  APPTerminating = 1;

  /* wxPendingEvents is deleted but not set to NULL -> disaster when restarted from an interpreter */
#if !defined(WXMAKINGDLL) && !defined(WXUSINGDLL)
  wxPendingEvents = NULL;
#endif

#if defined(wxUSE_ODBC) && (wxUSE_ODBC != 0)
  wxDbCloseConnections();
#endif

/* check memory leaks with visual C++ */
#if (defined(__WXDEBUG__) && defined(_MSC_VER))
  _CrtMemCheckpoint( &memEnd );
  if (_CrtMemDifference( &memDif, &memStart, &memEnd)
     && (memDif.lCounts[_NORMAL_BLOCK]>=-2 && memDif.lCounts[_NORMAL_BLOCK] <= 0))
  {
    _RPT0(_CRT_WARN,"\n** memory leak detected (**\n" );
    _CrtMemDumpStatistics(&memDif);
    /* _CrtMemDumpAllObjectsSince(&memStart);  */
    _RPT0(_CRT_WARN,"** memory leak report done **\n\n" );

  }
#endif
}

EWXWEXPORT(void, ELJApp_initialize)(void* _obj, AppInitFunc _func, char* _cmd, void* _inst)
{
  HINSTANCE wxhInstance = (HINSTANCE)_inst;
  APPTerminating = 0;
  wxEntry(wxhInstance, NULL, _cmd, SW_SHOWNORMAL);
  APPTerminating = 1;
  /* wxPendingEvents is deleted but not set to NULL -> disaster when restarted from an interpreter */
#if !defined(WXMAKINGDLL) && !defined(WXUSINGDLL)
  wxPendingEvents = NULL;
#endif
}

}

#else  /* not WIN32 */

#if !wxCHECK_VERSION(2,5,0)
#ifdef __WXMAC__ /* declare wxEntry explicitly as wxMAC seems to leave it out? */
void wxEntry( int argc, char** argv, bool enterLoop = true );
#endif
#ifdef __WXGTK__ /* declare explicitly or we get link errors? */
int wxEntry( int argc, char** argv );
#endif
#endif
extern "C"
{

EWXWEXPORT(void, ELJApp_InitializeC) (wxClosure* closure, int _argc, char** _argv)
{
  char* args[] = { "wxc", NULL };

  initClosure = closure;
  if (_argv == NULL) {
    /* note: wxGTK crashes when argv == NULL */
    _argv = args;
    _argc = 1;
  }
  APPTerminating = 0;
  wxEntry(_argc,_argv);
  APPTerminating = 1;
  /* wxPendingEvents is deleted but not set to NULL -> disaster when restarted from an interpreter */
#if !defined(WXMAKINGDLL) && !defined(WXUSINGDLL)
  wxPendingEvents = NULL;
#endif
}

EWXWEXPORT(void, ELJApp_initialize) (void* _obj, AppInitFunc _func, int _argc, void* _argv)
{
  APPTerminating = 0;
  wxEntry(_argc, (char**)_argv);
  APPTerminating = 1;
  /* wxPendingEvents is deleted but not set to NULL -> disaster when restarted from an interpreter */
#if !defined(WXMAKINGDLL) && !defined(WXUSINGDLL)
  wxPendingEvents = NULL;
#endif
}

}
#endif
