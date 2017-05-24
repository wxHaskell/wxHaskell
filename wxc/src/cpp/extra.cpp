#include "wrapper.h"
#include "wx/process.h"
#include "wx/textctrl.h"
#include "wx/progdlg.h"
#include "wx/listctrl.h"
#include "wx/grid.h"
#include "wx/fileconf.h"
#include "wx/spinctrl.h"
#include "wx/propgrid/propgrid.h"
#include "wx/propgrid/advprops.h"

#include <wx/numdlg.h>
#include <wx/power.h>

////////////////////////////////////////////////////////////////////////////////
// wxc specific events
////////////////////////////////////////////////////////////////////////////////

BEGIN_DECLARE_EVENT_TYPES()
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_DELETE, 1000)
#if (wxVERSION_NUMBER < 2905)	
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_HTML_CELL_CLICKED, 1001 )
#endif
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_HTML_CELL_MOUSE_HOVER, 1002 )
#if (wxVERSION_NUMBER < 2905)	
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_HTML_LINK_CLICKED, 1003 )
#endif
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_HTML_SET_TITLE, 1004 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_INPUT_SINK, 1005 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_SORT, 1006 )
END_DECLARE_EVENT_TYPES()

DEFINE_LOCAL_EVENT_TYPE( wxEVT_DELETE )
#if (wxVERSION_NUMBER < 2905)	
DEFINE_LOCAL_EVENT_TYPE( wxEVT_HTML_CELL_CLICKED )
#endif
DEFINE_LOCAL_EVENT_TYPE( wxEVT_HTML_CELL_MOUSE_HOVER )
#if (wxVERSION_NUMBER < 2905)	
DEFINE_LOCAL_EVENT_TYPE( wxEVT_HTML_LINK_CLICKED )
#endif
DEFINE_LOCAL_EVENT_TYPE( wxEVT_HTML_SET_TITLE )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_INPUT_SINK )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_SORT )

// Exported wxC event wrappers - must be C linkage
extern "C" {
  MAKE_EVENT_WRAPPER(EVT_DELETE)
  MAKE_EVENT_WRAPPER(EVT_HTML_CELL_CLICKED)
  MAKE_EVENT_WRAPPER(EVT_HTML_CELL_MOUSE_HOVER)
  MAKE_EVENT_WRAPPER(EVT_HTML_LINK_CLICKED)
  MAKE_EVENT_WRAPPER(EVT_HTML_SET_TITLE)
  MAKE_EVENT_WRAPPER(EVT_INPUT_SINK)
  MAKE_EVENT_WRAPPER(EVT_SORT)
}

/*-----------------------------------------------------------------------------
  Timers
-----------------------------------------------------------------------------*/
class wxTimerEx : public wxTimer
{
  private:
    wxClosure* m_closure;

  public:
    wxTimerEx();
    ~wxTimerEx();

    void Connect( wxClosure* closure );
    wxClosure* GetClosure();
    void Notify();
};

wxTimerEx::wxTimerEx()
{
  m_closure = NULL;
}

wxTimerEx::~wxTimerEx()
{
  if (m_closure) {
    m_closure->DecRef();
    m_closure=NULL;
  }
}

void wxTimerEx::Connect( wxClosure* closure )
{
  if (m_closure) m_closure->DecRef();
  m_closure = closure;
  if (m_closure) m_closure->IncRef();
}

wxClosure* wxTimerEx::GetClosure()
{
  return m_closure;
}

void wxTimerEx::Notify()
{
#if (wxVERSION_NUMBER < 2900)
  wxTimerEvent timerEvent(0,this->GetInterval());
#else
  wxTimerEvent timerEvent(*this);
#endif
  if (m_closure)
    m_closure->Invoke(&timerEvent);
}

/*-----------------------------------------------------------------------------
  wxInputSink wrapper: adds non-blocking event driven input channel
-----------------------------------------------------------------------------*/
class wxInputSink;

class wxInputSinkEvent : public wxEvent
{
private:
  char*         m_buffer;
  size_t        m_bufferLen;
  size_t        m_lastRead;
  wxStreamError m_lastError;

  void Read( wxInputStream* input );
  friend class wxInputSink;

public:
  wxInputSinkEvent( int id, size_t bufferLen );
  wxInputSinkEvent( const wxInputSinkEvent& event );  /* copy constructor */
  ~wxInputSinkEvent();

  wxEvent* Clone() const          { return new wxInputSinkEvent(*this); }

  wxStreamError LastError() const { return m_lastError; }
  int           LastRead()  const { return m_lastRead; }
  char*         LastInput() const { return m_buffer;   }
};

class wxInputSink : public wxThread
{
private:
  wxEvtHandler*    m_evtHandler;
  wxInputStream*   m_input;
  wxInputSinkEvent m_event;

protected:
  ExitCode Entry();

public:
  wxInputSink( wxInputStream* input, wxEvtHandler* evtHandler, int bufferLen );
  ~wxInputSink();

  intptr_t GetId();
  void Start();
};


wxInputSink::wxInputSink( wxInputStream* input, wxEvtHandler* evtHandler, int bufferLen )
  : wxThread(wxTHREAD_DETACHED), m_event( 0, bufferLen )
{
  m_input      = input;
  m_evtHandler = evtHandler;
}

wxInputSink::~wxInputSink()
{
/* fprintf(stderr, "InputSink is deleted\n"); */
}

void wxInputSink::Start()
{
  wxThreadError result = Create();
  switch(result) {
    case wxTHREAD_NO_ERROR:  Run(); break;
    case wxTHREAD_RUNNING :  break;
    default               :  Delete(); break;
  }
}

intptr_t wxInputSink::GetId()
{
  return (intptr_t)m_input;
}

wxThread::ExitCode wxInputSink::Entry()
{
  if (m_input == NULL || m_evtHandler == NULL || m_event.m_buffer == NULL ) return 0;

  m_event.SetId(GetId());

  /* while input && not external destroy */
  while (!TestDestroy() && m_event.LastError() == wxSTREAM_NO_ERROR)
  {
     /* (blocking) read */
     m_event.Read( m_input );

     /* post the event to the main gui thread (note: event is cloned and thus the input buffer copied)*/
     m_evtHandler->AddPendingEvent(m_event);
  }

  /* Process pending events */
  wxWakeUpIdle();

  /* return */
  if (m_event.LastError() == wxSTREAM_NO_ERROR || m_event.LastError() == wxSTREAM_EOF)
     return (ExitCode)0;
  else
     return (ExitCode)1;
}


wxInputSinkEvent::wxInputSinkEvent( int id, size_t bufferLen ) : wxEvent( id, wxEVT_INPUT_SINK  )
{
  m_lastError = wxSTREAM_NO_ERROR;
  m_lastRead  = 0;
  if (bufferLen <= 0) bufferLen = 128;
  m_buffer    = (char*)malloc( bufferLen + 1 );
  m_bufferLen = (m_buffer ? bufferLen : 0);
}

wxInputSinkEvent::wxInputSinkEvent( const wxInputSinkEvent& event ) : wxEvent( event )
{
  /* we copy only the exact input buffer, as 'Read' will never be called */
  m_lastError = event.m_lastError;
  m_bufferLen = 0;
  m_lastRead  = 0;
  m_buffer    = (char*)malloc( event.m_lastRead + 1 );
  if (m_buffer) {
    m_bufferLen = event.m_lastRead;
    m_lastRead  = event.m_lastRead;
    memcpy( m_buffer, event.m_buffer, m_lastRead );
    m_buffer[m_lastRead] = 0;
  }
}

wxInputSinkEvent::~wxInputSinkEvent()
{
  if (m_buffer) free(m_buffer);
}

void wxInputSinkEvent::Read( wxInputStream* input )
{
  /* check */
  if (input == NULL || m_buffer == NULL || m_bufferLen == 0) {
    m_lastError = wxSTREAM_READ_ERROR;
    m_lastRead  = 0;
    return;
  }

  /* read */
  input->Read( m_buffer, m_bufferLen );
#if wxCHECK_VERSION(2,5,0)
  m_lastError = input->GetLastError();
#else
  m_lastError = input->LastError();
#endif
  if (m_lastError == wxSTREAM_NO_ERROR)
    m_lastRead  = input->LastRead();
  else
    m_lastRead  = 0;


  /* maintain invariants */
  if (m_lastRead < 0)           m_lastRead = 0;
  if (m_lastRead > m_bufferLen) m_lastRead = m_bufferLen;

  /* add zero terminator */
  m_buffer[m_lastRead] = 0;
}


/*-----------------------------------------------------------------------------
  wxHtmlWindow wrapper: adds normal events instead of using inheritance
-----------------------------------------------------------------------------*/
class wxcHtmlWindow : public wxHtmlWindow
{
private:
    DECLARE_DYNAMIC_CLASS(wxcHtmlWindow)
public:
    wxcHtmlWindow() : wxHtmlWindow() {};   wxcHtmlWindow(wxWindow *, wxWindowID id, const wxPoint&, const wxSize& size, long style, const wxString& );

#if (wxVERSION_NUMBER <= 2800)
   void OnCellClicked(wxHtmlCell *cell, wxCoord x, wxCoord y, const wxMouseEvent& event);
#else
   bool OnCellClicked(wxHtmlCell *cell, wxCoord x, wxCoord y, const wxMouseEvent& event);
#endif
   void OnCellMouseHover(wxHtmlCell *cell, wxCoord x, wxCoord y);
   void OnLinkClicked(const wxHtmlLinkInfo& link);
   wxHtmlOpeningStatus OnOpeningURL(wxHtmlURLType type,const wxString& url, wxString *redirect);
   void OnSetTitle(const wxString& title);
};

IMPLEMENT_DYNAMIC_CLASS(wxcHtmlWindow, wxHtmlWindow)

class wxcHtmlEvent : public wxCommandEvent
{
private:
    DECLARE_DYNAMIC_CLASS(wxcHtmlEvent)
private:
    const wxMouseEvent*     m_mouseEvent;
    const wxHtmlCell*       m_htmlCell;
    const wxHtmlLinkInfo*   m_linkInfo;
    wxPoint                 m_logicalPosition;

public:
    wxcHtmlEvent() : wxCommandEvent() {};
    wxcHtmlEvent( wxEventType commandType, int id, const wxMouseEvent* mouseEvent, const wxHtmlCell* htmlCell, const wxHtmlLinkInfo* linkInfo, wxPoint logicalPoint );
    const wxMouseEvent* GetMouseEvent();
    const wxHtmlCell *  GetHtmlCell();
    wxString*           GetHtmlCellId();
    wxString*           GetHref();
    wxString*           GetTarget();
    wxPoint             GetLogicalPosition();
};

IMPLEMENT_DYNAMIC_CLASS(wxcHtmlEvent, wxCommandEvent)

wxcHtmlWindow::wxcHtmlWindow(wxWindow * prt, wxWindowID id, const wxPoint& pt, const wxSize& size, long style, const wxString& title )
: wxHtmlWindow( prt, id, pt, size, style, title )
{}

#if (wxVERSION_NUMBER < 2800)
void wxcHtmlWindow::OnCellClicked(wxHtmlCell *cell, wxCoord x, wxCoord y, const wxMouseEvent& event)
#else
bool wxcHtmlWindow::OnCellClicked(wxHtmlCell *cell, wxCoord x, wxCoord y, const wxMouseEvent& event)
#endif
{
    wxHtmlLinkInfo* linkPtr;
#if (wxVERSION_NUMBER < 2800)
    if (cell==NULL) return;
#else
    if (cell==NULL) return false;
#endif

    linkPtr = cell->GetLink(x, y);
    if (linkPtr != NULL)
    {
        wxHtmlLinkInfo link(*linkPtr);
        link.SetEvent(&event);
        link.SetHtmlCell(cell);
        {
          wxcHtmlEvent htmlEvent( wxEVT_HTML_LINK_CLICKED, this->GetId(), &event, cell, &link, wxPoint(x,y) );
          this->ProcessEvent( htmlEvent );
        }
    }
    else
    {
      wxcHtmlEvent htmlEvent( wxEVT_HTML_CELL_CLICKED, this->GetId(), &event, cell, NULL, wxPoint(x,y) );
      this->ProcessEvent( htmlEvent );
    }
#if (wxVERSION_NUMBER >= 2800)
    return true;
#endif
}

void wxcHtmlWindow::OnCellMouseHover(wxHtmlCell *cell, wxCoord x, wxCoord y)
{
    wxcHtmlEvent htmlEvent( wxEVT_HTML_CELL_MOUSE_HOVER, this->GetId(), NULL, cell, NULL, wxPoint(x,y) );
    this->ProcessEvent( htmlEvent );
}

void wxcHtmlWindow::OnLinkClicked(const wxHtmlLinkInfo& link)
{
    wxcHtmlEvent htmlEvent( wxEVT_HTML_LINK_CLICKED, this->GetId(), link.GetEvent(), link.GetHtmlCell(), &link, wxPoint(-1,-1) );
    this->ProcessEvent( htmlEvent );
}

void wxcHtmlWindow::OnSetTitle(const wxString& title)
{
    wxcHtmlEvent htmlEvent( wxEVT_HTML_SET_TITLE, this->GetId(), NULL, NULL, NULL, wxPoint(-1,-1) );
    htmlEvent.SetString( title );
    this->ProcessEvent( htmlEvent );
}


wxcHtmlEvent::wxcHtmlEvent( wxEventType commandType, int id, const wxMouseEvent* mouseEvent, const wxHtmlCell* htmlCell, const wxHtmlLinkInfo* linkInfo, wxPoint logicalPoint )
: wxCommandEvent( commandType, id )
{
    m_mouseEvent = mouseEvent;
    m_htmlCell = htmlCell;
    m_linkInfo = linkInfo;
    m_logicalPosition = logicalPoint;
}

const wxMouseEvent* wxcHtmlEvent::GetMouseEvent()
{
    return m_mouseEvent;
}

const wxHtmlCell *  wxcHtmlEvent::GetHtmlCell()
{
    return m_htmlCell;
}

wxString* wxcHtmlEvent::GetHtmlCellId()
{
    if (m_htmlCell)
        return new wxString(m_htmlCell->GetId());
    else
        return new wxString(wxT(""));
}

wxString* wxcHtmlEvent::GetHref()
{
    if (m_linkInfo)
        return new wxString(m_linkInfo->GetHref());
    else
        return new wxString(wxT(""));
}

wxString* wxcHtmlEvent::GetTarget()
{
    if (m_linkInfo)
        return new wxString (m_linkInfo->GetTarget());
    else
        return new wxString(wxT(""));
}

wxPoint wxcHtmlEvent::GetLogicalPosition()
{
    return m_logicalPosition;
}

/*-----------------------------------------------------------------------------
  wxGridCellTextEnterEditor
-----------------------------------------------------------------------------*/
class wxGridCellTextEnterEditor : public wxGridCellTextEditor
{
public:
  wxGridCellTextEnterEditor() : wxGridCellTextEditor() {}

  void Create(wxWindow* parent,
                        wxWindowID id,
                        wxEvtHandler* evtHandler);

};

void wxGridCellTextEnterEditor::Create(wxWindow* parent,
                                       wxWindowID id,
                                       wxEvtHandler* evtHandler)
{
    wxGridCellTextEditor::Create(parent, id, evtHandler);
    
    {
      long style = m_control->GetWindowStyle();
      m_control->SetWindowStyle( style | wxTE_PROCESS_ENTER );
    }
}


/*-----------------------------------------------------------------------------
  pre processor definitions
-----------------------------------------------------------------------------*/
static const wxChar* defineDefs[] = {
#ifdef __WINDOWS__
  wxT("__WINDOWS__"),
#endif // any Windows, yom may also use
#ifdef __WIN16__
  wxT("__WIN16__"),
#endif // Win16 API
#ifdef __WIN32__
  wxT("__WIN32__"),
#endif // Win32 API
#ifdef __WIN95__
  wxT("__WIN95__"),
#endif // Windows 95 or NT 4.0 and above system (not NT 3.5x)
#ifdef __WXGTK__
  wxT("__WXGTK__"),
#endif // GTK
#ifdef __WXGTK12__
  wxT("__WXGTK12__"),
#endif // GTK 1.2 or higher
#ifdef __WXGTK20__
  wxT("__WXGTK20__"),
#endif // GTK 2.0 or higher
#ifdef __WXMOTIF__
  wxT("__WXMOTIF__"),
#endif // Motif
#ifdef __WXMAC__
  wxT("__WXMAC__"),
#endif // MacOS
#ifdef __WXMGL__
  wxT("__WXMGL__"),
#endif // SciTech Soft MGL
#ifdef __WXUNIVERSAL__
  wxT("__WXUNIVERSAL__"),
#endif //will be also defined)
#ifdef __WXMSW__
  wxT("__WXMSW__"),
#endif // Any Windows
#ifdef __WXOS2__
  wxT("__WXOS2__"),
#endif // OS/2 native Presentation Manager
#ifdef __WXPM__
  wxT("__WXPM__"),
#endif // OS/2 native Presentation Manager
#ifdef __WXSTUBS__
  wxT("__WXSTUBS__"),
#endif // Stubbed version ('template' wxWin implementation)
#ifdef __WXXT__
  wxT("__WXXT__"),
#endif // Xt; mutually exclusive with WX_MOTIF, not implemented in wxWindows 2.x
// wxX11
#ifdef __WXX11__
  wxT("__WXX11__"),
#endif
#ifdef __WXUNIVERSAL__
  wxT("__WXUNIVERSAL__"),
#endif //will be also defined)
#ifdef __WXWINE__
  wxT("__WXWINE__"),
#endif // WINE (i.e. Win32 on Unix)
#ifdef __WXUNIVERSAL__
  wxT("__WXUNIVERSAL__"),
#endif // wxUniversal port, always defined in addition to one of the symbols above so this should be tested first.
#ifdef __X__
  wxT("__X__"),
#endif // any X11-based GUI toolkit except GTK+

// any Mac OS version
#ifdef __APPLE__
  wxT("__APPLE__"),
#endif
// AIX
#ifdef __AIX__
  wxT("__AIX__"),
#endif
// Any BSD
#ifdef __BSD__
  wxT("__BSD__"),
#endif
// Cygwin: Unix on Win32
#ifdef __CYGWIN__
  wxT("__CYGWIN__"),
#endif
// Mac OS X
#ifdef __DARWIN__
  wxT("__DARWIN__"),
#endif
#ifdef ____DATA_GENERAL____
  wxT("__DATA_GENERAL__"),
#endif
// DOS (used with wxMGL only)
#ifdef __DOS_GENERAL__
  wxT("__DOS_GENERAL__"),
#endif
// FreeBSD
#ifdef __FREEBSD__
  wxT("__FREEBSD__"),
#endif
// HP-UX (Unix)
#ifdef ____HPUX____
  wxT("__HPUX__"),
#endif
// Linux
#ifdef __LINUX__
  wxT("__LINUX__"),
#endif
// OSF/1
#ifdef __OSF__
  wxT("__OSF__"),
#endif
// IRIX
#ifdef __SGI__
  wxT("__SGI__"),
#endif
// Solaris
#ifdef __SOLARIS__
  wxT("__SOLARIS__"),
#endif
// Any Sun
#ifdef __SUN__
  wxT("__SUN__"),
#endif
// Sun OS
#ifdef __SUNOS__
  wxT("__SUNOS__"),
#endif
// SystemV R4
#ifdef __SVR4__
  wxT("__SVR4__"),
#endif
// SystemV generic
#ifdef __SYSV__
  wxT("__SYSV__"),
#endif
// Ultrix
#ifdef __ULTRIX__
  wxT("__ULTRIX__"),
#endif
// any Unix
#ifdef __UNIX__
  wxT("__UNIX__"),
#endif
// Unix, BeOS or VMS
#ifdef __UNIX_LIKE__
  wxT("__UNIX_LIKE__"),
#endif
// VMS
#ifdef __VMS__
  wxT("__VMS__"),
#endif
// any Windows
#ifdef __WINDOWS__
  wxT("__WINDOWS__"),
#endif

// DEC Alpha architecture
#ifdef __ALPHA__
  wxT("__ALPHA__"),
#endif
// Intel i386 or compatible
#ifdef __INTEL__
  wxT("__INTEL__"),
#endif
// Motorola Power PC
#ifdef __POWERPC__
  wxT("__POWERPC__"),
#endif

// Borland C++
#ifdef __BORLANDC__
  wxT("__BORLANDC__"),
#endif
// DJGPP
#ifdef __DJGPP__
  wxT("__DJGPP__"),
#endif
// Gnu C++ on any platform
#ifdef __GNUG__
  wxT("__GNUG__"),
#endif
// GnuWin32 compiler
#ifdef __GNUWIN32__
  wxT("__GNUWIN32__"),
#endif
// CodeWarrior MetroWerks compiler
#ifdef __MWERKS__
  wxT("__MWERKS__"),
#endif
// Sun CC
#ifdef __SUNCC__
  wxT("__SUNCC__"),
#endif
// Symantec C++
#ifdef __SYMANTECC__
  wxT("__SYMANTECC__"),
#endif
// IBM Visual Age (OS/2)
#ifdef __VISAGECPP__
  wxT("__VISAGECPP__"),
#endif
// Microsoft Visual C++
#ifdef __VISUALC__
  wxT("__VISUALC__"),
#endif
// AIX compiler
#ifdef __XLC__
  wxT("__XLC__"),
#endif
// Watcom C++
#ifdef __WATCOMC__
  wxT("__WATCOMC__"),
#endif
  NULL
};

static const wxChar* useDefs[] = {
#ifdef wxUSE_UNIX
  wxT("UNIX"),
#endif
#ifdef wxUSE_NANOX
  wxT("NANOX"),
#endif
#ifdef wxUSE_NATIVE_STATUSBAR
  wxT("NATIVE_STATUSBAR"),
#endif
#ifdef wxUSE_OWNER_DRAWN
  wxT("OWNER_DRAWN"),
#endif
#ifdef wxUSE_OWNER_DRAWN
  wxT("OWNER_DRAWN"),
#endif
#ifdef wxUSE_RICHEDIT
  wxT("RICHEDIT"),
#endif
#ifdef wxUSE_RICHEDIT
  wxT("RICHEDIT"),
#endif
#ifdef wxUSE_REGEX
  wxT("REGEX"),
#endif
#ifdef wxUSE_ZLIB
  wxT("ZLIB"),
#endif
#ifdef wxUSE_LIBPNG
  wxT("LIBPNG"),
#endif
#ifdef wxUSE_LIBJPEG
  wxT("LIBJPEG"),
#endif
#ifdef wxUSE_LIBTIFF
  wxT("LIBTIFF"),
#endif
#ifdef wxUSE_ODBC
  wxT("ODBC"),
#endif
#ifdef wxUSE_FREETYPE
  wxT("FREETYPE"),
#endif
#ifdef wxUSE_THREADS
  wxT("THREADS"),
#endif
#if defined(wxcREFUSE_OPENGL)
# undef wxUSE_OPENGL
# undef wxUSE_GLCANVAS
#endif
#ifdef wxUSE_OPENGL
  wxT("OPENGL"),
#endif
#ifdef wxUSE_GLCANVAS
  wxT("GLCANVAS"),
#endif
#ifdef wxUSE_GUI
  wxT("GUI"),
#endif
#ifdef wxUSE_NOGUI
  wxT("NOGUI"),
#endif
#ifdef wxUSE_ON_FATAL_EXCEPTION
  wxT("ON_FATAL_EXCEPTION"),
#endif
#ifdef wxUSE_SNGLINST_CHECKER
  wxT("SNGLINST_CHECKER"),
#endif
#ifdef wxUSE_CONSTRAINTS
  wxT("CONSTRAINTS"),
#endif
#ifdef wxUSE_VALIDATORS
  wxT("VALIDATORS"),
#endif
#ifdef wxUSE_CONTROLS
  wxT("CONTROLS"),
#endif
#ifdef wxUSE_POPUPWIN
  wxT("POPUPWIN"),
#endif
#ifdef wxUSE_TIPWINDOW
  wxT("TIPWINDOW"),
#endif
#ifdef wxUSE_ACCEL
  wxT("ACCEL"),
#endif
#ifdef wxUSE_CALENDARCTRL
  wxT("CALENDARCTRL"),
#endif
#ifdef wxUSE_FILEDLG
  wxT("FILEDLG"),
#endif
#ifdef wxUSE_FINDREPLDLG
  wxT("FINDREPLDLG"),
#endif
#ifdef wxUSE_FONTDLG
  wxT("FONTDLG"),
#endif
#ifdef wxUSE_MIMETYPE
  wxT("MIMETYPE"),
#endif
#ifdef wxUSE_SYSTEM_OPTIONS
  wxT("SYSTEM_OPTIONS"),
#endif
#ifdef wxUSE_MSGDLG
  wxT("MSGDLG"),
#endif
#ifdef wxUSE_NUMBERDLG
  wxT("NUMBERDLG"),
#endif
#ifdef wxUSE_TEXTDLG
  wxT("TEXTDLG"),
#endif
#ifdef wxUSE_STARTUP_TIPS
  wxT("STARTUP_TIPS"),
#endif
#ifdef wxUSE_PROGRESSDLG
  wxT("PROGRESSDLG"),
#endif
#ifdef wxUSE_CHOICEDLG
  wxT("CHOICEDLG"),
#endif
#ifdef wxUSE_COLOURDLG
  wxT("COLOURDLG"),
#endif
#ifdef wxUSE_DIRDLG
  wxT("DIRDLG"),
#endif
#ifdef wxUSE_DRAGIMAGE
  wxT("DRAGIMAGE"),
#endif
#ifdef wxUSE_PROPSHEET
  wxT("PROPSHEET"),
#endif
#ifdef wxUSE_WIZARDDLG
  wxT("WIZARDDLG"),
#endif
#ifdef wxUSE_SPLASH
  wxT("SPLASH"),
#endif
#ifdef wxUSE_JOYSTICK
  wxT("JOYSTICK"),
#endif
#ifdef wxUSE_BUTTON
  wxT("BUTTON"),
#endif
#ifdef wxUSE_CARET
  wxT("CARET"),
#endif
#ifdef wxUSE_BMPBUTTON
  wxT("BMPBUTTON"),
#endif
#ifdef wxUSE_CHECKBOX
  wxT("CHECKBOX"),
#endif
#ifdef wxUSE_CHECKLISTBOX
  wxT("CHECKLISTBOX"),
#endif
#ifdef wxUSE_COMBOBOX
  wxT("COMBOBOX"),
#endif
#ifdef wxUSE_CHOICE
  wxT("CHOICE"),
#endif
#ifdef wxUSE_GAUGE
  wxT("GAUGE"),
#endif
#ifdef wxUSE_GRID
  wxT("GRID"),
#endif
#ifdef wxUSE_NEW_GRID
  wxT("NEW_GRID"),
#endif
#ifdef wxUSE_IMAGLIST
  wxT("IMAGLIST"),
#endif
#ifdef wxUSE_LISTBOX
  wxT("LISTBOX"),
#endif
#ifdef wxUSE_LISTCTRL
  wxT("LISTCTRL"),
#endif
#ifdef wxUSE_MENUS
  wxT("MENUS"),
#endif
#ifdef wxUSE_NOTEBOOK
  wxT("NOTEBOOK"),
#endif
#ifdef wxUSE_RADIOBOX
  wxT("RADIOBOX"),
#endif
#ifdef wxUSE_RADIOBTN
  wxT("RADIOBTN"),
#endif
#ifdef wxUSE_SASH
  wxT("SASH"),
#endif
#ifdef wxUSE_SCROLLBAR
  wxT("SCROLLBAR"),
#endif
#ifdef wxUSE_SLIDER
  wxT("SLIDER"),
#endif
#ifdef wxUSE_SPINBTN
  wxT("SPINBTN"),
#endif
#ifdef wxUSE_SPINCTRL
  wxT("SPINCTRL"),
#endif
#ifdef wxUSE_SPLITTER
  wxT("SPLITTER"),
#endif
#ifdef wxUSE_STATBMP
  wxT("STATBMP"),
#endif
#ifdef wxUSE_STATBOX
  wxT("STATBOX"),
#endif
#ifdef wxUSE_STATLINE
  wxT("STATLINE"),
#endif
#ifdef wxUSE_STATTEXT
  wxT("STATTEXT"),
#endif
#ifdef wxUSE_STATUSBAR
  wxT("STATUSBAR"),
#endif
#ifdef wxUSE_TOGGLEBTN
  wxT("TOGGLEBTN"),
#endif
#ifdef wxUSE_TAB_DIALOG
  wxT("TAB_DIALOG"),
#endif
#ifdef wxUSE_TABDIALOG
  wxT("TABDIALOG"),
#endif
#ifdef wxUSE_TEXTCTRL
  wxT("TEXTCTRL"),
#endif
#ifdef wxUSE_TOOLBAR
  wxT("TOOLBAR"),
#endif
#ifdef wxUSE_TOOLBAR_NATIVE
  wxT("TOOLBAR_NATIVE"),
#endif
#ifdef wxUSE_TOOLBAR_SIMPLE
  wxT("TOOLBAR_SIMPLE"),
#endif
#ifdef wxUSE_BUTTONBAR
  wxT("BUTTONBAR"),
#endif
#ifdef wxUSE_TREELAYOUT
  wxT("TREELAYOUT"),
#endif
#ifdef wxUSE_TREECTRL
  wxT("TREECTRL"),
#endif
#ifdef wxUSE_LONGLONG
  wxT("LONGLONG"),
#endif
#ifdef wxUSE_GEOMETRY
  wxT("GEOMETRY"),
#endif
#ifdef wxUSE_CMDLINE_PARSER
  wxT("CMDLINE_PARSER"),
#endif
#ifdef wxUSE_DATETIME
  wxT("DATETIME"),
#endif
#ifdef wxUSE_FILE
  wxT("FILE"),
#endif
#ifdef wxUSE_FFILE
  wxT("FFILE"),
#endif
#ifdef wxUSE_FSVOLUME
  wxT("FSVOLUME"),
#endif
#ifdef wxUSE_TEXTBUFFER
  wxT("TEXTBUFFER"),
#endif
#ifdef wxUSE_TEXTFILE
  wxT("TEXTFILE"),
#endif
#ifdef wxUSE_LOG
  wxT("LOG"),
#endif
#ifdef wxUSE_LOGWINDOW
  wxT("LOGWINDOW"),
#endif
#ifdef wxUSE_LOGGUI
  wxT("LOGGUI"),
#endif
#ifdef wxUSE_LOG_DIALOG
  wxT("LOG_DIALOG"),
#endif
#ifdef wxUSE_STOPWATCH
  wxT("STOPWATCH"),
#endif
#ifdef wxUSE_TIMEDATE
  wxT("TIMEDATE"),
#endif
#ifdef wxUSE_WAVE
  wxT("WAVE"),
#endif
#ifdef wxUSE_SOUND
  wxT("SOUND"),
#endif
#ifdef wxUSE_CONFIG
  wxT("CONFIG"),
#endif
#ifdef wxUSE_FONTMAP
  wxT("FONTMAP"),
#endif
#ifdef wxUSE_INTL
  wxT("INTL"),
#endif
#ifdef wxUSE_PROTOCOL
  wxT("PROTOCOL"),
#endif
#ifdef wxUSE_PROTOCOL_FILE
  wxT("PROTOCOL_FILE"),
#endif
#ifdef wxUSE_PROTOCOL_FTP
  wxT("PROTOCOL_FTP"),
#endif
#ifdef wxUSE_PROTOCOL_HTTP
  wxT("PROTOCOL_HTTP"),
#endif
#ifdef wxUSE_STREAMS
  wxT("STREAMS"),
#endif
#ifdef wxUSE_SOCKETS
  wxT("SOCKETS"),
#endif
#ifdef wxUSE_DIALUP_MANAGER
  wxT("DIALUP_MANAGER"),
#endif
#ifdef wxUSE_STD_IOSTREAM
  wxT("STD_IOSTREAM"),
#endif
#ifdef wxUSE_DYNLIB_CLASS
  wxT("DYNLIB_CLASS"),
#endif
#ifdef wxUSE_DYNAMIC_LOADER
  wxT("DYNAMIC_LOADER"),
#endif
#ifdef wxUSE_TIMER
  wxT("TIMER"),
#endif
#ifdef wxUSE_AFM_FOR_POSTSCRIPT
  wxT("AFM_FOR_POSTSCRIPT"),
#endif
#ifdef wxUSE_NORMALIZED_PS_FONTS
  wxT("NORMALIZED_PS_FONTS"),
#endif
#ifdef wxUSE_POSTSCRIPT
  wxT("POSTSCRIPT"),
#endif
#ifdef wxUSE_WCHAR_T
  wxT("WCHAR_T"),
#endif
#ifdef wxUSE_UNICODE
  wxT("UNICODE"),
#endif
#ifdef wxUSE_UNICODE_MSLU
  wxT("UNICODE_MSLU"),
#endif
#ifdef wxUSE_URL
  wxT("URL"),
#endif
#ifdef wxUSE_WCSRTOMBS
  wxT("WCSRTOMBS"),
#endif
#ifdef wxUSE_EXPERIMENTAL_
  wxT("EXPERIMENTAL_PRINTF"),
#endif
#ifdef wxUSE_IPC
  wxT("IPC"),
#endif
#ifdef wxUSE_X_RESOURCES
  wxT("X_RESOURCES"),
#endif
#ifdef wxUSE_CLIPBOARD
  wxT("CLIPBOARD"),
#endif
#ifdef wxUSE_DATAOBJ
  wxT("DATAOBJ"),
#endif
#ifdef wxUSE_TOOLTIPS
  wxT("TOOLTIPS"),
#endif
#ifdef wxUSE_DRAG_AND_DROP
  wxT("DRAG_AND_DROP"),
#endif
#ifdef wxUSE_OLE
  wxT("OLE"),
#endif
#ifdef wxUSE_SPLINES
  wxT("SPLINES"),
#endif
#ifdef wxUSE_MDI_ARCHITECTURE
  wxT("MDI_ARCHITECTURE"),
#endif
#ifdef wxUSE_DOC_VIEW_ARCHITECTURE
  wxT("DOC_VIEW_ARCHITECTURE"),
#endif
#ifdef wxUSE_PRINTING_ARCHITECTURE
  wxT("PRINTING_ARCHITECTURE"),
#endif
#ifdef wxUSE_PROLOGIO
  wxT("PROLOGIO"),
#endif
#ifdef wxUSE_RESOURCES
  wxT("RESOURCES"),
#endif
#ifdef wxUSE_WX_RESOURCES
  wxT("WX_RESOURCES"),
#endif
#ifdef wxUSE_HELP
  wxT("HELP"),
#endif
#ifdef wxUSE_WXHTML_HELP
  wxT("WXHTML_HELP"),
#endif
#ifdef wxUSE_MS_HTML_HELP
  wxT("MS_HTML_HELP"),
#endif
#ifdef wxUSE_IOSTREAMH
  wxT("IOSTREAMH"),
#endif
#ifdef wxUSE_APPLE_IEEE
  wxT("APPLE_IEEE"),
#endif
#ifdef wxUSE_MEMORY_TRACING
  wxT("MEMORY_TRACING"),
#endif
#ifdef wxUSE_DEBUG_NEW_ALWAYS
  wxT("DEBUG_NEW_ALWAYS"),
#endif
#ifdef wxUSE_DEBUG_CONTEXT
  wxT("DEBUG_CONTEXT"),
#endif
#ifdef wxUSE_GLOBAL_MEMORY_OPERATORS
  wxT("GLOBAL_MEMORY_OPERATORS"),
#endif
#ifdef wxUSE_SPLINES
  wxT("SPLINES"),
#endif
#ifdef wxUSE_DYNAMIC_CLASSES
  wxT("DYNAMIC_CLASSES"),
#endif
#ifdef wxUSE_METAFILE
  wxT("METAFILE"),
#endif
#ifdef wxUSE_ENH_METAFILE
  wxT("ENH_METAFILE"),
#endif
#ifdef wxUSE_MINIFRAME
  wxT("MINIFRAME"),
#endif
#ifdef wxUSE_HTML
  wxT("HTML"),
#endif
#ifdef wxUSE_FILESYSTEM
  wxT("FILESYSTEM"),
#endif
#ifdef wxUSE_FS_INET
  wxT("FS_INET"),
#endif
#ifdef wxUSE_FS_ZIP
  wxT("FS_ZIP"),
#endif
#ifdef wxUSE_BUSYINFO
  wxT("BUSYINFO"),
#endif
#ifdef wxUSE_ZIPSTREAM
  wxT("ZIPSTREAM"),
#endif
#ifdef wxUSE_PALETTE
  wxT("PALETTE"),
#endif
#ifdef wxUSE_IMAGE
  wxT("IMAGE"),
#endif
#ifdef wxUSE_GIF
  wxT("GIF"),
#endif
#ifdef wxUSE_PCX
  wxT("PCX"),
#endif
#ifdef wxUSE_IFF
  wxT("IFF"),
#endif
#ifdef wxUSE_PNM
  wxT("PNM"),
#endif
#ifdef wxUSE_XPM
  wxT("XPM"),
#endif
#ifdef wxUSE_ICO_CUR
  wxT("ICO_CUR"),
#endif

// detect using optional libraries in the contrib hierarchy.
#ifdef wxUSE_STC
  wxT("STC"),
#endif
  NULL
};


static const wxChar* hasDefs[] = {
#ifdef wxHAS_POWER_EVENTS
  wxT("POWER_EVENTS"),
#endif
#ifdef wxHAS_RADIO_MENU_ITEMS
  wxT("RADIO_MENU_ITEMS"),
#endif
  NULL
};

/*-----------------------------------------------------------------------------
  EXTERN C
-----------------------------------------------------------------------------*/
extern "C"
{

/*-----------------------------------------------------------------------------
  String
-----------------------------------------------------------------------------*/
typedef char utf8char;

EWXWEXPORT(wxString*,wxString_Create)(wxChar* buffer)
{
  return new wxString(buffer);
}

EWXWEXPORT(wxString*,wxString_CreateUTF8)(utf8char* buffer)
{
  return new wxString (buffer,wxConvUTF8);
}

EWXWEXPORT(wxString*,wxString_CreateLen)(wxChar* buffer,int len)
{
  return new wxString(buffer,len);
}

EWXWEXPORT(void,wxString_Delete)(wxString* s)
{
  delete s;
}

EWXWEXPORT(int,wxString_GetString)(wxString* s,wxChar* buffer)
{
  if (buffer) memcpy (buffer, s->wc_str(), s->Length() * sizeof(wxChar));
  return s->Length();
}

EWXWEXPORT(size_t,wxString_Length)(wxString* s)
{
  return s->length();
}

EWXWEXPORT(wxCharBuffer*,wxString_GetUtf8)(wxString* s)
{
  wxCharBuffer *cb = new wxCharBuffer;
  *cb = s->utf8_str();
  return cb;
}

EWXWEXPORT(utf8char*,wxCharBuffer_DataUtf8)(wxCharBuffer* cb)
{
  return (utf8char*)cb->data();
}

EWXWEXPORT(void,wxCharBuffer_Delete)(wxCharBuffer* cb)
{
  delete cb;
}
/*-----------------------------------------------------------------------------
  Point
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*,wxPoint_Create)(int x,int y)
{
  return new wxPoint(x,y);
}

EWXWEXPORT(void,wxPoint_Delete)(void* p)
{
  delete (wxPoint*)p;
}

EWXWEXPORT(int,wxPoint_GetX)(void* p)
{
  return ((wxPoint*)p)->x;
}

EWXWEXPORT(int,wxPoint_GetY)(void* p)
{
  return ((wxPoint*)p)->y;
}

EWXWEXPORT(void,wxPoint_SetX)(void* p,int x)
{
  ((wxPoint*)p)->x = x;
}

EWXWEXPORT(void,wxPoint_SetY)(void* p,int y)
{
  ((wxPoint*)p)->y = y;
}

/*-----------------------------------------------------------------------------
  Size
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*,wxSize_Create)(int w,int h)
{
  return new wxSize(w,h);
}

EWXWEXPORT(void,wxSize_Delete)(void* s)
{
  delete (wxSize*)s;
}

EWXWEXPORT(int,wxSize_GetWidth)(void* s)
{
  return ((wxSize*)s)->GetWidth();
}

EWXWEXPORT(int,wxSize_GetHeight)(void* s)
{
  return ((wxSize*)s)->GetHeight();
}

EWXWEXPORT(void,wxSize_SetWidth)(wxSize* s,int w)
{
  s->SetWidth(w);
}

EWXWEXPORT(void,wxSize_SetHeight)(wxSize* s,int h)
{
  s->SetHeight(h);
}

/*-----------------------------------------------------------------------------
  Rect
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*,wxRect_Create)(int x,int y,int w,int h)
{
  return new wxRect(x,y,w,h);
}

EWXWEXPORT(void,wxRect_Delete)(void* r)
{
  delete (wxRect*)r;
}

EWXWEXPORT(int,wxRect_GetX)(wxRect* r)
{
  return r->GetX();
}

EWXWEXPORT(int,wxRect_GetY)(wxRect* r)
{
  return r->GetY();
}

EWXWEXPORT(int,wxRect_GetWidth)(wxRect* r)
{
  return r->GetWidth();
}

EWXWEXPORT(int,wxRect_GetHeight)(wxRect* r)
{
  return r->GetHeight();
}

EWXWEXPORT(void,wxRect_SetX)(wxRect* r,int x)
{
  r->SetX(x);
}

EWXWEXPORT(void,wxRect_SetY)(wxRect* r,int y)
{
  r->SetY(y);
}

EWXWEXPORT(void,wxRect_SetWidth)(wxRect* r,int w)
{
  r->SetWidth(w);
}

EWXWEXPORT(void,wxRect_SetHeight)(wxRect* r,int h)
{
  r->SetHeight(h);
}

/*-----------------------------------------------------------------------------
  pre-processor
-----------------------------------------------------------------------------*/
EWXWEXPORT(int,wxVersionNumber)()
{
  return wxVERSION_NUMBER;
}

EWXWEXPORT(wxString*,wxVersionString)()
{
  return new wxString(wxVERSION_STRING);
}

EWXWEXPORT(int,wxIsDefined)(wxChar* s)
{
  int i;
  if (s==NULL) return 0;
  /* check define */
  for( i=0; defineDefs[i] != NULL; i++ ) {
    if (wxStrcmp(s,defineDefs[i]) == 0) return 1;
  }
  /* check wxUSE_XXX */
  if (wxStrncmp(s,wxT("wxUSE_"),6) == 0) {
    const wxChar* t = s+6;
    for( i=0; useDefs[i] != NULL; i++ ) {
      if (wxStrcmp(t,useDefs[i]) == 0) return 1;
    }
  }
  /* check wxHAS_XXX */
  if (wxStrncmp(s,wxT("wxHAS_"),6) == 0) {
    const wxChar* t = s+6;
    for( i=0; hasDefs[i] != NULL; i++ ) {
      if (wxStrcmp(t,hasDefs[i]) == 0) return 1;
    }
  }
  return 0;
}

EWXWEXPORT(void*,wxcMalloc)(int size)
{
  return malloc(size);
}

EWXWEXPORT(void,wxcFree)(void* p)
{
  if (p!=NULL) free(p);
}

EWXWEXPORT(wxColour*,wxcSystemSettingsGetColour)(int systemColour)
{
   wxColour* colour = new wxColour();
   *colour = wxSystemSettings::GetColour( (wxSystemColour)systemColour );
   return colour;
}

EWXWEXPORT(void,wxcWakeUpIdle)(void)
{
  wxWakeUpIdle();
}

/*-----------------------------------------------------------------------------
  delete
-----------------------------------------------------------------------------*/
EWXWEXPORT(void,wxCursor_Delete)(wxCursor* self)
{
  delete self;
}

EWXWEXPORT(void,wxDateTime_Delete)(wxDateTime* self)
{
  delete self;
}

/*-----------------------------------------------------------------------------
  frame
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxString*,wxFrame_GetTitle)(wxFrame* self)
{
  wxString *result = new wxString();
  *result = self->GetTitle();
  return result;
}

EWXWEXPORT(void,wxFrame_SetTitle)(wxFrame* self,wxString* _txt)
{
  self->SetTitle(*_txt);
}

EWXWEXPORT(bool,wxFrame_SetShape)(wxFrame* self,wxRegion* region)
{
  return self->SetShape( *region );
}

EWXWEXPORT(bool,wxFrame_ShowFullScreen)(wxFrame* self,bool show,int style)
{
  return self->ShowFullScreen( show, style );
}

EWXWEXPORT(bool,wxFrame_IsFullScreen)(wxFrame* self)
{
  return self->IsFullScreen();
}

EWXWEXPORT(void,wxFrame_Centre)(wxFrame* self,int orientation)
{
  self->Centre();
}


EWXWEXPORT(void,wxNotebook_AssignImageList)(wxNotebook* self,wxImageList* imageList)
{
  self->AssignImageList(imageList);
}

/*-----------------------------------------------------------------------------
  menu & toolbar
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxMenuBar*,wxMenu_GetMenuBar)(wxMenu* self)
{
  return self->GetMenuBar();
}


EWXWEXPORT(wxFrame*,wxMenuBar_GetFrame)(wxMenuBar* self)
{
  return self->GetFrame();
}

EWXWEXPORT(void,wxToolBar_AddTool2)(wxToolBar* self,int toolId,wxString* label,wxBitmap* bmp,wxBitmap* bmpDisabled,int itemKind,wxString* shortHelp,wxString* longHelp)
{
  self->AddTool(toolId,*label,*bmp,*bmpDisabled,(wxItemKind)itemKind,*shortHelp,*longHelp,NULL);
}

/*-----------------------------------------------------------------------------
  listctrl
-----------------------------------------------------------------------------*/
EWXWEXPORT(int,wxListEvent_GetCacheFrom)(wxListEvent* self)
{
  return self->GetCacheFrom();
}

EWXWEXPORT(int,wxListEvent_GetCacheTo)(wxListEvent* self)
{
  return self->GetCacheTo();
}


EWXWEXPORT(void,wxListCtrl_AssignImageList)(wxListCtrl* self,wxImageList* images,int which)
{
  self->AssignImageList(images,which);
}



EWXWEXPORT(void,wxListCtrl_GetColumn2)(wxListCtrl* self,int col,wxListItem* item)
{
  bool success = self->GetColumn(col,*item);
  if (!success) item->SetId(-1);
}

EWXWEXPORT(void,wxListCtrl_GetItem2)(wxListCtrl* self,wxListItem* info)
{
  bool success = self->GetItem(*info);
  if (!success) info->SetId(-1);
}

EWXWEXPORT(wxPoint*,wxListCtrl_GetItemPosition2)(wxListCtrl* self,int item)
{
	wxPoint* pos = new wxPoint();
	bool success = self->GetItemPosition((long)item, *pos);
	if (success) {
		return pos;
	}
	else {
		delete pos;
		wxPoint* pt = new wxPoint(-1,-1);
		return pt;
	}
}


struct SortData {
  long id;
  wxClosure* closure;
};

int wxCALLBACK sortCallBack( wxIntPtr item1, wxIntPtr item2, wxIntPtr data )
{
  wxClosure* closure = ((SortData*)data)->closure;
  long       id      = ((SortData*)data)->id;

  wxCommandEvent event( wxEVT_SORT, id );
  event.SetInt(item1);
  event.SetExtraLong(item2);
  closure->Invoke(&event);
  return event.GetInt();
}

EWXWEXPORT(bool,wxListCtrl_SortItems2)(wxListCtrl* self,wxClosure* closure)
{
  SortData sortData = { self->GetId(), closure };
  return self->SortItems( sortCallBack, (wxIntPtr)&sortData );
}



/*-----------------------------------------------------------------------------
  DC
-----------------------------------------------------------------------------*/
EWXWEXPORT(void,wxDC_GetPixel2)(wxDC* self,int x,int y,wxColour* col)
{
  bool success = self->GetPixel((wxCoord)x, (wxCoord)y, col);
  if (!success) *col = wxNullColour;
}


/*-----------------------------------------------------------------------------
  Object & static ClassInfo
-----------------------------------------------------------------------------*/
EWXWEXPORT(bool,wxObject_IsKindOf)(wxObject* self,wxClassInfo* classInfo)
{
  return self->IsKindOf(classInfo);
}

EWXWEXPORT(wxClassInfo*,wxObject_GetClassInfo)(wxObject* self)
{
  return self->GetClassInfo();
}

/* optimize */
EWXWEXPORT(bool,wxObject_IsScrolledWindow)(wxObject* self)
{
  return self->IsKindOf(CLASSINFO(wxScrolledWindow));
}

EWXWEXPORT(void,wxObject_Delete)(wxObject* self)
{
  delete self;
}

/*-----------------------------------------------------------------------------
  classinfo
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxClassInfo*,wxClassInfo_FindClass)(wxString* name)
{
  return wxClassInfo::FindClass(*name);
}

EWXWEXPORT(wxString*,wxClassInfo_GetClassNameEx)(wxClassInfo* self)
{
  wxString *result = new wxString();
  *result = self->GetClassName();
  return result;
}

EWXWEXPORT(wxString*,wxClassInfo_GetBaseClassName1)(wxClassInfo* self)
{
  wxString *result = new wxString();
  *result = self->GetBaseClassName1();
  return result;
}

EWXWEXPORT(wxString*,wxClassInfo_GetBaseClassName2)(wxClassInfo* self)
{
  wxString *result = new wxString();
  *result = self->GetBaseClassName2();
  return result;
}

EWXWEXPORT(bool,wxClassInfo_IsKindOfEx)(wxClassInfo* self,wxClassInfo* classInfo)
{
  return self->IsKindOf(classInfo);
}

EWXWEXPORT(int,wxClassInfo_GetSize)(wxClassInfo* self)
{
  return (self)->GetSize();
}

/*-----------------------------------------------------------------------------
  window
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxPoint*,wxWindow_ConvertPixelsToDialogEx)(wxWindow* self,int x,int y)
{
	wxPoint* pt = new wxPoint();
	*pt = self->ConvertPixelsToDialog( wxPoint(x, y) );
	return pt;
}

EWXWEXPORT(wxPoint*,wxWindow_ConvertDialogToPixelsEx)(wxWindow* self,int x,int y)
{
	wxPoint* pt = new wxPoint();
	*pt = self->ConvertDialogToPixels( wxPoint(x, y) );
	return pt;
}


EWXWEXPORT(void,wxWindow_SetClientObject)(wxWindow* self,wxClientData* obj)
{
	self->SetClientObject(obj);
}


EWXWEXPORT(wxPoint*,wxWindow_ScreenToClient2)(wxWindow* self,int x,int y)
{
	wxPoint* pt = new wxPoint();
	*pt = self->ScreenToClient( wxPoint(x, y) );
	return pt;
}



EWXWEXPORT(wxPoint*,wxcGetMousePosition)()
{
	wxPoint* pt = new wxPoint();
	*pt = wxGetMousePosition();
	return pt;
}

/*-----------------------------------------------------------------------------
  scrolledwindow
-----------------------------------------------------------------------------*/
EWXWEXPORT(void,wxScrolledWindow_SetScrollRate)(wxScrolledWindow* self,int xstep,int ystep)
{
  self->SetScrollRate(xstep,ystep);
}

/*-----------------------------------------------------------------------------
  mouse
-----------------------------------------------------------------------------*/
EWXWEXPORT(int,wxMouseEvent_GetWheelDelta)(wxMouseEvent* self)
{
  return self->GetWheelDelta();
}

EWXWEXPORT(int,wxMouseEvent_GetWheelRotation)(wxMouseEvent* self)
{
  return self->GetWheelRotation();
}

EWXWEXPORT(int,wxMouseEvent_GetButton)(wxMouseEvent* self)
{
  return self->GetButton();
}

/*-----------------------------------------------------------------------------
  DC
-----------------------------------------------------------------------------*/
EWXWEXPORT(double,wxDC_GetUserScaleX)(wxDC* dc)
{
  double x = 1.0;
  double y = 1.0;
  dc->GetUserScale(&x,&y);
  return x;
}


EWXWEXPORT(double,wxDC_GetUserScaleY)(wxDC* dc)
{
  double x = 1.0;
  double y = 1.0;
  dc->GetUserScale(&x,&y);
  return y;
}


/*-----------------------------------------------------------------------------
  timers
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxTimerEx*,wxTimerEx_Create)()
{
  return new wxTimerEx();
}

EWXWEXPORT(void,wxTimerEx_Connect)(wxTimerEx* self,wxClosure* _closure)
{
  self->Connect(_closure);
}

EWXWEXPORT(wxClosure*,wxTimerEx_GetClosure)(wxTimerEx* self)
{
  return self->GetClosure();
}


/*-----------------------------------------------------------------------------
  menu items
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxMenuItem*,wxMenuItem_CreateSeparator)()
{
  return new wxMenuItem( NULL, wxID_SEPARATOR, wxT(""), wxT(""), wxITEM_SEPARATOR, NULL );
}


EWXWEXPORT(wxMenuItem*,wxMenuItem_CreateEx)(int id,wxString* text,wxString* helpstr,int itemkind,wxMenu* submenu)
{
  return new wxMenuItem( NULL, id,*text,*helpstr, (wxItemKind)itemkind, submenu );
}


EWXWEXPORT(void,wxMenu_AppendRadioItem)(wxMenu* self,int id,wxString* text,wxString* help)
{
#ifdef wxHAS_RADIO_MENU_ITEMS
  self->AppendRadioItem(id,*text,*help);
#else
  self->AppendCheckItem(id,*text,*help);
#endif
}


/*------------------------------------------------------------------------------
  process
------------------------------------------------------------------------------*/
EWXWEXPORT(bool,wxProcess_IsErrorAvailable)(wxProcess* self)
{
    return self->IsErrorAvailable();
}

EWXWEXPORT(bool,wxProcess_IsInputAvailable)(wxProcess* self)
{
    return self->IsInputAvailable();
}

EWXWEXPORT(bool,wxProcess_IsInputOpened)(wxProcess* self)
{
    return self->IsInputOpened();
}

EWXWEXPORT(wxProcess*,wxProcess_Open)(wxString* cmd,int flags)
{
    return wxProcess::Open( *cmd, ((flags | wxEXEC_ASYNC) & ~wxEXEC_SYNC) );
}

EWXWEXPORT(wxKillError,wxKill)(int pid,wxSignal signal)
{
  return wxProcess::Kill(pid,signal);
}

EWXWEXPORT(void,wxStreamBase_Delete)(wxStreamBase* stream)
{
  if (stream) delete stream;
}

/*------------------------------------------------------------------------------
  TextCtrl
------------------------------------------------------------------------------*/
EWXWEXPORT(int,wxTextCtrl_EmulateKeyPress)(wxTextCtrl* self,wxKeyEvent* keyevent)
{
    return self->EmulateKeyPress(*keyevent);
}


EWXWEXPORT(void*,wxTextCtrl_GetDefaultStyle)(wxTextCtrl* self)
{
    return (void*)& self->GetDefaultStyle();
}

EWXWEXPORT(wxString*,wxTextCtrl_GetRange)(wxTextCtrl* self,long from,long to)
{
    wxString *result = new wxString();
    *result = self->GetRange(from, to);
    return result;
}

EWXWEXPORT(wxString*,wxTextCtrl_GetStringSelection)(wxTextCtrl* self)
{
    wxString *result = new wxString();
    *result = self->GetStringSelection();
    return result;
}

EWXWEXPORT(bool,wxTextCtrl_IsMultiLine)(wxTextCtrl* self)
{
    return  self->IsMultiLine();
}

EWXWEXPORT(bool,wxTextCtrl_IsSingleLine)(wxTextCtrl* self)
{
    return self->IsSingleLine(   );
}


EWXWEXPORT(bool,wxTextCtrl_SetDefaultStyle)(wxTextCtrl* self,wxTextAttr* style)
{
    return self->SetDefaultStyle(*style);
}

EWXWEXPORT(void,wxTextCtrl_SetMaxLength)(wxTextCtrl* self,long len)
{
    self->SetMaxLength( len  );
}

EWXWEXPORT(bool,wxTextCtrl_SetStyle)(wxTextCtrl* self,long start,long end,wxTextAttr* style)
{
    return self->SetStyle(start, end,*style);
}

/*------------------------------------------------------------------------------
  TextAttr
------------------------------------------------------------------------------*/
EWXWEXPORT(wxTextAttr*,wxTextAttr_CreateDefault)()
{
  return new wxTextAttr();
}

EWXWEXPORT(wxTextAttr*,wxTextAttr_Create)(wxColour* colText,wxColour* colBack,wxFont* font)
{
    return new wxTextAttr( *colText,*colBack,*font );
}

EWXWEXPORT(void,wxTextAttr_Delete)(wxTextAttr* self)
{
    delete self;
}

EWXWEXPORT(void,wxTextAttr_GetBackgroundColour)(wxTextAttr* self,wxColour* colour)
{
    *colour = self->GetBackgroundColour();
}

EWXWEXPORT(void,wxTextAttr_GetFont)(wxTextAttr* self,wxFont* font)
{
    *font = self->GetFont();
}

EWXWEXPORT(void,wxTextAttr_GetTextColour)(wxTextAttr* self,wxColour* colour)
{
    *colour = self->GetTextColour();
}

EWXWEXPORT(bool,wxTextAttr_HasBackgroundColour)(wxTextAttr* self)
{
    return  self->HasBackgroundColour();
}

EWXWEXPORT(bool,wxTextAttr_HasFont)(wxTextAttr* self)
{
    return self->HasFont();
}

EWXWEXPORT(bool,wxTextAttr_HasTextColour)(wxTextAttr* self)
{
    return  self->HasTextColour();
}

EWXWEXPORT(bool,wxTextAttr_IsDefault)(wxTextAttr* self)
{
    return  self->IsDefault(   );
}

EWXWEXPORT(void,wxTextAttr_SetTextColour)(wxTextAttr* self,wxColour* colour)
{
   self->SetTextColour(*colour);
}

EWXWEXPORT(void,wxTextAttr_SetBackgroundColour)(wxTextAttr* self,wxColour* colour)
{
   self->SetBackgroundColour(*colour);
}

EWXWEXPORT(void,wxTextAttr_SetFont)(wxTextAttr* self,wxFont* font)
{
   self->SetFont(*font);
}

/*------------------------------------------------------------------------------
  progress dialog
------------------------------------------------------------------------------*/
EWXWEXPORT(wxProgressDialog*,wxProgressDialog_Create)(wxString* title,wxString* message,int max,wxWindow* parent,int style)
{
  return new wxProgressDialog( *title, *message, max, parent, style );
}

EWXWEXPORT(bool,wxProgressDialog_Update)(wxProgressDialog* self,int value)
{
  return self->Update(value);
}

EWXWEXPORT(bool,wxProgressDialog_UpdateWithMessage)(wxProgressDialog* self,int value,wxString* message)
{
  return self->Update(value,*message);
}

EWXWEXPORT(void,wxProgressDialog_Resume)(wxProgressDialog* self)
{
  self->Resume();
}


/*------------------------------------------------------------------------------
  standard dialogs
------------------------------------------------------------------------------*/
EWXWEXPORT(void,wxGetColourFromUser)(wxWindow* parent,wxColour* colInit,wxColour* colour)
{
  *colour = wxGetColourFromUser(parent, *colInit);
}

EWXWEXPORT(void,wxGetFontFromUser)(wxWindow* parent,wxFont* fontInit,wxFont* font)
{
  *font = wxGetFontFromUser(parent, *fontInit);
}

EWXWEXPORT(int, wxGetPasswordFromUser)(wxChar* message, wxChar* caption, wxChar* defaultText, wxWindow* parent, wxChar* _buf )
{
/* we use a complicated caching method as we don't want to call getpassword twice :-) */
  static wxChar* resultBuffer = NULL;
  if (_buf==NULL) {
    if (resultBuffer) { free(resultBuffer); resultBuffer = NULL; }
    wxString result = wxGetPasswordFromUser( message, caption, defaultText, parent );
    resultBuffer = (wxChar*)malloc( (result.Length() + 1) * sizeof(wxChar) );
    if (resultBuffer) {
      wxStrcpy( resultBuffer, result.c_str() ); /* save result */
      return result.Length();
    }
    else {
      return 0;
    }
  }
  else if (resultBuffer) {
    int len = wxStrlen(resultBuffer);
    memcpy(_buf, resultBuffer, len * sizeof(wxChar) );  /* copy saved result */
    free(resultBuffer);
    resultBuffer = NULL;
    return len;
  }
  else {
    return 0;
  }
}

EWXWEXPORT(int, wxGetTextFromUser)(wxChar* message, wxChar* caption, wxChar* defaultText, wxWindow* parent, int x, int y, int center, wxChar* _buf )
{
/* we use a complicated caching method as we don't want to call gettext twice :-) */
  static wxChar* resultBuffer = NULL;
  if (_buf==NULL) {
    if (resultBuffer) { free(resultBuffer); resultBuffer = NULL; }
    wxString result = wxGetTextFromUser( message, caption, defaultText, parent, x, y, center!=0 );
    resultBuffer = (wxChar*)malloc( (result.Length() + 1) * sizeof(wxChar) );
    if (resultBuffer) {
      wxStrcpy( resultBuffer, result.c_str() ); /* save result */
      return result.Length();
    }
    else {
      return 0;
    }
  }
  else if (resultBuffer) {
    int len = wxStrlen(resultBuffer);
    memcpy(_buf, resultBuffer, len * sizeof(wxChar) );  /* copy saved result */
    free(resultBuffer);
    resultBuffer = NULL;
    return len;
  }
  else {
    return 0;
  }
}

EWXWEXPORT(long,wxGetNumberFromUser)(wxString* message,wxString* prompt,wxString* caption,long value,long min,long max,wxWindow* parent,int x,int y)
{
  return wxGetNumberFromUser(*message, *prompt, *caption, value, min, max, parent, wxPoint(x, y) );
}

EWXWEXPORT(void,wxcBell)(void)
{
  wxBell();
}

EWXWEXPORT(void,wxcBeginBusyCursor)(void)
{
  wxBeginBusyCursor();
}

EWXWEXPORT(int,wxcIsBusy)(void)
{
  return (wxIsBusy());
}

EWXWEXPORT(void,wxcEndBusyCursor)(void)
{
  wxEndBusyCursor();
}

/*-----------------------------------------------------------------------------
  wxInputSink
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxInputSink*,wxInputSink_Create)(wxInputStream* input,wxEvtHandler* evtHandler,int bufferLen)
{
  return new wxInputSink(input,evtHandler,bufferLen);
}

EWXWEXPORT(int,wxInputSink_GetId)(wxInputSink* self)
{
  return self->GetId();
}

EWXWEXPORT(void,wxInputSink_Start)(wxInputSink* self)
{
  self->Start();
}


EWXWEXPORT(int,wxInputSinkEvent_LastError)(wxInputSinkEvent* self)
{
  return self->LastError();
}

EWXWEXPORT(int,wxInputSinkEvent_LastRead)(wxInputSinkEvent* self)
{
  return self->LastRead();
}

EWXWEXPORT(char*,wxInputSinkEvent_LastInput)(wxInputSinkEvent* self)
{
  return self->LastInput();
}


/*-----------------------------------------------------------------------------
  html window
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*,wxcHtmlEvent_GetMouseEvent)(wxcHtmlEvent* self)
{
    return (void*)(self->GetMouseEvent());
}

EWXWEXPORT(void*,wxcHtmlEvent_GetHtmlCell)(wxcHtmlEvent* self)
{
    return (void*)(self->GetHtmlCell());
}

EWXWEXPORT(wxString*,wxcHtmlEvent_GetHtmlCellId)(wxcHtmlEvent* self)
{
    return self->GetHtmlCellId();
}

EWXWEXPORT(wxString*,wxcHtmlEvent_GetHref)(wxcHtmlEvent* self)
{
    return self->GetHref();
}

EWXWEXPORT(wxString*,wxcHtmlEvent_GetTarget)(wxcHtmlEvent* self)
{
    return self->GetTarget();
}

EWXWEXPORT(wxPoint*,wxcHtmlEvent_GetLogicalPosition)(wxcHtmlEvent* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetLogicalPosition();
	return pt;
}


/*-----------------------------------------------------------------------------
  html window
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxHtmlWindow*,wxHtmlWindow_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,long _stl, wxString* _txt)
{
    return new wxHtmlWindow(_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, *_txt );
}

EWXWEXPORT(wxHtmlWindow*, wxcHtmlWindow_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,long _stl, wxString* _txt)
{
    return new wxcHtmlWindow(_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, *_txt );
}


EWXWEXPORT(bool,wxHtmlWindow_AppendToPage)(wxHtmlWindow* self,wxString* source)
{
    return self->AppendToPage(*source);
}

EWXWEXPORT(void*,wxHtmlWindow_GetInternalRepresentation)(wxHtmlWindow* self)
{
    return (void*)self->GetInternalRepresentation();
}

EWXWEXPORT(wxString*,wxHtmlWindow_GetOpenedAnchor)(wxHtmlWindow* self)
{
	wxString *result = new wxString();
	*result = self->GetOpenedAnchor();
	return result;
}

EWXWEXPORT(wxString*,wxHtmlWindow_GetOpenedPage)(wxHtmlWindow* self)
{
	wxString *result = new wxString();
	*result = self->GetOpenedPage();
	return result;
}

EWXWEXPORT(wxString*,wxHtmlWindow_GetOpenedPageTitle)(wxHtmlWindow* self)
{
	wxString *result = new wxString();
	*result = self->GetOpenedPageTitle();
	return result;
}


EWXWEXPORT(void*,wxHtmlWindow_GetRelatedFrame)(wxHtmlWindow* self)
{
    return (void*)self->GetRelatedFrame(   );
}

EWXWEXPORT(bool,wxHtmlWindow_HistoryBack)(wxHtmlWindow* self)
{
    return self->HistoryBack();
}

EWXWEXPORT(bool,wxHtmlWindow_HistoryCanBack)(wxHtmlWindow* self)
{
    return self->HistoryCanBack();
}

EWXWEXPORT(bool,wxHtmlWindow_HistoryCanForward)(wxHtmlWindow* self)
{
    return self->HistoryCanForward();
}

EWXWEXPORT(void,wxHtmlWindow_HistoryClear)(wxHtmlWindow* self)
{
    self->HistoryClear(   );
}

EWXWEXPORT(bool,wxHtmlWindow_HistoryForward)(wxHtmlWindow* self)
{
    return self->HistoryForward();
}

EWXWEXPORT(bool,wxHtmlWindow_LoadPage)(wxHtmlWindow* self,wxString* location)
{
    return self->LoadPage(*location );
}


EWXWEXPORT(void,wxHtmlWindow_ReadCustomization)(wxHtmlWindow* self,wxConfigBase* cfg,wxString* path)
{
    self->ReadCustomization(  cfg,*path );
}

EWXWEXPORT(void,wxHtmlWindow_SetBorders)(wxHtmlWindow* self,int b)
{
    self->SetBorders( b );
}

EWXWEXPORT(void,wxHtmlWindow_SetFonts)(wxHtmlWindow* self,wxString* normal_face,wxString* fixed_face,int* sizes)
{
    self->SetFonts(*normal_face,*fixed_face, sizes );
}

EWXWEXPORT(int,wxHtmlWindow_SetPage)(wxHtmlWindow* self,wxString* source)
{
    return self->SetPage( *source );
}

EWXWEXPORT(void,wxHtmlWindow_SetRelatedFrame)(wxHtmlWindow* self,wxFrame* frame,wxString* format)
{
    self->SetRelatedFrame(frame,*format);
}

EWXWEXPORT(void,wxHtmlWindow_SetRelatedStatusBar)(wxHtmlWindow* self,int bar)
{
    self->SetRelatedStatusBar(bar);
}

EWXWEXPORT(void,wxHtmlWindow_WriteCustomization)(wxHtmlWindow* self,wxConfigBase* cfg,wxString* path)
{
    self->WriteCustomization(cfg,*path );
}

/*-----------------------------------------------------------------------------
  LOGGER
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxLogStderr*,wxLogStderr_Create)()
{
	return new wxLogStderr();
}

EWXWEXPORT(wxLogStderr*,wxLogStderr_CreateStdOut)()
{
	return new wxLogStderr(stdout);
}

EWXWEXPORT(wxLogNull*,wxLogNull_Create)()
{
	return new wxLogNull();
}

EWXWEXPORT(wxLogTextCtrl*,wxLogTextCtrl_Create)(wxTextCtrl* text)
{
	return new wxLogTextCtrl(text);
}

EWXWEXPORT(wxLogWindow*,wxLogWindow_Create)(wxFrame* parent,wxString* title,bool showit,bool passthrough)
{
	return new wxLogWindow(parent,*title,showit,passthrough);
}

EWXWEXPORT(wxFrame*,wxLogWindow_GetFrame)(wxLogWindow* self)
{
	return self->GetFrame();
}

EWXWEXPORT(void,wxLog_Delete)(wxLog* self)
{
	delete self;
}

EWXWEXPORT(void,wxLog_OnLog)(wxLog* self,int level,void* szString,int t)
{
	self->OnLog((wxLogLevel)level, (const wxChar*)szString, (time_t)t);
}

EWXWEXPORT(void,wxLog_Flush)(wxLog* self)
{
	self->Flush();
}

EWXWEXPORT(bool,wxLog_HasPendingMessages)(wxLog* self)
{
	return self->HasPendingMessages();
}

EWXWEXPORT(void,wxLog_FlushActive)(wxLog* self)
{
	self->FlushActive();
}

EWXWEXPORT(void*,wxLog_GetActiveTarget)()
{
	return (void*)wxLog::GetActiveTarget();
}

EWXWEXPORT(void*,wxLog_SetActiveTarget)(wxLog* pLogger)
{
	return (void*)wxLog::SetActiveTarget(pLogger);
}

EWXWEXPORT(void,wxLog_Suspend)(wxLog* self)
{
	self->Suspend();
}

EWXWEXPORT(void,wxLog_Resume)(wxLog* self)
{
	self->Resume();
}

EWXWEXPORT(void,wxLog_SetVerbose)(wxLog* self,bool bVerbose)
{
	self->SetVerbose(bVerbose);
}

EWXWEXPORT(void,wxLog_DontCreateOnDemand)(wxLog* self)
{
	self->DontCreateOnDemand();
}

// Obsolete
// EWXWEXPORT(void,wxLog_SetTraceMask)(wxLog* self,int ulMask)

EWXWEXPORT(void,wxLog_AddTraceMask)(wxLog* self,void* str)
{
	self->AddTraceMask((const wxChar*)str);
}

EWXWEXPORT(void,wxLog_RemoveTraceMask)(wxLog* self,void* str)
{
	self->RemoveTraceMask((const wxChar*)str);
}

EWXWEXPORT(void,wxLog_SetTimestamp)(wxLog* self,void* ts)
{
	self->SetTimestamp((const wxChar*)ts);
}

EWXWEXPORT(bool,wxLog_GetVerbose)(wxLog* self)
{
	return self->GetVerbose();
}

#if !wxCHECK_VERSION(3, 1, 0)
EWXWEXPORT(int,wxLog_GetTraceMask)(wxLog* self)
{
	return (int)self->GetTraceMask();
}
#endif

EWXWEXPORT(bool,wxLog_IsAllowedTraceMask)(wxLog* self,void* mask)
{
	return self->IsAllowedTraceMask((const wxChar*)mask);
}

EWXWEXPORT(void*,wxLog_GetTimestamp)(wxLog* self)
{
# if (wxVERSION_NUMBER >= 2900)
  return (void*)wxStrdup((self->GetTimestamp()).wchar_str());
#else
  return (void*)wxStrdup(self->GetTimestamp());
#endif
}


EWXWEXPORT(void,LogError)(wxString* _msg)
{
	wxLogError(*_msg);
}

EWXWEXPORT(void,LogFatalError)(wxString* _msg)
{
	wxLogFatalError(*_msg);
}

EWXWEXPORT(void,LogWarning)(wxString* _msg)
{
	wxLogWarning(*_msg);
}

EWXWEXPORT(void,LogMessage)(wxString* _msg)
{
	wxLogMessage(*_msg);
}

EWXWEXPORT(void,LogVerbose)(wxString* _msg)
{
	wxLogVerbose(*_msg);
}

EWXWEXPORT(void,LogStatus)(wxString* _msg)
{
	wxLogStatus(*_msg);
}

EWXWEXPORT(void,LogSysError)(wxString* _msg)
{
	wxLogSysError(*_msg);
}

EWXWEXPORT(void,LogDebug)(wxString* _msg)
{
	wxLogDebug(*_msg);
}

EWXWEXPORT(void,LogTrace)(wxString* mask,wxString* _msg)
{
	wxLogTrace(*mask,*_msg);
}

/*-----------------------------------------------------------------------------
  Grid text editor
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxGridCellTextEnterEditor*,wxGridCellTextEnterEditor_Ctor)()
{
	return new wxGridCellTextEnterEditor();
}

/*-----------------------------------------------------------------------------
  ConfigBase
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxConfigBase*,wxConfigBase_Get)()
{
	return wxConfigBase::Get();
}

EWXWEXPORT(void,wxConfigBase_Set)(wxConfigBase* self)
{
	wxConfigBase::Set( self );
}

EWXWEXPORT(wxFileConfig*,wxFileConfig_Create)(wxInputStream* inp)
{
	return new wxFileConfig( *inp );
}

/*-----------------------------------------------------------------------------
  PropertyGrid
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*,wxPropertyGrid_Append)(wxPropertyGrid* self, wxPGProperty * property)
{
        return (void*) self->Append(property);
}

EWXWEXPORT(void*,wxPropertyGrid_Create)(wxWindow* _prt,int _id, int _lft,int _top,int _wdt,int _hgt,int _stl)
{
        return (void*) new wxPropertyGrid (_prt, _id,wxPoint(_lft, _top),wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(bool,wxPropertyGrid_DisableProperty)(wxPropertyGrid* self, wxString const * propName)
{
        return self->DisableProperty(*propName);
}

/*-----------------------------------------------------------------------------
  wxPropertyGridEvent 
  Defined within eljevent.cpp
-----------------------------------------------------------------------------*/

/*-----------------------------------------------------------------------------
  PGProperty
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxString*,wxPGProperty_GetLabel)(wxPGProperty* self)
{
        wxString *result = new wxString();
        *result = self->GetLabel();
        return result;
}

EWXWEXPORT(wxString*,wxPGProperty_GetName)(wxPGProperty* self)
{
        wxString *result = new wxString();
        *result = self->GetName();
        return result;
}

EWXWEXPORT(wxString*,wxPGProperty_GetValueAsString)(wxPGProperty* self)
{
        wxString *result = new wxString();
        *result = self->GetValueAsString();
        return result;
}

EWXWEXPORT(wxString*,wxPGProperty_GetValueType)(wxPGProperty* self)
{
        wxString *result = new wxString();
        *result = self->GetValueType();
        return result;
}

EWXWEXPORT(void,wxPGProperty_SetHelpString)(wxPGProperty* self, wxString const* helpString)
{
        self->SetHelpString(*helpString);
}

/*-----------------------------------------------------------------------------
  wxStringProperty
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxStringProperty*,wxStringProperty_Create)(wxString const* label, wxString const* name, wxString const* value)
{
        return new wxStringProperty(*label, *name, *value);
}

/*-----------------------------------------------------------------------------
  wxIntProperty
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxIntProperty*,wxIntProperty_Create)(wxString const* label, wxString const* name, int value)
{
        return new wxIntProperty(*label, *name, value);
}

/*-----------------------------------------------------------------------------
  wxBoolProperty
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxBoolProperty*,wxBoolProperty_Create)(wxString const* label, wxString const* name, bool value)
{
        return new wxBoolProperty(*label, *name, value);
}

/*-----------------------------------------------------------------------------
  wxFloatProperty
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxFloatProperty*,wxFloatProperty_Create)(wxString const* label, wxString const* name, float value)
{
        return new wxFloatProperty(*label, *name, value);
}

/*-----------------------------------------------------------------------------
  wxDateProperty
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxDateProperty*,wxDateProperty_Create)(wxString const* label, wxString const* name, wxDateTime const* value)
{
        return new wxDateProperty(*label, *name, *value);
}

/*-----------------------------------------------------------------------------
  wxFileProperty
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxFileProperty*,wxFileProperty_Create)(wxString const* label, wxString const* name, wxString const* value)
{
        return new wxFileProperty(*label, *name, *value);
}

/*-----------------------------------------------------------------------------
  wxPropertyCategory
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxPropertyCategory*,wxPropertyCategory_Create)(wxString const* label)
{
        return new wxPropertyCategory(*label);
}

} /* extern "C" */
