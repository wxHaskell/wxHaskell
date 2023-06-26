#include "wxc/wrapper.h"
#include "wx/wx.h"
#include "wx/print.h"
#include "wx/printdlg.h"


/*-----------------------------------------------------------------------------
  Special wxPrintout class that sends events.
-----------------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------
  new event types
-----------------------------------------------------------------------------*/
BEGIN_DECLARE_EVENT_TYPES()
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_PRINT_BEGIN, 2000)
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_PRINT_END, 2001 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_PRINT_BEGIN_DOC, 1002 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_PRINT_END_DOC, 1003 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_PRINT_PREPARE, 1004 )
    DECLARE_LOCAL_EVENT_TYPE(wxEVT_PRINT_PAGE, 1005 )
END_DECLARE_EVENT_TYPES()


DEFINE_LOCAL_EVENT_TYPE( wxEVT_PRINT_BEGIN )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_PRINT_END )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_PRINT_BEGIN_DOC )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_PRINT_END_DOC )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_PRINT_PREPARE )
DEFINE_LOCAL_EVENT_TYPE( wxEVT_PRINT_PAGE )

extern "C" {

EWXWEXPORT(int,expEVT_PRINT_BEGIN)()
{
  return (int)wxEVT_PRINT_BEGIN;
}

EWXWEXPORT(int,expEVT_PRINT_BEGIN_DOC)()
{
  return (int)wxEVT_PRINT_BEGIN_DOC;
}

EWXWEXPORT(int,expEVT_PRINT_END)()
{
  return (int)wxEVT_PRINT_END;
}

EWXWEXPORT(int,expEVT_PRINT_END_DOC)()
{
  return (int)wxEVT_PRINT_END_DOC;
}

EWXWEXPORT(int,expEVT_PRINT_PREPARE)()
{
  return (int)wxEVT_PRINT_PREPARE;
}

EWXWEXPORT(int,expEVT_PRINT_PAGE)()
{
  return (int)wxEVT_PRINT_PAGE;
}

}


/*-----------------------------------------------------------------------------
  Printout and events
-----------------------------------------------------------------------------*/

class wxcPrintout : public wxPrintout
{
private:
  DECLARE_DYNAMIC_CLASS(wxcPrintout)

protected:
  int   m_startPage;
  int   m_endPage;
  int   m_fromPage;
  int   m_toPage;
  wxEvtHandler* m_evtHandler;

public:
  wxcPrintout() : wxPrintout() {};
  wxcPrintout( const wxString& title );
  ~wxcPrintout();

  void SetPageLimits( int startPage, int endPage, int fromPage, int toPage );   
  wxEvtHandler* GetEvtHandler();

  /* virtual members */
  void GetPageInfo( int* startPage, int* endPage, int* fromPage, int* toPage );
  bool OnBeginDocument( int startPage, int endPage );
  void OnEndDocument();
  void OnBeginPrinting();
  void OnEndPrinting();
  void OnPreparePrinting();
  bool OnPrintPage( int page );
  bool HasPage( int page );
};

IMPLEMENT_DYNAMIC_CLASS(wxcPrintout, wxPrintout)


class wxcPrintEvent : public wxEvent
{
private:
    DECLARE_DYNAMIC_CLASS(wxcPrintEvent)
private:
    wxcPrintout* m_printOut;
    int         m_page;
    int         m_lastPage;
    bool        m_continue;

public:
    wxcPrintEvent() : wxEvent() {};
    wxcPrintEvent( const wxcPrintEvent& printEvent ); // copy constructor
    wxcPrintEvent( wxEventType evtType, int id, wxcPrintout* printOut, int page, int lastPage );
    wxEvent* Clone() const          { return new wxcPrintEvent(*this); }

    wxcPrintout* GetPrintout();
    int         GetPage();
    int         GetEndPage();
    bool        GetContinue();
    void        SetContinue( bool cont );
    void        SetPageLimits( int startPage, int endPage, int fromPage, int toPage );   
};

IMPLEMENT_DYNAMIC_CLASS(wxcPrintEvent, wxEvent)


/*-----------------------------------------------------------------------------
  Print events
-----------------------------------------------------------------------------*/
wxcPrintEvent::wxcPrintEvent( wxEventType evtType, int id, wxcPrintout* printOut, int page, int lastPage )
: wxEvent( id, evtType )
{
  m_printOut = printOut;
  m_page     = page;
  m_lastPage = lastPage;
  m_continue = true;
}

wxcPrintEvent::wxcPrintEvent( const wxcPrintEvent& printEvent ) : wxEvent( printEvent )
{
  m_printOut = printEvent.m_printOut;
  m_page     = printEvent.m_page;
  m_lastPage = printEvent.m_lastPage;
  m_continue = printEvent.m_continue;
}

wxcPrintout* wxcPrintEvent::GetPrintout()
{
  return m_printOut;
}

int wxcPrintEvent::GetPage()
{
  return m_page;
}

int wxcPrintEvent::GetEndPage()
{
  return m_lastPage;
}

bool wxcPrintEvent::GetContinue()
{
  return m_continue;
}

void wxcPrintEvent::SetContinue( bool cont )
{
  m_continue = cont;
}

void wxcPrintEvent::SetPageLimits( int startPage, int endPage, int fromPage, int toPage )
{
  if (m_printOut) {
    m_printOut->SetPageLimits( startPage, endPage, fromPage, toPage );
  }
}


/*-----------------------------------------------------------------------------
  Printout 
-----------------------------------------------------------------------------*/
wxcPrintout::wxcPrintout( const wxString& title ) : wxPrintout( title )
{
  m_startPage = 1;
  m_endPage   = 32000;
  m_fromPage  = 1;
  m_toPage    = 1;
  m_evtHandler = new wxEvtHandler();
}

wxcPrintout::~wxcPrintout()
{
  if (m_evtHandler) delete m_evtHandler;
}

wxEvtHandler* wxcPrintout::GetEvtHandler()
{
  return m_evtHandler;
}

void wxcPrintout::SetPageLimits( int startPage, int endPage, int fromPage, int toPage )
{
  m_startPage = startPage;
  m_endPage   = endPage;
  m_fromPage  = fromPage;
  m_toPage    = toPage;  
}

void wxcPrintout::GetPageInfo( int* startPage, int* endPage, int* fromPage, int* toPage )
{
  if (startPage) *startPage = m_startPage;
  if (endPage)   *endPage   = m_endPage;
  if (fromPage)  *fromPage  = m_fromPage;
  if (toPage)    *toPage    = m_toPage;
}

bool wxcPrintout::OnBeginDocument( int startPage, int endPage )
{
  bool cont = wxPrintout::OnBeginDocument( startPage, endPage );
  if (cont) {
    wxcPrintEvent printEvent( wxEVT_PRINT_BEGIN_DOC, 0, this, startPage, endPage );
    m_evtHandler->ProcessEvent( printEvent );
    cont = printEvent.GetContinue();
  }
  return cont;
}

void wxcPrintout::OnEndDocument()
{
  wxcPrintEvent printEvent( wxEVT_PRINT_END_DOC, 0, this, 0, 0 );
  m_evtHandler->ProcessEvent(printEvent);
  wxPrintout::OnEndDocument();
}

void wxcPrintout::OnBeginPrinting()
{
  wxcPrintEvent printEvent( wxEVT_PRINT_BEGIN, 0, this, 0, 0 );
  wxPrintout::OnBeginPrinting();
  m_evtHandler->ProcessEvent(printEvent);
}

void wxcPrintout::OnEndPrinting()
{
  wxcPrintEvent printEvent( wxEVT_PRINT_END, 0, this, 0, 0 );
  m_evtHandler->ProcessEvent(printEvent);
  wxPrintout::OnEndPrinting();
}

void wxcPrintout::OnPreparePrinting()
{
  wxcPrintEvent printEvent( wxEVT_PRINT_PREPARE, 0, this, 0, 0 );
  wxPrintout::OnPreparePrinting(); 
  m_evtHandler->ProcessEvent(printEvent);  
}

bool wxcPrintout::OnPrintPage( int page )
{
  wxcPrintEvent printEvent( wxEVT_PRINT_PAGE, 0, this, page, page );
  m_evtHandler->ProcessEvent(printEvent);
  return printEvent.GetContinue();  
}

bool wxcPrintout::HasPage( int page )
{
  return (page >= m_fromPage && page <= m_toPage);
}


/*-----------------------------------------------------------------------------
  Wrappers
-----------------------------------------------------------------------------*/
extern "C" 
{

EWXWEXPORT(wxPrintDialogData*,wxPrintDialog_GetPrintDialogData)(wxPrintDialog* _obj)
{
  return &(_obj->GetPrintDialogData());
}

  
EWXWEXPORT(wxcPrintout*,wxcPrintout_Create)(wxString* title)
{
  return new wxcPrintout( *title );
}

EWXWEXPORT(void,wxcPrintout_Delete)(wxcPrintout* self)
{
  if (self) delete self;
}

EWXWEXPORT(void,wxcPrintout_SetPageLimits)(wxcPrintout* self,int startPage,int endPage,int fromPage,int toPage)
{
  self->SetPageLimits( startPage, endPage, fromPage, toPage );
}

EWXWEXPORT(wxEvtHandler*,wxcPrintout_GetEvtHandler)(wxcPrintout* self)
{
  return self->GetEvtHandler();
}


EWXWEXPORT(wxcPrintout*,wxcPrintEvent_GetPrintout)(wxcPrintEvent* self)
{
  return self->GetPrintout();
}

EWXWEXPORT(int,wxcPrintEvent_GetPage)(wxcPrintEvent* self)
{
  return self->GetPage();
} 

EWXWEXPORT(int,wxcPrintEvent_GetEndPage)(wxcPrintEvent* self)
{
  return self->GetEndPage();
} 

EWXWEXPORT(bool,wxcPrintEvent_GetContinue)(wxcPrintEvent* self)
{
  return self->GetContinue();
} 
    
EWXWEXPORT(void,wxcPrintEvent_SetContinue)(wxcPrintEvent* self,bool cont)
{
  self->SetContinue(cont);
} 

EWXWEXPORT(void,wxcPrintEvent_SetPageLimits)(wxcPrintEvent* self,int startPage,int endPage,int fromPage,int toPage)
{
  self->SetPageLimits(startPage, endPage, fromPage, toPage );
}


/*-----------------------------------------------------------------------------
  Printout
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxString*,wxPrintout_GetTitle)(void* _obj)
{
	wxString title = ((wxPrintout*)_obj)->GetTitle();
	return new wxString(title);
}
	
EWXWEXPORT(void*,wxPrintout_GetDC)(void* _obj)
{
	return (void*)((wxPrintout*)_obj)->GetDC();
}
	
EWXWEXPORT(void,wxPrintout_SetDC)(void* _obj,void* dc)
{
	((wxPrintout*)_obj)->SetDC((wxDC*)dc);
}
	
EWXWEXPORT(void,wxPrintout_SetPageSizePixels)(void* _obj,int w,int h)
{
	((wxPrintout*)_obj)->SetPageSizePixels(w, h);
}
	
EWXWEXPORT(void,wxPrintout_GetPageSizePixels)(void* _obj,int* w,int* h)
{
	((wxPrintout*)_obj)->GetPageSizePixels(w,h);
}
	
EWXWEXPORT(void,wxPrintout_SetPageSizeMM)(void* _obj,int w,int h)
{
	((wxPrintout*)_obj)->SetPageSizeMM(w, h);
}
	
EWXWEXPORT(void,wxPrintout_GetPageSizeMM)(void* _obj,int* w,int* h)
{
	((wxPrintout*)_obj)->GetPageSizeMM(w,h);
}
	
EWXWEXPORT(void,wxPrintout_SetPPIScreen)(void* _obj,int x,int y)
{
	((wxPrintout*)_obj)->SetPPIScreen(x, y);
}
	
EWXWEXPORT(void,wxPrintout_GetPPIScreen)(void* _obj,int* x,int* y)
{
	((wxPrintout*)_obj)->GetPPIScreen(x,y);
}
	
EWXWEXPORT(void,wxPrintout_SetPPIPrinter)(void* _obj,int x,int y)
{
	((wxPrintout*)_obj)->SetPPIPrinter(x, y);
}
	
EWXWEXPORT(void,wxPrintout_GetPPIPrinter)(void* _obj,int* x,int* y)
{
	((wxPrintout*)_obj)->GetPPIPrinter(x,y);
}
	
EWXWEXPORT(bool,wxPrintout_IsPreview)(wxPrintout* _obj)
{
	return _obj->IsPreview();
}
	    
}
