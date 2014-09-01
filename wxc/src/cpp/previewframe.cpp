#include "wrapper.h"
#include "wx/wx.h"
#include "wx/print.h"

/*-----------------------------------------------------------------------------

-----------------------------------------------------------------------------*/
extern "C" {

/*-----------------------------------------------------------------------------
  PreviewFrame
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxPreviewFrame*, wxPreviewFrame_Create)( wxPrintPreview* preview
                                                  , wxFrame* parent
                                                  , wxString* title
                                                  , int x, int y
                                                  , int w, int h
                                                  , int _stl
                                                  , wxString* name
                                                  )
{
  return new wxPreviewFrame( preview, parent, *title, wxPoint(x,y), wxSize(w,h), _stl, *name );
}

EWXWEXPORT(void, wxPreviewFrame_Delete)( wxPreviewFrame* self )
{
  if (self) delete self;
}

EWXWEXPORT(void, wxPreviewFrame_Initialize)( wxPreviewFrame* self )
{
  self->Initialize();
}


}
