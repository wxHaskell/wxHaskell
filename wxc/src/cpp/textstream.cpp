#include "wxc/wrapper.h"
#include <wx/txtstrm.h>

extern "C"
{
EWXWEXPORT( bool, wxInputStream_CanRead)( wxInputStream* self )
{
  if (self)
    return self->CanRead();
  else
    return false;
}



EWXWEXPORT( wxTextInputStream*, wxTextInputStream_Create)( wxInputStream* inputStream, wxString* sep )
{
  return new wxTextInputStream( *inputStream, *sep );
}

EWXWEXPORT( void, wxTextInputStream_Delete)( wxTextInputStream* self )
{
  if (self) delete self;
}

EWXWEXPORT( wxString*, wxTextInputStream_ReadLine)( wxTextInputStream* self )
{
  if (!self)
    return new wxString(wxT(""));
  else
    return new wxString( self->ReadLine() );
}

EWXWEXPORT( wxTextOutputStream*, wxTextOutputStream_Create)( wxOutputStream* outputStream, wxEOL mode )
{
  return new wxTextOutputStream( *outputStream, mode );
}

EWXWEXPORT( void, wxTextOutputStream_Delete)( wxTextOutputStream* self )
{
  if (self) delete self;
}

EWXWEXPORT( void, wxTextOutputStream_WriteString)( wxTextOutputStream* self, wxString* txt )
{
  if (!self) return;
  self->WriteString( *txt );
}

}
