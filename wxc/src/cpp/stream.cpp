#include "wrapper.h"
#include <wx/wfstream.h>

extern "C"
{

/*-----------------------------------------------------------------------------
  Memory Input stream
-----------------------------------------------------------------------------*/

EWXWEXPORT( wxMemoryInputStream*, wxMemoryInputStream_Create)(void* data,int len)
{
  return new wxMemoryInputStream((unsigned char*) data,len);
}

EWXWEXPORT( void, wxMemoryInputStream_Delete)(wxMemoryInputStream* self)
{
  if (self) delete self;
}

/*-----------------------------------------------------------------------------
  File Input stream
-----------------------------------------------------------------------------*/

EWXWEXPORT( wxFileInputStream*, wxFileInputStream_Create)(wxString* ofileName)
{
  return new wxFileInputStream(*ofileName);
}

EWXWEXPORT( void, wxFileInputStream_Delete)(wxFileInputStream* self)
{
  if (self) delete self;
}

EWXWEXPORT( bool, wxFileInputStream_IsOk)(wxFileInputStream* self)
{
  return self->IsOk();
}

/*-----------------------------------------------------------------------------
  File Output stream
-----------------------------------------------------------------------------*/

EWXWEXPORT( wxFileOutputStream*, wxFileOutputStream_Create)(wxString* ofileName)
{
  return new wxFileOutputStream(*ofileName);
}

EWXWEXPORT( void, wxFileOutputStream_Delete)(wxFileOutputStream* self)
{
  if (self) delete self;
}

EWXWEXPORT( bool, wxFileOutputStream_IsOk)(wxFileOutputStream* self)
{
  return self->IsOk();
}

}