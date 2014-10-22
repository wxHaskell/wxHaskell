#include "wrapper.h"
#include "wx/glcanvas.h"

/*-----------------------------------------------------------------------------
  We want to include the function signatures always -- even on 
  systems that don't support openGL. This means that every function body is
  surrounded by #ifdef wxUSE_GLCANVAS directives :-(
-----------------------------------------------------------------------------*/
#if defined(wxUSE_GLCANVAS) && (wxUSE_GLCANVAS==0)
# undef wxUSE_GLCANVAS
#endif

#if defined(wxcREFUSE_OPENGL)
# undef wxUSE_GLCANVAS
#endif

#ifndef wxUSE_GLCANVAS
# define wxGLCanvas      void
# define wxGLContext     void
#endif

extern "C" {

/*-----------------------------------------------------------------------------
  GLCanvas
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxGLCanvas*,wxGLCanvas_Create)(wxWindow* parent,
					  int windowID,
					  int* attributes,
					  int x, int y, int w, int h, // TRect
					  int style,
					  wxString* title, 
					  wxPalette* palette )  
{
#ifdef wxUSE_GLCANVAS 
  return new wxGLCanvas( parent, windowID, attributes, wxPoint(x,y), wxSize(w,h), style,
			(title ? *title : wxString(wxT("GLCanvas"))), 
			(palette ? *palette : wxNullPalette));
#else
  return NULL;
#endif
}

  EWXWEXPORT(bool,wxGLCanvas_SetCurrent)(wxGLCanvas* self, wxGLContext* ctxt)  
{
#ifdef wxUSE_GLCANVAS 
  if (ctxt == NULL)
    return false;
  else
    return self->SetCurrent(*ctxt);
#else
  return false; 
#endif
}

EWXWEXPORT(bool,wxGLCanvas_SetColour)(wxGLCanvas* self, wxColour* colour)  
{
#ifdef wxUSE_GLCANVAS 
  wxString name = wxTheColourDatabase->FindName( *colour );
  if (name.IsEmpty()) 
    return self->SetColour(wxString(wxT("WHITE")));
  else
    return self->SetColour(name);
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxGLCanvas_SwapBuffers)(wxGLCanvas* self)  
{
#ifdef wxUSE_GLCANVAS 
  return self->SwapBuffers();
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxGLCanvas_IsDisplaySupported)(int* attributes)
{
#ifdef wxUSE_GLCANVAS
  return wxGLCanvas::IsDisplaySupported(attributes);
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxGLCanvas_IsExtensionSupported(wxString* extension))
{
#ifdef wxUSE_GLCANVAS
  const char* str = (const char*) extension->c_str();
  if (str != NULL) {
    bool retval = wxGLCanvas::IsExtensionSupported(str);
    delete str;
    return retval;
  }
  else
    return false;
#else
  return false;
#endif
}

EWXWEXPORT(wxGLContext*,wxGLContext_Create(wxGLCanvas* win, wxGLContext* other))
{
#ifdef wxUSE_GLCANVAS
  return new wxGLContext(win, other);
#endif
}

EWXWEXPORT(wxGLContext*,wxGLContext_CreateFromNull(wxGLCanvas* win))
{
#ifdef wxUSE_GLCANVAS
  return new wxGLContext(win);
#endif
}

EWXWEXPORT(bool,wxGLContext_SetCurrent(wxGLContext* self, wxGLCanvas* win))
{
#ifdef wxUSE_GLCANVAS
  if (win == NULL)
    return false;
  else
    return self->SetCurrent(*win);
#else
  return false;
#endif
}

}


