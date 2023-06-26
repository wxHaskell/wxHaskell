/*-----------------------------------------------------------------------------
  GLCanvas
-----------------------------------------------------------------------------*/
TClassDefExtend(wxGLCanvas,wxWindow);
TClassDefExtend(wxGLContext,wxObject);

TClass(wxGLCanvas)   wxGLCanvas_Create( TClass(wxWindow) parent, int windowID, int* attributes, TRect(x,y,w,h), int _stl, TClass(wxString) title, TClass(wxPalette) palette );
TBool   wxGLCanvas_SetColour( TSelf(wxGLCanvas) self, TClass(wxColour) colour );
TBool   wxGLCanvas_SetCurrent( TSelf(wxGLCanvas) self, TClass(wxGLContext) ctxt );
TBool   wxGLCanvas_SwapBuffers( TSelf(wxGLCanvas) self );  
TBool   wxGLCanvas_IsDisplaySupported( int* attributes );
TBool   wxGLCanvas_IsExtensionSupported( TClass(wxString) extension );

/*-----------------------------------------------------------------------------
  GLCanvas
-----------------------------------------------------------------------------*/

TClass(wxGLContext)   wxGLContext_Create( TClass(wxGLCanvas) win, TClass(wxGLContext) other );
TClass(wxGLContext)   wxGLContext_CreateFromNull( TClass(wxGLCanvas) win );
TBool   wxGLContext_SetCurrent( TSelf(wxGLContext) self, TClass(wxGLCanvas) win );
