

  
/*-----------------------------------------------------------------------------
  Input stream
-----------------------------------------------------------------------------*/
TBool wxInputStream_CanRead( TSelf(wxInputStream) self );

TClassDef( wxTextInputStream );
TClass(wxTextInputStream) wxTextInputStream_Create( TClass(wxInputStream) inputStream, TClass(wxString) sep );
void wxTextInputStream_Delete( TSelf(wxTextInputStream) self );
TClass(wxString) wxTextInputStream_ReadLine( TSelf(wxTextInputStream) self );


/*-----------------------------------------------------------------------------
  Output stream
-----------------------------------------------------------------------------*/
TClassDef( wxTextOutputStream );
TClass(wxTextOutputStream) wxTextOutputStream_Create( TClass(wxOutputStream) outputStream, int mode );
void wxTextOutputStream_Delete( TSelf(wxTextOutputStream) self );
void wxTextOutputStream_WriteString( TSelf(wxTextOutputStream) self, TClass(wxString) txt );
