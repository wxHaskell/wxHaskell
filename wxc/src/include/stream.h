#ifndef FILESTEAM_H
#define FILESTEAM_H

/*-----------------------------------------------------------------------------
  Memory Input stream
-----------------------------------------------------------------------------*/

TClass(wxMemoryInputStream) wxMemoryInputStream_Create(void* data,int len);
void wxMemoryInputStream_Delete( TSelf(wxMemoryInputStream) self );

/*-----------------------------------------------------------------------------
  File Input stream
-----------------------------------------------------------------------------*/

TClass(wxFileInputStream) wxFileInputStream_Create(TClass(wxString) ofileName);
void wxFileInputStream_Delete( TSelf(wxFileInputStream) self );
TBool wxFileInputStream_IsOk( TSelf(wxFileInputStream) self);

/*-----------------------------------------------------------------------------
  File Output stream
-----------------------------------------------------------------------------*/

TClass(wxFileOutputStream) wxFileOutputStream_Create(TClass(wxString) ofileName);
void wxFileOutputStream_Delete( TSelf(wxFileOutputStream) self );
TBool wxFileOutputStream_IsOk( TSelf(wxFileOutputStream) self);

#endif /* FILESTEAM_H */