/*-----------------------------------------------------------------------------
  ManagedPtr
-----------------------------------------------------------------------------*/
TClassDef(wxManagedPtr)

#if defined (__WXMAC__) && defined (EXPORT)
#undef EXPORT
#define EXPORT extern "C"
#endif

void* wxManagedPtr_GetPtr( TSelf(wxManagedPtr) self );
void  wxManagedPtr_NoFinalize( TSelf(wxManagedPtr) self );
void  wxManagedPtr_Finalize( TSelf(wxManagedPtr) self );
void  wxManagedPtr_Delete( TSelf(wxManagedPtr) self );
EXPORT void* wxManagedPtr_GetDeleteFunction( );

/*-----------------------------------------------------------------------------
  Creators
-----------------------------------------------------------------------------*/
TClass(wxManagedPtr) wxManagedPtr_CreateFromObject(TClass(wxObject) obj);
TClass(wxManagedPtr) wxManagedPtr_CreateFromDateTime(TClass(wxDateTime) obj);
TClass(wxManagedPtr) wxManagedPtr_CreateFromGridCellCoordsArray(TClass(wxGridCellCoordsArray) obj);

TClass(wxManagedPtr) wxManagedPtr_CreateFromBitmap(TClass(wxBitmap) obj);
TClass(wxManagedPtr) wxManagedPtr_CreateFromIcon(TClass(wxIcon) obj);

TClass(wxManagedPtr) wxManagedPtr_CreateFromBrush(TClass(wxBrush) obj);
TClass(wxManagedPtr) wxManagedPtr_CreateFromColour(TClass(wxColour) obj);
TClass(wxManagedPtr) wxManagedPtr_CreateFromCursor(TClass(wxCursor) obj);
TClass(wxManagedPtr) wxManagedPtr_CreateFromFont(TClass(wxFont) obj);
TClass(wxManagedPtr) wxManagedPtr_CreateFromPen(TClass(wxPen) obj);

/*-----------------------------------------------------------------------------
  Safe deletion
-----------------------------------------------------------------------------*/
void wxObject_SafeDelete( TSelf(Object) self );

void wxBitmap_SafeDelete( TSelf(Bitmap) self );
void wxIcon_SafeDelete( TSelf(Icon) self );

void wxBrush_SafeDelete( TSelf(Brush) self );
void wxColour_SafeDelete( TSelf(Colour) self );
void wxCursor_SafeDelete( TSelf(Cursor) self );
void wxFont_SafeDelete( TSelf(Font) self );
void wxPen_SafeDelete( TSelf(Pen) self );

/*-----------------------------------------------------------------------------
  Is an object static (i.e. do not delete it)
-----------------------------------------------------------------------------*/
TBool wxBitmap_IsStatic( TSelf(Bitmap) self );
TBool wxIcon_IsStatic( TSelf(Icon) self );

TBool wxBrush_IsStatic( TSelf(Brush) self );
TBool wxColour_IsStatic( TSelf(Colour) self );
TBool wxCursor_IsStatic( TSelf(Cursor) self );
TBool wxFont_IsStatic( TSelf(Font) self );
TBool wxPen_IsStatic( TSelf(Pen) self );

