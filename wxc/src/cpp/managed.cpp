#include "wrapper.h"

/* safety */
class wxManagedPtr;
#include "wxc_types.h"
#include "managed.h"


/*-----------------------------------------------------------------------------
   Managed pointers are pointed to by ForeignPtr's in Haskell.
   They are basically indirections with a finalization
   function.
-----------------------------------------------------------------------------*/
typedef void (_cdecl *Finalizer)(void* ptr);

class wxManagedPtr 
{
private:
  void*     ptr;
  Finalizer finalizer;

public:
  wxManagedPtr( void* p, Finalizer f ) 
  {
    ptr       = p;
    finalizer = f;
  };

  void* GetPtr() 
  {
    return ptr;
  }

  void NoFinalize()
  {
    finalizer = NULL;
  }

  void Finalize()
  {
    if (ptr!=NULL && finalizer!=NULL) {
      finalizer(ptr);
      finalizer = NULL;
    }
  }

  ~wxManagedPtr() 
  {
    Finalize();
  };
};

#if (wxVERSION_NUMBER > 2800)
class wxStockGDIExtra : wxStockGDI {
  public:
    static bool IsInStock(wxObject *obj)
    {
      for (int i = 0; i != ITEMCOUNT; ++i) {
        if (obj == ms_stockObject[i]) {
          return true;
        }
      }

      return false;
    }
};
#endif

extern "C" {
/*-----------------------------------------------------------------------------
  Operations
-----------------------------------------------------------------------------*/
EWXWEXPORT(void*,wxManagedPtr_GetPtr)(wxManagedPtr* self)
{
  if (self!=NULL) return self->GetPtr();
             else return NULL;
}

EWXWEXPORT(void,wxManagedPtr_NoFinalize)(wxManagedPtr* self)
{
  if (self!=NULL) {
    self->NoFinalize();
  }
}

EWXWEXPORT(void,wxManagedPtr_Finalize)(wxManagedPtr* self)
{
  if (self!=NULL) {
    self->Finalize();
  }
}

EWXWEXPORT(void,wxManagedPtr_Delete)(wxManagedPtr* self)
{
  if (self!=NULL) {
    delete self;
  }
}

static void _cdecl deleteManagedPtr(wxManagedPtr* mp)
{
  if (mp!=NULL) {
    delete mp;
  }
}

EWXWEXPORT(void*,wxManagedPtr_GetDeleteFunction)()
{
  return (void*)&deleteManagedPtr;  
}



/*-----------------------------------------------------------------------------
  Finalize wxObject
-----------------------------------------------------------------------------*/
static void _cdecl deleteObject( wxObject* obj )
{
  if (obj!=NULL) {
    delete obj;
  }
}

EWXWEXPORT(void,wxObject_SafeDelete)(wxObject* obj)
{
  deleteObject(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromObject)(wxObject* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteObject );
}

/*-----------------------------------------------------------------------------
  Finalize DateTime
-----------------------------------------------------------------------------*/
static void _cdecl deleteDateTime( wxDateTime* obj )
{
  if (obj!=NULL) {
    delete obj;
  }
}


EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromDateTime)(wxDateTime* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteDateTime );
}

/*-----------------------------------------------------------------------------
  Finalize wxGridCellCoordsArray
-----------------------------------------------------------------------------*/
static void _cdecl deleteGridCellCoordsArray( wxGridCellCoordsArray* obj )
{
  if (obj!=NULL) {
    delete obj;
  }
}


EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromGridCellCoordsArray)(wxGridCellCoordsArray* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteGridCellCoordsArray );
}


/*-----------------------------------------------------------------------------
  Finalize wxBitmap
-----------------------------------------------------------------------------*/
EWXWEXPORT(bool,wxBitmap_IsStatic)(wxBitmap* obj)
{
  static int calls=0; calls++;    /* prevent bug in VisualC 6.0 ? */
  return (obj==&wxNullBitmap || obj==NULL);
}

static void _cdecl deleteBitmap( wxBitmap* obj )
{
  if (!wxBitmap_IsStatic(obj)) {
    delete obj;
  }
}

EWXWEXPORT(void,wxBitmap_SafeDelete)(wxBitmap* obj)
{
  deleteBitmap(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromBitmap)(wxBitmap* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteBitmap );
}


/*-----------------------------------------------------------------------------
  Finalize wxIcon
-----------------------------------------------------------------------------*/
EWXWEXPORT(bool,wxIcon_IsStatic)(wxIcon* obj)
{
  static int calls=0; calls++;    /* prevent bug in VisualC 6.0 ? */
  return (obj==NULL || obj==&wxNullIcon);
}

static void _cdecl deleteIcon( wxIcon* obj )
{
  if (!wxIcon_IsStatic(obj)) {
    delete obj;
  }
}

EWXWEXPORT(void,wxIcon_SafeDelete)(wxIcon* obj)
{
  deleteIcon(obj);
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromIcon)(wxIcon* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteIcon );
}


/*-----------------------------------------------------------------------------
  Finalize wxBrush
-----------------------------------------------------------------------------*/
/* defined as macro to type without casts ... sigh */
/* use indirection array to let wx properly initialize the object pointers */

#define IsStatic(obj,statics) \
  { \
    int i; \
    if (obj==NULL) return true; \
    for( i = 0; statics[i] != NULL; i++ ) \
    { \
      if (*statics[i] == obj) return true; \
    }  \
    return false; \
  }


#if (wxVERSION_NUMBER < 2800)
static wxBrush* wxNULL_BRUSH = &wxNullBrush;

static wxBrush** staticsBrush[] =
    {&wxNULL_BRUSH
    ,&wxBLUE_BRUSH
    ,&wxGREEN_BRUSH
    ,&wxWHITE_BRUSH
    ,&wxBLACK_BRUSH
    ,&wxGREY_BRUSH
    ,&wxMEDIUM_GREY_BRUSH
    ,&wxLIGHT_GREY_BRUSH
    ,&wxTRANSPARENT_BRUSH
    ,&wxCYAN_BRUSH
    ,&wxRED_BRUSH
    ,NULL
    };

EWXWEXPORT(bool,wxBrush_IsStatic)(wxBrush* obj)
{
  IsStatic(obj,staticsBrush);  
}

#else

EWXWEXPORT(bool,wxBrush_IsStatic)(wxBrush* obj)
{
  return !obj || obj == &wxNullBrush || wxStockGDIExtra::IsInStock(obj);
}
#endif

static void _cdecl deleteBrush( wxBrush* obj )
{
  delete obj;
}

EWXWEXPORT(void,wxBrush_SafeDelete)(wxBrush* obj)
{
  if (!wxBrush_IsStatic(obj)) {
    deleteBrush(obj);
  }
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromBrush)(wxBrush* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteBrush );
}

/*-----------------------------------------------------------------------------
  Finalize wxColour
-----------------------------------------------------------------------------*/
#if (wxVERSION_NUMBER < 2800)
static wxColour* wxNULL_COLOUR = &wxNullColour;

static wxColour** staticsColour[] = 
    {&wxNULL_COLOUR
    ,&wxBLACK
    ,&wxWHITE
    ,&wxRED
    ,&wxBLUE
    ,&wxGREEN
    ,&wxCYAN
    ,&wxLIGHT_GREY
    ,NULL
    };

EWXWEXPORT(bool,wxColour_IsStatic)(wxColour* obj)
{
  IsStatic(obj,staticsColour);  
}

#else

EWXWEXPORT(bool,wxColour_IsStatic)(wxColour* obj)
{
  return !obj || obj == &wxNullColour || wxStockGDIExtra::IsInStock(obj);
}
#endif

static void _cdecl deleteColour( wxColour* obj )
{
  delete obj;
}

EWXWEXPORT(void,wxColour_SafeDelete)(wxColour* obj)
{
  if (!wxColour_IsStatic(obj)) {
    deleteColour(obj);
  }
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromColour)(wxColour* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteColour );
}


/*-----------------------------------------------------------------------------
  Finalize wxCursor
-----------------------------------------------------------------------------*/
#if (wxVERSION_NUMBER < 2800)
wxCursor*** staticsCursor(void)
{
static wxCursor* wxNULL_CURSOR = &wxNullCursor;

static wxCursor** staticsCursor[] = 
    {&wxNULL_CURSOR
    ,&wxSTANDARD_CURSOR
    ,&wxHOURGLASS_CURSOR
    ,&wxCROSS_CURSOR
    ,NULL
    };
  return staticsCursor;
}

EWXWEXPORT(bool,wxCursor_IsStatic)(wxCursor* obj)
{
  IsStatic(obj,staticsCursor());  
}
#else

EWXWEXPORT(bool,wxCursor_IsStatic)(wxCursor* obj)
{
  return !obj || obj == &wxNullCursor || wxStockGDIExtra::IsInStock(obj);
}
#endif

static void _cdecl deleteCursor( wxCursor* obj )
{
  delete obj;
}

EWXWEXPORT(void,wxCursor_SafeDelete)(wxCursor* obj)
{
  if (!wxCursor_IsStatic(obj)) {
    deleteCursor(obj);
  }
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromCursor)(wxCursor* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteCursor );
}


/*-----------------------------------------------------------------------------
  Finalize wxFont
-----------------------------------------------------------------------------*/

#if (wxVERSION_NUMBER < 2800)
static wxFont* wxNULL_FONT  = &wxNullFont;

static wxFont** staticsFont[] = 
    {&wxNULL_FONT
    ,&wxNORMAL_FONT
    ,&wxSMALL_FONT
    ,&wxITALIC_FONT
    ,&wxSWISS_FONT
    ,NULL
    };

EWXWEXPORT(bool,wxFont_IsStatic)(wxFont* obj)
{
  IsStatic(obj,staticsFont);  
}
#else

EWXWEXPORT(bool,wxFont_IsStatic)(wxFont* obj)
{
  return !obj || obj == &wxNullFont || wxStockGDIExtra::IsInStock(obj);
}
#endif

static void _cdecl deleteFont( wxFont* obj )
{
  delete obj;
}

EWXWEXPORT(void,wxFont_SafeDelete)(wxFont* obj)
{
  if (!wxFont_IsStatic(obj)) {
    deleteFont(obj);
  }
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromFont)(wxFont* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deleteFont );
}


/*-----------------------------------------------------------------------------
  Finalize wxPen
-----------------------------------------------------------------------------*/
#if (wxVERSION_NUMBER < 2800)
static wxPen* wxNULL_PEN = &wxNullPen;

static wxPen** staticsPen[] = 
    {&wxNULL_PEN
    ,&wxRED_PEN
    ,&wxCYAN_PEN
    ,&wxGREEN_PEN
    ,&wxBLACK_PEN
    ,&wxWHITE_PEN
    ,&wxTRANSPARENT_PEN
    ,&wxBLACK_DASHED_PEN
    ,&wxGREY_PEN
    ,&wxMEDIUM_GREY_PEN
    ,&wxLIGHT_GREY_PEN
    ,NULL
    };

EWXWEXPORT(bool,wxPen_IsStatic)(wxPen* obj)
{
  IsStatic(obj,staticsPen);  
}
#else

EWXWEXPORT(bool,wxPen_IsStatic)(wxPen* obj)
{
  return !obj || obj == &wxNullPen || wxStockGDIExtra::IsInStock(obj);
}
#endif

static void _cdecl deletePen( wxPen* obj )
{
  delete obj;
}

EWXWEXPORT(void,wxPen_SafeDelete)(wxPen* obj)
{
  if (!wxPen_IsStatic(obj)) {
    deletePen(obj);
  }
}

EWXWEXPORT(wxManagedPtr*,wxManagedPtr_CreateFromPen)(wxPen* ptr)
{
  return new wxManagedPtr( ptr, (Finalizer)&deletePen );
}

/* extern "C" */
}
