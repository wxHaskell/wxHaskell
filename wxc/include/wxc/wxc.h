#ifndef wxc_h
#define wxc_h

/* eiffel uses stdcall but we use __cdecl!! */
#ifdef _stdcall
# undef _stdcall
#endif

#define _stdcall

#define EXPORT


/*-----------------------------------------------------------------------------

  Standard includes

-----------------------------------------------------------------------------*/

#include "wxc_types.h"
#include "wxc_glue.h"





/*-----------------------------------------------------------------------------

  Modular extra exports

-----------------------------------------------------------------------------*/

#include "dragimage.h"
#include "graphicscontext.h"
#include "glcanvas.h"
#include "sound.h"
#include "managed.h"
#include "mediactrl.h"
#include "previewframe.h"
#include "printout.h"
#include "textstream.h"
#include "stream.h"
#include "hyperlinkctrl.h"
#include "pickerctrl.h"
#include "stc.h"

/*-----------------------------------------------------------------------------
  Extra exports
-----------------------------------------------------------------------------*/

/* wxClosure */
TClassDefExtend(wxClosure,wxObject)
TClass(wxClosure)  wxClosure_Create( TClosureFun _fun_CEvent, void* _data );
void*              wxClosure_GetData( TSelf(wxClosure) _obj );

TClass(wxClosure)  wxEvtHandler_GetClosure( TSelf(wxEvtHandler) _obj, int id, int type );

/** Get the client data in the form of a closure. Use 'closureGetData' to get to the actual data.*/
TClass(wxClosure)  wxEvtHandler_GetClientClosure( TSelf(wxEvtHandler) _obj );
/** Set the client data as a closure. The closure data contains the data while the function is called on deletion. */
void               wxEvtHandler_SetClientClosure( TSelf(wxEvtHandler) _obj, TClass(wxClosure) closure );

/** Get the reference data of an object as a closure: only works if properly initialized. Use 'closureGetData' to get to the actual data. */
TClass(wxClosure)  wxObject_GetClientClosure( TSelf(wxObject) _obj );
/** Set the reference data of an object as a closure. The closure data contains the data while the function is called on deletion. Returns 'True' on success. Only works if the reference data is unused by wxWidgets! */
void               wxObject_SetClientClosure( TSelf(wxObject) _obj, TClass(wxClosure) closure );


/* extra class definitions for classInfo */

TClassDefExtend(wxGauge95,wxGauge)

TClassDefExtend(wxGaugeMSW,wxGauge)

TClassDefExtend(wxSlider95,wxSlider)

TClassDefExtend(wxSliderMSW,wxSlider)





/* Object */

void wxObject_Delete( TSelf(wxObject) obj );


/* Frame */
TClass(wxString) wxFrame_GetTitle( TSelf(wxFrame) _obj );
void        wxFrame_SetTitle( TSelf(wxFrame) _frame, TClass(wxString) _txt );
TBool       wxFrame_SetShape( TSelf(wxFrame) self, TClass(wxRegion) region);

TBool       wxFrame_ShowFullScreen( TSelf(wxFrame) self, TBool show, int style);

TBool       wxFrame_IsFullScreen( TSelf(wxFrame) self );

void        wxFrame_Centre( TSelf(wxFrame) self, int orientation );


/* Create/Delete */
void   wxCursor_Delete( TSelf(wxCursor) _obj );
void  wxDateTime_Delete(TSelf(wxDateTime) _obj);

/* wxMouseEvent */
int   wxMouseEvent_GetWheelDelta( TSelf(wxMouseEvent) _obj );
int   wxMouseEvent_GetWheelRotation( TSelf(wxMouseEvent) _obj );
int   wxMouseEvent_GetButton( TSelf(wxMouseEvent) _obj );


TClass(wxPoint) wxcGetMousePosition( );





/* wxDC */

double wxDC_GetUserScaleX( TSelf(wxDC) dc );

double wxDC_GetUserScaleY( TSelf(wxDC) dc );


/* wxWindow */
TClass(wxPoint) wxWindow_ConvertDialogToPixelsEx( TSelf(wxWindow) _obj );

TClass(wxPoint) wxWindow_ConvertPixelsToDialogEx( TSelf(wxWindow) _obj );

TClass(wxPoint) wxWindow_ScreenToClient2( TSelf(wxWindow) _obj, TPoint(x,y) );



/* wxString helpers */

TClass(wxString) wxString_Create( TString buffer );

TClass(wxString) wxString_CreateLen( TString buffer, int len );

void             wxString_Delete( TSelf(wxString) s );

TStringLen       wxString_GetString( TSelf(wxString) s, TStringOut buffer );

size_t           wxString_Length( TSelf(wxString) s );



/* menu */
TClass(wxMenuBar) wxMenu_GetMenuBar( TSelf(wxMenu) _obj );

TClass(wxFrame)   wxMenuBar_GetFrame( TSelf(wxMenuBar) _obj );



int wxListEvent_GetCacheFrom( TSelf(wxListEvent) _obj);

int wxListEvent_GetCacheTo( TSelf(wxListEvent) _obj);



void wxListCtrl_AssignImageList( TSelf(wxListCtrl) _obj, TClass(wxImageList) images, int which );

void wxListCtrl_GetColumn2( TSelf(wxListCtrl) _obj, int col, TClassRef(wxListItem) item);

void wxListCtrl_GetItem2( TSelf(wxListCtrl) _obj, TClassRef(wxListItem) info);

TClass(wxPoint) wxListCtrl_GetItemPosition2( TSelf(wxListCtrl) _obj, int item );

/** Sort items in a list control. Takes a closure that is called with a 'CommandEvent' where the @Int@ is the item data of the first item and the @ExtraLong@ the item data of the second item. The event handler should set the @Int@ to 0 when the items are equal, -1 when the first is less, and 1 when the second is less. */

TBool wxListCtrl_SortItems2(TSelf(wxListCtrl) _obj, TClass(wxClosure) closure );



/* tree ctrl */

TClassDefExtend(wxcTreeItemData,wxTreeItemData)



/** Create tree item data with a closure. The closure data contains the data while the function is called on deletion. */

TClass(wxcTreeItemData) wxcTreeItemData_Create( TClass(wxClosure) closure );

/** Get the client data in the form of a closure. Use 'closureGetData' to get to the actual data.*/

TClass(wxClosure) wxcTreeItemData_GetClientClosure( TSelf(wxcTreeItemData) self );

/** Set the tree item data with a closure. The closure data contains the data while the function is called on deletion. */

void  wxcTreeItemData_SetClientClosure( TSelf(wxcTreeItemData) self, TClass(wxClosure) closure );



TClass(wxTreeItemId) wxTreeItemId_Clone( TSelf(wxTreeItemId) _obj);

TClass(wxTreeItemId) wxTreeItemId_CreateFromValue(TIntPtr value);

TIntPtr wxTreeItemId_GetValue( TSelf(wxTreeItemId) _obj);





TClass(wxKeyEvent) wxTreeEvent_GetKeyEvent( TSelf(wxTreeEvent) _obj);

int    wxTreeEvent_IsEditCancelled( TSelf(wxTreeEvent) _obj);

void   wxTreeEvent_Allow( TSelf(wxTreeEvent) _obj);



TClass(wxTreeCtrl) wxTreeCtrl_Create2( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl );

void   wxTreeCtrl_InsertItem2( TSelf(wxTreeCtrl) _obj, TClass(wxWindow) parent, TClass(wxTreeItemId) idPrevious, TClass(wxString) text, int image, int selectedImage, TClass(wxClosure) closure, TClassRef(wxTreeItemId) _item );

void   wxTreeCtrl_InsertItemByIndex2( TSelf(wxTreeCtrl) _obj, TClass(wxWindow) parent, int index, TClass(wxString) text, int image, int selectedImage, TClass(wxClosure) closure, TClassRef(wxTreeItemId) _item );

TClass(wxClosure)  wxTreeCtrl_GetItemClientClosure( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item );

void   wxTreeCtrl_SetItemClientClosure( TSelf(wxTreeCtrl) _obj, TClass(wxTreeItemId) item, TClass(wxClosure) closure );

void   wxTreeCtrl_AssignImageList(TSelf(wxTreeCtrl) _obj, TClass(wxImageList) imageList );

void   wxTreeCtrl_AssignStateImageList(TSelf(wxTreeCtrl) _obj, TClass(wxImageList) imageList );





/* dc */

/** Get the color of pixel. Note: this is not a portable method at the moment and its use is discouraged. */

void wxDC_GetPixel2( TSelf(wxDC) _obj, TPoint(x,y), TClassRef(wxColour) col);



/* scrolledwindow */
void wxScrolledWindow_SetScrollRate( TSelf(wxScrolledWindow) _obj, int xstep, int ystep );


/* wxObject */
TClassDef(wxObject)
TClass(wxClassInfo)  wxObject_GetClassInfo( TSelf(wxObject) _obj );
TBool       wxObject_IsKindOf( TSelf(wxObject) _obj, TClass(wxClassInfo) classInfo );
TBool       wxObject_IsScrolledWindow( TSelf(wxObject) _obj );


/* wxClassInfo */
TClassDef(wxClassInfo)
TClass(wxClassInfo)  wxClassInfo_FindClass( TClass(wxString) _txt );
TClass(wxString) wxClassInfo_GetBaseClassName1( TSelf(wxClassInfo) _obj );
TClass(wxString) wxClassInfo_GetBaseClassName2( TSelf(wxClassInfo) _obj );
TClass(wxString) wxClassInfo_GetClassNameEx( TSelf(wxClassInfo) _obj );
int         wxClassInfo_GetSize( TSelf(wxClassInfo) _obj );
TBool       wxClassInfo_IsKindOfEx( TSelf(wxClassInfo) _obj, TClass(wxClassInfo) classInfo );

/* wxNotebook */
void        wxNotebook_AssignImageList( TSelf(wxNotebook) _obj, TClass(wxImageList) imageList );

/* Timers */
TClassDefExtend(wxTimerEx,wxTimer)
void               wxTimerEx_Connect( TSelf(wxTimerEx) _obj, TClass(wxClosure) closure );
TClass(wxTimerEx)  wxTimerEx_Create(  );
TClass(wxClosure)  wxTimerEx_GetClosure( TSelf(wxTimerEx) _obj );

/* Menu */

void  wxMenu_AppendRadioItem( TSelf(wxMenu) self, int id, TClass(wxString) text, TClass(wxString) help);





/* Menu Item */
TClass(wxMenuItem)  wxMenuItem_CreateSeparator();
TClass(wxMenuItem)  wxMenuItem_CreateEx(int id, TClass(wxString) label, TClass(wxString) help, int itemkind, TClass(wxMenu) submenu);


/* Toolbar */

void wxToolBar_AddTool2( TSelf(wxToolBar) _obj, int toolId, TClass(wxString) label, TClass(wxBitmap) bmp, TClass(wxBitmap) bmpDisabled, int itemKind, TClass(wxString) shortHelp, TClass(wxString) longHelp );


/* Progress dialog */
TClass(wxProgressDialog) wxProgressDialog_Create( TClass(wxString) title, TClass(wxString) message, int max, TClass(wxWindow) parent, int style );
TBool wxProgressDialog_Update(TSelf(wxProgressDialog) obj, int value );
TBool wxProgressDialog_UpdateWithMessage( TSelf(wxProgressDialog) obj, int value, TClass(wxString) message );
void wxProgressDialog_Resume( TSelf(wxProgressDialog) obj );

/** Get the version number of wxWidgets as a number composed of the major version times 1000, minor version times 100, and the release number. For example, release 2.1.15 becomes 2115. */
int wxVersionNumber();
/** Check if a preprocessor macro is defined. For example, @wxIsDefined("__WXGTK__")@ or @wxIsDefined("wxUSE_GIF")@. */
TBoolInt wxIsDefined( TString s );

/* Input sink */
TClassDefExtend(wxInputSink,wxThread)

/** Create an event driven input stream. It is unsafe to reference the original inputStream after this call! The last parameter @bufferLen@ gives the default input batch size. The sink is automatically destroyed whenever the input stream has no more input. */
TClass(wxInputSink) wxInputSink_Create( TClass(wxInputStream) input, TClass(wxEvtHandler) evtHandler, int bufferLen );
/** After creation, retrieve the @id@ of the sink to connect to @wxEVT_INPUT_SINK@ events. */
int   wxInputSink_GetId( TSelf(wxInputSink) obj );
/** After event connection, start non-blocking reading of the inputstream. This will generate @inputSinkEvent@ events. */
void  wxInputSink_Start( TSelf(wxInputSink) obj );

/* Input sink events */
TClassDefExtend(wxInputSinkEvent,wxEvent)

/** Get the input status (@wxSTREAM_NO_ERROR@ is ok). */
int wxInputSinkEvent_LastError( TSelf(wxInputSinkEvent) obj );
/** The number of characters in the input buffer. */
int wxInputSinkEvent_LastRead( TSelf(wxInputSinkEvent) obj );
/** The input buffer. */
char* wxInputSinkEvent_LastInput( TSelf(wxInputSinkEvent) obj );


/* HTML events */
TClassDefExtend(wxcHtmlEvent,wxCommandEvent)

TClass(wxMouseEvent) wxcHtmlEvent_GetMouseEvent( TSelf(wxcHtmlEvent) self );
TClass(wxHtmlCell)   wxcHtmlEvent_GetHtmlCell( TSelf(wxcHtmlEvent) self );

/** Return the /id/ attribute of the associated html cell (if applicable) */

TClass(wxString)     wxcHtmlEvent_GetHtmlCellId( TSelf(wxcHtmlEvent) self );

/** Return the /href/ attribute of the associated html anchor (if applicable) */
TClass(wxString)     wxcHtmlEvent_GetHref( TSelf(wxcHtmlEvent) self );
TClass(wxString)     wxcHtmlEvent_GetTarget( TSelf(wxcHtmlEvent) self );
TClass(wxPoint)      wxcHtmlEvent_GetLogicalPosition( TSelf(wxcHtmlEvent) self );

/* HTML window */
TClassDefExtend(wxcHtmlWindow,wxHtmlWindow)
TClass(wxcHtmlWindow) wxcHtmlWindow_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl, TClass(wxString) _txt );

TClass(wxHtmlWindow) wxHtmlWindow_Create( TClass(wxWindow) _prt, int _id, TRect(_lft,_top,_wdt,_hgt), int _stl, TClass(wxString) _txt );
TBool                wxHtmlWindow_AppendToPage( TSelf(wxHtmlWindow) _obj, TClass(wxString) source );
TClass(wxHtmlContainerCell) wxHtmlWindow_GetInternalRepresentation( TSelf(wxHtmlWindow) _obj );
TClass(wxString)     wxHtmlWindow_GetOpenedAnchor( TSelf(wxHtmlWindow) _obj ) ;
TClass(wxString)     wxHtmlWindow_GetOpenedPage( TSelf(wxHtmlWindow) _obj );
TClass(wxString)     wxHtmlWindow_GetOpenedPageTitle( TSelf(wxHtmlWindow) _obj );
TClass(wxFrame)      wxHtmlWindow_GetRelatedFrame( TSelf(wxHtmlWindow) _obj );
TBool                wxHtmlWindow_HistoryBack( TSelf(wxHtmlWindow) _obj);
TBool                wxHtmlWindow_HistoryCanBack( TSelf(wxHtmlWindow) _obj );
TBool                wxHtmlWindow_HistoryCanForward( TSelf(wxHtmlWindow) _obj );
void                 wxHtmlWindow_HistoryClear( TSelf(wxHtmlWindow) _obj);
TBool                wxHtmlWindow_HistoryForward( TSelf(wxHtmlWindow) _obj );
TBool                wxHtmlWindow_LoadPage( TSelf(wxHtmlWindow) _obj, TClass(wxString) location );
void                 wxHtmlWindow_ReadCustomization( TSelf(wxHtmlWindow) _obj, TClass(wxConfigBase) cfg, TClass(wxString) path);
void                 wxHtmlWindow_SetBorders( TSelf(wxHtmlWindow) _obj, int b );
void                 wxHtmlWindow_SetFonts( TSelf(wxHtmlWindow) _obj, TClass(wxString) normal_face, TClass(wxString) fixed_face, int * sizes );
void                 wxHtmlWindow_SetPage( TSelf(wxHtmlWindow) _obj, TClass(wxString) source );
void                 wxHtmlWindow_SetRelatedFrame( TSelf(wxHtmlWindow) _obj , TClass(wxFrame) frame, TClass(wxString) format );
void                 wxHtmlWindow_SetRelatedStatusBar( TSelf(wxHtmlWindow) _obj, int bar);
void                 wxHtmlWindow_WriteCustomization( TSelf(wxHtmlWindow) _obj, TClass(wxConfigBase) cfg, TClass(wxString) path );

/* wxGridCellTextEnterEditor */

TClassDefExtend(wxGridCellTextEnterEditor,wxGridCellTextEditor)

TClass(wxGridCellTextEnterEditor) wxGridCellTextEnterEditor_Ctor();



/* logger */
TClass(wxLogStderr)   wxLogStderr_Create();
TClass(wxLogStderr)   wxLogStderr_CreateStdOut();
TClass(wxLogNull)     wxLogNull_Create();
TClass(wxLogTextCtrl) wxLogTextCtrl_Create( TClass(wxTextCtrl) text );
TClass(wxLogWindow)   wxLogWindow_Create( TClass(wxWindow) parent, TString title, TBool showit, TBool passthrough );
TClass(wxFrame)       wxLogWindow_GetFrame( TSelf(wxLogWindow) obj );

void   LogError(TClass(wxString) _msg);
void   LogFatalError(TClass(wxString) _msg);
void   LogWarning(TClass(wxString) _msg);
void   LogMessage(TClass(wxString) _msg);
void   LogVerbose(TClass(wxString) _msg);
void   LogStatus(TClass(wxString) _msg);
void   LogSysError(TClass(wxString) _msg);
void   LogDebug(TClass(wxString) _msg);
void   LogTrace(TClass(wxString) mask, TClass(wxString) _msg);

void       wxLog_AddTraceMask( TSelf(wxLog) _obj, TClass(wxString) str );
void       wxLog_Delete( TSelf(wxLog) _obj );
void       wxLog_DontCreateOnDemand( TSelf(wxLog) _obj );
void       wxLog_Flush( TSelf(wxLog) _obj );
void       wxLog_FlushActive( TSelf(wxLog) _obj );
TClass(wxLog)  wxLog_GetActiveTarget(  );
char*      wxLog_GetTimestamp( TSelf(wxLog) _obj );
#if !wxCHECK_VERSION(3, 1, 0)
int        wxLog_GetTraceMask( TSelf(wxLog) _obj );
#endif
int        wxLog_GetVerbose( TSelf(wxLog) _obj );
TBool      wxLog_HasPendingMessages( TSelf(wxLog) _obj );
TBool      wxLog_IsAllowedTraceMask( TSelf(wxLog) _obj, TClass(wxMask) mask );
void       wxLog_OnLog( TSelf(wxLog) _obj, int level, TStringVoid szString, int t );
void       wxLog_RemoveTraceMask( TSelf(wxLog) _obj, TClass(wxString) str );
void       wxLog_Resume( TSelf(wxLog) _obj );
TClass(wxLog)  wxLog_SetActiveTarget( TSelf(wxLog) pLogger );
void       wxLog_SetTimestamp( TSelf(wxLog) _obj, TStringVoid ts );
// Obsolete
// void       wxLog_SetTraceMask( TSelf(wxLog) _obj, int ulMask );
void       wxLog_SetVerbose( TSelf(wxLog) _obj, TBoolInt bVerbose );
void       wxLog_Suspend( TSelf(wxLog) _obj );


/* process */
TClass(wxProcess) wxProcess_Open( TClass(wxString) cmd, int flags );
TBool      wxProcess_IsErrorAvailable( TSelf(wxProcess) _obj );
TBool      wxProcess_IsInputAvailable( TSelf(wxProcess) _obj );
TBool      wxProcess_IsInputOpened( TSelf(wxProcess) _obj );
int        wxKill( int pid, int signal );

void       wxStreamBase_Delete( TSelf(wxStreamBase) obj);

/* Dialogs */
void        wxGetColourFromUser(TClass(wxWindow) parent, TClass(wxColour) colInit, TClassRef(wxColour) colour );
void        wxGetFontFromUser(TClass(wxWindow) parent, TClass(wxFont) fontInit, TClassRef(wxFont) font );
TStringLen  wxGetPasswordFromUser(TString message, TString caption, TString defaultText, TClass(wxWindow) parent, TStringOut _buf );
TStringLen  wxGetTextFromUser(TString message, TString caption, TString defaultText, TClass(wxWindow) parent, TPoint(x,y), TBool center, TStringOut _buf );
long        wxGetNumberFromUser( TClass(wxString) message, TClass(wxString) prompt, TClass(wxString) caption, long value, long min, long max, TClass(wxWindow) parent, TPoint(x,y) );
void        wxcBell();
void        wxcBeginBusyCursor();
void        wxcEndBusyCursor();
void        wxcIsBusy();

/* text ctrl */
TBool               wxTextCtrl_EmulateKeyPress( TSelf(wxTextCtrl) _obj, TClass(wxKeyEvent) keyevent);
TClass (wxTextAttr) wxTextCtrl_GetDefaultStyle( TSelf(wxTextCtrl) _obj );
TClass(wxString)    wxTextCtrl_GetRange( TSelf(wxTextCtrl) _obj, long from, long to );
TClass(wxString)    wxTextCtrl_GetStringSelection( TSelf(wxTextCtrl) _obj );
TBool               wxTextCtrl_IsMultiLine( TSelf(wxTextCtrl) _obj );
TBool               wxTextCtrl_IsSingleLine( TSelf(wxTextCtrl) _obj );
TBool               wxTextCtrl_SetDefaultStyle( TSelf(wxTextCtrl) _obj, TClass(wxTextAttr) style);
void                wxTextCtrl_SetMaxLength( TSelf(wxTextCtrl) _obj, long len );
TBool               wxTextCtrl_SetStyle( TSelf(wxTextCtrl) _obj, long start, long end, TClass(wxTextAttr) style );

/* text attributes */
TClass(wxTextAttr)  wxTextAttr_Create(TClass(wxColour) colText, TClass(wxColour) colBack, TClass(wxFont) font);
TClass(wxTextAttr)  wxTextAttr_CreateDefault();
void      wxTextAttr_Delete( TSelf(wxTextAttr) _obj );
void      wxTextAttr_GetBackgroundColour( TSelf(wxTextAttr) _obj, TClassRef(wxColour) colour  );
void      wxTextAttr_GetFont( TSelf(wxTextAttr) _obj, TClassRef(wxFont) font );
void      wxTextAttr_GetTextColour( TSelf(wxTextAttr) _obj, TClassRef(wxColour) colour );
TBool     wxTextAttr_HasBackgroundColour( TSelf(wxTextAttr) _obj );
TBool     wxTextAttr_HasFont( TSelf(wxTextAttr) _obj );
TBool     wxTextAttr_HasTextColour( TSelf(wxTextAttr) _obj );
TBool     wxTextAttr_IsDefault( TSelf(wxTextAttr) _obj );
void      wxTextAttr_SetTextColour(TSelf(wxTextAttr) _obj, TClass(wxColour) colour );
void      wxTextAttr_SetBackgroundColour(TSelf(wxTextAttr) _obj, TClass(wxColour) colour );
void      wxTextAttr_SetFont(TSelf(wxTextAttr) _obj, TClass(wxFont) font );


/* ConfigBase */

TClassDefExtend(wxFileConfig,wxConfigBase)



TClass(wxConfigBase) wxConfigBase_Get();

void                 wxConfigBase_Set( TClass(wxConfigBase) self );

TClass(wxFileConfig) wxFileConfig_Create( TClass(wxInputStream) inp );



/* Image.cpp */

TClass(wxBitmap) wxBitmap_CreateFromImage( TClass(wxImage) image, int depth );

TClass(wxImage) wxImage_CreateFromDataEx( TSize(width,height), void* data, TBoolInt isStaticData);

void wxImage_Delete( TSelf(wxImage) image );



/** Create from rgb int. */

TClass(wxColour) wxColour_CreateFromInt(int rgb);

/** Return colors as an rgb int. */

int wxColour_GetInt( TSelf(wxColour) colour);

/** Create from rgba unsigned int. */

TClass(wxColour) wxColour_CreateFromUnsignedInt(TUInt rgba);

/** Return colors as an rgba unsigned int. */

TUInt wxColour_GetUnsignedInt( TSelf(wxColour) colour);



/** Create from system colour. */

TClass(wxColour) wxcSystemSettingsGetColour( int systemColour );





/* basic pixel manipulation */

void wxcSetPixelRGB( TUInt8* buffer, int width, TPoint(x,y), int rgb  );

int  wxcGetPixelRGB( TUInt8* buffer, int width, TPoint(x,y) );

void wxcSetPixelRowRGB( TUInt8* buffer, int width, TPoint(x,y), int rgbStart, int rgbEnd, int count );

void wxcInitPixelsRGB( TUInt8* buffer, TSize(width,height), int rgba );

void wxcSetPixelRGBA( TUInt8* buffer, int width, TPoint(x,y), TUInt rgba  );

TUInt  wxcGetPixelRGBA( TUInt8* buffer, int width, TPoint(x,y) );

void wxcSetPixelRowRGBA( TUInt8* buffer, int width, TPoint(x,y), int rgbaStart, int rgbEnd, TUInt count );

void wxcInitPixelsRGBA( TUInt8* buffer, TSize(width,height), TUInt rgba );



/* malloc/free */

void* wxcMalloc(int size );

void  wxcFree( void* p );



/* wakeup idle */

void wxcWakeUpIdle();



/* application directory */

/** Return the directory of the application. On unix systems (except MacOS X), it is not always possible to determine this correctly. Therefore, the APPDIR environment variable is returned first if it is defined. */

TClass(wxString) wxGetApplicationDir();

/** Return the full path of the application. On unix systems (except MacOS X), it is not always possible to determine this correctly. */

TClass(wxString) wxGetApplicationPath();



/* ELJApp */
void  ELJApp_InitializeC( TClass(wxClosure) closure, int _argc, TChar** _argv );

int   ELJApp_GetIdleInterval();

void  ELJApp_SetIdleInterval( int interval );




#endif /* wxc_h */
