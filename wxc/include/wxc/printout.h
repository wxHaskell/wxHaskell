
/*-----------------------------------------------------------------------------
  Printout events
-----------------------------------------------------------------------------*/
int expEVT_PRINT_BEGIN();
int expEVT_PRINT_BEGIN_DOC();
int expEVT_PRINT_END();
int expEVT_PRINT_END_DOC();
int expEVT_PRINT_PREPARE();
int expEVT_PRINT_PAGE();

/*-----------------------------------------------------------------------------
  Printout 
-----------------------------------------------------------------------------*/
TClass(wxDC) wxPrintout_GetDC( TSelf(wxPrintout) _obj );
void       wxPrintout_GetPPIPrinter( TSelf(wxPrintout) _obj, TPointOutVoid(_x,_y) );
void       wxPrintout_GetPPIScreen( TSelf(wxPrintout) _obj, TPointOutVoid(_x,_y) );
void       wxPrintout_GetPageSizeMM( TSelf(wxPrintout) _obj, TSizeOutVoid(_w,_h) );
void       wxPrintout_GetPageSizePixels( TSelf(wxPrintout) _obj, TSizeOutVoid(_w,_h) );
TClass(wxString) wxPrintout_GetTitle( TSelf(wxPrintout) _obj );
TBool      wxPrintout_IsPreview( TSelf(wxPrintout) _obj );
void       wxPrintout_SetDC( TSelf(wxPrintout) _obj, TClass(wxDC) dc );
void       wxPrintout_SetPPIPrinter( TSelf(wxPrintout) _obj, TPoint(x,y) );
void       wxPrintout_SetPPIScreen( TSelf(wxPrintout) _obj, TPoint(x,y) );
void       wxPrintout_SetPageSizeMM( TSelf(wxPrintout) _obj, TSize(w,h) );
void       wxPrintout_SetPageSizePixels( TSelf(wxPrintout) _obj, TSize(w,h) );



/*-----------------------------------------------------------------------------
  WXCPrintout 
-----------------------------------------------------------------------------*/
TClassDefExtend( wxcPrintout, wxPrintout );
TClassDefExtend( wxcPrintEvent, wxEvent );
TClassDefExtend( wxcPrintoutHandler, wxEvtHandler );
  
TClass(wxcPrintout) wxcPrintout_Create( TClass(wxString) title );
void wxcPrintout_Delete( TSelf(wxcPrintout) self );
void wxcPrintout_SetPageLimits( TSelf(wxcPrintout) self, int startPage, int endPage, int fromPage, int toPage );
/** Usage: @wxcPrintoutGetEvtHandler self@. Do not delete the associated event handler! */
TClass(wxcPrintoutHandler) wxcPrintout_GetEvtHandler( TSelf(wxcPrintout) self );

/** Usage: @wxcPrintEventGetPrintout self@. Do not delete the associated printout! */
TClass(wxcPrintout) wxcPrintEvent_GetPrintout( TSelf(wxcPrintEvent) self );
int   wxcPrintEvent_GetPage( TSelf(wxcPrintEvent) self );
int   wxcPrintEvent_GetEndPage( TSelf(wxcPrintEvent) self );
TBool wxcPrintEvent_GetContinue( TSelf(wxcPrintEvent) self );
void  wxcPrintEvent_SetContinue( TSelf(wxcPrintEvent) self, TBool cont );
void  wxcPrintEvent_SetPageLimits( TSelf(wxcPrintEvent) self, int startPage, int endPage, int fromPage, int toPage );

