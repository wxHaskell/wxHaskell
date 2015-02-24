TClassDefExtend(wxGCDC,wxDC);
TClassDefExtend(wxGraphicsObject,wxObject);
TClassDefExtend(wxGraphicsBrush,wxGraphicsObject);
TClassDefExtend(wxGraphicsContext,wxGraphicsObject);
TClassDefExtend(wxGraphicsFont,wxGraphicsObject);
TClassDefExtend(wxGraphicsMatrix,wxGraphicsObject);
TClassDefExtend(wxGraphicsPath,wxGraphicsObject);
TClassDefExtend(wxGraphicsPen,wxGraphicsObject);
TClassDefExtend(wxGraphicsRenderer,wxGraphicsObject);


/*-----------------------------------------------------------------------------
  GCDC
-----------------------------------------------------------------------------*/

TClass(wxGCDC)  wxGcdc_Create(TClass(wxWindowDC) dc);
TClass(wxGCDC)  wxGcdc_CreateFromMemory(TClass(wxMemoryDC) dc);
TClass(wxGCDC)  wxGcdc_CreateFromPrinter(TClass(wxPrinterDC) dc);
TClass(wxGraphicsContext)  wxGcdc_GetGraphicsContext(TSelf(wxGCDC) self);
void wxGcdc_SetGraphicsContext(TClass(wxGCDC) self, TClass(wxGraphicsContext) gc);
void wxGcdc_Delete(TClass(wxGCDC) self);

/*-----------------------------------------------------------------------------
  GraphicsBrush
-----------------------------------------------------------------------------*/
TClass(wxGraphicsBrush)  wxGraphicsBrush_Create( );
void  wxGraphicsBrush_Delete(TSelf(wxGraphicsBrush) self);

/*-----------------------------------------------------------------------------
  GraphicsContext
-----------------------------------------------------------------------------*/
TClass(wxGraphicsContext)  wxGraphicsContext_Create( TClass(wxWindowDC) dc );
TClass(wxGraphicsContext)  wxGraphicsContext_CreateFromMemory( TClass(wxMemoryDC) dc );
TClass(wxGraphicsContext)  wxGraphicsContext_CreateFromPrinter( TClass(wxPrinterDC) dc );
TClass(wxGraphicsContext)  wxGraphicsContext_CreateFromWindow( TClass(wxWindow) window );
void  wxGraphicsContext_Delete(TSelf(wxGraphicsContext) self);
TClass(wxGraphicsContext)  wxGraphicsContext_CreateFromNative( void* context );
TClass(wxGraphicsContext)  wxGraphicsContext_CreateFromNativeWindow( void* window );
TClass(wxGraphicsPath)     wxGraphicsContext_CreatePath( TSelf(wxGraphicsContext) self);
TClass(wxGraphicsMatrix)   wxGraphicsContext_CreateMatrix( TSelf(wxGraphicsContext) self, double a, double b, double c, double d, double tx , double ty );
TClass(wxGraphicsMatrix)   wxGraphicsContext_CreateDefaultMatrix( TSelf(wxGraphicsContext) self);
void  wxGraphicsContext_Clip( TSelf(wxGraphicsContext) self, TClass(wxRegion) region );
void  wxGraphicsContext_ClipByRectangle( TSelf(wxGraphicsContext) self, TRectDouble(x,y,w,h) );
void  wxGraphicsContext_ResetClip( TSelf(wxGraphicsContext) self );
void  wxGraphicsContext_DrawBitmap( TSelf(wxGraphicsContext) self, TClass(wxBitmap) bmp, TRectDouble(x,y,w,h) );
void  wxGraphicsContext_DrawEllipse( TSelf(wxGraphicsContext) self, TRectDouble(x,y,w,h) );
void  wxGraphicsContext_DrawIcon( TSelf(wxGraphicsContext) self, TClass(wxIcon) icon, TRectDouble(x,y,w,h) );
void  wxGraphicsContext_DrawLines( TSelf(wxGraphicsContext) self, size_t n, void* x, void* y, int style );
void  wxGraphicsContext_DrawPath( TSelf(wxGraphicsContext) self, TClass(wxGraphicsPath) path, int style );
void  wxGraphicsContext_DrawRectangle( TSelf(wxGraphicsContext) self, TRectDouble(x,y,w,h) );
void  wxGraphicsContext_DrawRoundedRectangle( TSelf(wxGraphicsContext) self, TRectDouble(x,y,w,h), double radius );
void  wxGraphicsContext_DrawText( TSelf(wxGraphicsContext) self, TClass(wxString) text, TPointDouble(x,y) );
void  wxGraphicsContext_DrawTextWithAngle( TSelf(wxGraphicsContext) self, TClass(wxString) text, TPointDouble(x,y), double radius );
void  wxGraphicsContext_FillPath( TSelf(wxGraphicsContext) self, TClass(wxGraphicsPath) path, int style );
void  wxGraphicsContext_StrokePath( TSelf(wxGraphicsContext) self, TClass(wxGraphicsPath) path );
void*  wxGraphicsContext_GetNativeContext( TSelf(wxGraphicsContext) self );
void  wxGraphicsContext_GetTextExtent( TSelf(wxGraphicsContext) self, TClass(wxString) text, double* width, double* height, double* descent, double* externalLeading );
void  wxGraphicsContext_Rotate( TSelf(wxGraphicsContext) self, double angle );
void  wxGraphicsContext_Scale( TSelf(wxGraphicsContext) self, TSizeDouble(xScale,yScale) );
void  wxGraphicsContext_Translate( TSelf(wxGraphicsContext) self, double dx, double dy );
void  wxGraphicsContext_SetTransform( TSelf(wxGraphicsContext) self, TClass(wxGraphicsMatrix) path );
void  wxGraphicsContext_ConcatTransform( TSelf(wxGraphicsContext) self, TClass(wxGraphicsMatrix) path );
void  wxGraphicsContext_SetBrush( TSelf(wxGraphicsContext) self, TClass(wxBrush) brush );
void  wxGraphicsContext_SetGraphicsBrush( TSelf(wxGraphicsContext) self, TClass(wxGraphicsBrush) brush );
void  wxGraphicsContext_SetFont( TSelf(wxGraphicsContext) self, TClass(wxFont) font, TClass(wxColour) colour );
void  wxGraphicsContext_SetGraphicsFont( TSelf(wxGraphicsContext) self, TClass(wxGraphicsFont) font );
void  wxGraphicsContext_SetPen( TSelf(wxGraphicsContext) self, TClass(wxPen) pen );
void  wxGraphicsContext_SetGraphicsPen( TSelf(wxGraphicsContext) self, TClass(wxGraphicsPen) pen );
void  wxGraphicsContext_StrokeLine( TSelf(wxGraphicsContext) self, TPointDouble(x1,y1), TPointDouble(x2,y2) );
void  wxGraphicsContext_StrokeLines( TSelf(wxGraphicsContext) self, size_t n, void* x, void* y, int style );
void  wxGraphicsContext_PushState( TSelf(wxGraphicsContext) self );
void  wxGraphicsContext_PopState( TSelf(wxGraphicsContext) self );

/*-----------------------------------------------------------------------------
  GraphicsFont
-----------------------------------------------------------------------------*/
TClass(wxGraphicsFont)  wxGraphicsFont_Create( );
void  wxGraphicsFont_Delete(TSelf(wxGraphicsFont) self);

/*-----------------------------------------------------------------------------
  GraphicsMatrix
-----------------------------------------------------------------------------*/
TClass(wxGraphicsMatrix)  wxGraphicsMatrix_Create( );
void  wxGraphicsMatrix_Delete(TSelf(wxGraphicsMatrix) self);
void  wxGraphicsMatrix_Concat( TSelf(wxGraphicsMatrix) self, TClass(wxGraphicsMatrix) t );
void  wxGraphicsMatrix_Get( TSelf(wxGraphicsMatrix) self, double* a, double* b, double* c, double* d, double* tx, double* ty );
void*  wxGraphicsMatrix_GetNativeMatrix( TSelf(wxGraphicsMatrix) self );
void  wxGraphicsMatrix_Invert( TSelf(wxGraphicsMatrix) self );
TBool  wxGraphicsMatrix_IsEqual( TSelf(wxGraphicsMatrix) self, TClass(wxGraphicsMatrix) t );
TBool  wxGraphicsMatrix_IsIdentity( TSelf(wxGraphicsMatrix) self );
void  wxGraphicsMatrix_Rotate( TSelf(wxGraphicsMatrix) self, double angle );
void  wxGraphicsMatrix_Scale( TSelf(wxGraphicsMatrix) self, TSizeDouble(xScale,yScale) );
void  wxGraphicsMatrix_Set( TSelf(wxGraphicsMatrix) self, double a, double b, double c, double d, double tx, double ty );
void  wxGraphicsMatrix_Translate( TSelf(wxGraphicsMatrix) self, double dx, double dy );
void  wxGraphicsMatrix_TransformPoint( TSelf(wxGraphicsMatrix) self, TPointOutDouble(x,y) );
void  wxGraphicsMatrix_TransformDistance( TSelf(wxGraphicsMatrix) self, double* dx, double* dy );

/*-----------------------------------------------------------------------------
  GraphicsObject
-----------------------------------------------------------------------------*/
TClass(wxGraphicsRenderer)  wxGraphicsObject_GetRenderer( );
TBool  wxGraphicsObject_IsNull(TSelf(wxGraphicsObject) self);

/*-----------------------------------------------------------------------------
  GraphicsPath
-----------------------------------------------------------------------------*/
void  wxGraphicsPath_Delete(TSelf(wxGraphicsPath) self);
void  wxGraphicsPath_MoveToPoint(TSelf(wxGraphicsPath) self, TPointDouble(x,y));
void  wxGraphicsPath_AddArc(TSelf(wxGraphicsPath) self, TPointDouble(x,y), double r, double startAngle, double endAngle, TBool clockwise );
void  wxGraphicsPath_AddArcToPoint(TSelf(wxGraphicsPath) self, TPointDouble(x1,y1), TPointDouble(x2,y2), double r );
void  wxGraphicsPath_AddCircle(TSelf(wxGraphicsPath) self, TPointDouble(x,y), double r );
void  wxGraphicsPath_AddCurveToPoint(TSelf(wxGraphicsPath) self, TPointDouble(cx1,cy1), TPointDouble(cx2,cy2), TPointDouble(x,y) );
void  wxGraphicsPath_AddEllipse(TSelf(wxGraphicsPath) self, TRectDouble(x,y,w,h) );
void  wxGraphicsPath_AddLineToPoint(TSelf(wxGraphicsPath) self, TPointDouble(x,y) );
void  wxGraphicsPath_AddPath(TSelf(wxGraphicsPath) self, TPointDouble(x,y), TClass(wxGraphicsPath) path );
void  wxGraphicsPath_AddQuadCurveToPoint(TSelf(wxGraphicsPath) self, TPointDouble(cx,cy), TPointDouble(x,y) );
void  wxGraphicsPath_AddRectangle(TSelf(wxGraphicsPath) self, TRectDouble(x,y,w,h) );
void  wxGraphicsPath_AddRoundedRectangle(TSelf(wxGraphicsPath) self, TRectDouble(x,y,w,h), double radius );
void  wxGraphicsPath_CloseSubpath(TSelf(wxGraphicsPath) self );
void  wxGraphicsPath_Contains(TSelf(wxGraphicsPath) self, TPointDouble(x,y), int style);
void  wxGraphicsPath_GetBox(TSelf(wxGraphicsPath) self, TRectOutDouble(x,y,w,h));
void  wxGraphicsPath_GetCurrentPoint(TSelf(wxGraphicsPath) self, TPointOutDouble(x,y));
void  wxGraphicsPath_Transform( TSelf(wxGraphicsPath) self, TClass(wxGraphicsMatrix) matrix );
void*  wxGraphicsPath_GetNativePath( TSelf(wxGraphicsPath) self );
void  wxGraphicsPath_UnGetNativePath( void* p );

/*-----------------------------------------------------------------------------
  GraphicsPen
-----------------------------------------------------------------------------*/
TClass(wxGraphicsPen)  wxGraphicsPen_Create( );
void  wxGraphicsPen_Delete(TSelf(wxGraphicsPen) self);

/*-----------------------------------------------------------------------------
  GraphicsRenderer
-----------------------------------------------------------------------------*/
void  wxGraphicsRenderer_Delete(TSelf(wxGraphicsRenderer) self);
TClass(wxGraphicsRenderer)  wxGraphicsRenderer_GetDefaultRenderer(TSelf(wxGraphicsRenderer) self);
TClass(wxGraphicsContext)  wxGraphicsRenderer_CreateContext( TClass(wxWindowDC) dc );
TClass(wxGraphicsContext)  wxGraphicsRenderer_CreateContextFromWindow( TClass(wxWindow) window );
TClass(wxGraphicsContext)  wxGraphicsRenderer_CreateContextFromNativeContext( void* context );
TClass(wxGraphicsContext)  wxGraphicsRenderer_CreateContextFromNativeWindow( void* window );
TClass(wxGraphicsPath)  wxGraphicsRenderer_CreatePath( TSelf(wxGraphicsRenderer) self );
