/*-----------------------------------------------------------------------------
  PreviewFrame
-----------------------------------------------------------------------------*/
TClassDefExtend(wxPreviewFrame,wxFrame);

/** Usage: @previewFrameCreate printPreview parent title rect name @. */
TClass(wxPreviewFrame) wxPreviewFrame_Create( TClass(wxPrintPreview) preview, TClass(wxFrame) parent, TClass(wxString) title, TRect(x,y,width,height), int _stl, TClass(wxString) name  );
void wxPreviewFrame_Delete( TSelf(wxPreviewFrame) self );
/** Usage: @previewFrameInitialize self@, call this before showing the frame. */
void wxPreviewFrame_Initialize( TSelf(wxPreviewFrame) self );

/*-----------------------------------------------------------------------------
  PreviewControlBar
-----------------------------------------------------------------------------*/
TClassDefExtend(wxPreviewControlBar,wxPanel);
