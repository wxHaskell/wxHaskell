#include "wrapper.h"

#if !defined(__WXGTK__)
# include <wx/dcprint.h>
#endif

#if defined(wxUSE_POSTSCRIPT) && (wxUSE_POSTSCRIPT==0)
# undef wxUSE_POSTSCRIPT
#endif

#ifdef wxUSE_POSTSCRIPT
# include <wx/dcps.h>
# include "wx/generic/prntdlgg.h"
#endif

#ifndef wxUSE_POSTSCRIPT
# define wxPostScriptDC        void
#endif

extern "C"
{

EWXWEXPORT(void*,wxPrinter_Create)(void* data)
{
	return (void*)new wxPrinter((wxPrintDialogData*)data);
}

EWXWEXPORT(void,wxPrinter_Delete)(wxPrinter* self)
{
	delete self;
}

EWXWEXPORT(void*,wxPrinter_CreateAbortWindow)(wxPrinter* self,wxWindow* parent,wxPrintout* printout)
{
	return (void*)self->CreateAbortWindow(parent, printout);
}
	
EWXWEXPORT(void,wxPrinter_ReportError)(wxPrinter* self,wxWindow* parent,wxPrintout* printout,wxString* message)
{
	self->ReportError(parent, printout,*message);
}
	
EWXWEXPORT(void,wxPrinter_GetPrintDialogData)(wxPrinter* self,wxPrintDialogData* _ref)
{
	*_ref = self->GetPrintDialogData();
}
	
EWXWEXPORT(bool,wxPrinter_GetAbort)(wxPrinter* self)
{
	return self->GetAbort();
}
	
EWXWEXPORT(int,wxPrinter_GetLastError)(wxPrinter* self)
{
	return self->GetLastError();
}
	
EWXWEXPORT(bool,wxPrinter_Setup)(wxPrinter* self,wxWindow* parent)
{
	return self->Setup(parent);
}
	
EWXWEXPORT(bool,wxPrinter_Print)(wxPrinter* self,wxWindow* parent,wxPrintout* printout,bool prompt)
{
	return self->Print(parent, printout, prompt);
}
	
EWXWEXPORT(void*,wxPrinter_PrintDialog)(wxPrinter* self,wxWindow* parent)
{
	return (void*)self->PrintDialog(parent);
}
	
EWXWEXPORT(void*,ELJPrintout_Create)(void* title,void* self,void* _DoOnBeginDocument,void* _DoOnEndDocument,void* _DoOnBeginPrinting,void* _DoOnEndPrinting,void* _DoOnPreparePrinting,void* _DoOnPrintPage,void* _DoOnHasPage,void* DoOnPageInfo)
{
	return (void*)new ELJPrintout( title, self, _DoOnBeginDocument, _DoOnEndDocument, _DoOnBeginPrinting, _DoOnEndPrinting, _DoOnPreparePrinting, _DoOnPrintPage, _DoOnHasPage, DoOnPageInfo);
}
EWXWEXPORT(void,ELJPrintout_Delete)(ELJPrintout* self)
{
	delete self;
}

EWXWEXPORT(wxString*,ELJPrintout_GetTitle)(ELJPrintout* self)
{
	return new wxString(self->GetTitle());
}
	
EWXWEXPORT(void*,ELJPrintout_GetDC)(ELJPrintout* self)
{
	return (void*)self->GetDC();
}
	
EWXWEXPORT(void,ELJPrintout_SetDC)(ELJPrintout* self,wxDC* dc)
{
	self->SetDC(dc);
}
	
EWXWEXPORT(void,ELJPrintout_SetPageSizePixels)(void* _obj,int w,int h)
{
	((ELJPrintout*)_obj)->SetPageSizePixels(w, h);
}
	
EWXWEXPORT(void,ELJPrintout_GetPageSizePixels)(void* _obj,int* w,int* h)
{
	((ELJPrintout*)_obj)->GetPageSizePixels(w,h);
}
	
EWXWEXPORT(void,ELJPrintout_SetPageSizeMM)(void* _obj,int w,int h)
{
	((ELJPrintout*)_obj)->SetPageSizeMM(w, h);
}
	
EWXWEXPORT(void,ELJPrintout_GetPageSizeMM)(void* _obj,int* w,int* h)
{
	((ELJPrintout*)_obj)->GetPageSizeMM(w,h);
}
	
EWXWEXPORT(void,ELJPrintout_SetPPIScreen)(void* _obj,int x,int y)
{
	((ELJPrintout*)_obj)->SetPPIScreen(x, y);
}
	
EWXWEXPORT(void,ELJPrintout_GetPPIScreen)(void* _obj,int* x,int* y)
{
	((ELJPrintout*)_obj)->GetPPIScreen(x,y);
}
	
EWXWEXPORT(void,ELJPrintout_SetPPIPrinter)(void* _obj,int x,int y)
{
	((ELJPrintout*)_obj)->SetPPIPrinter(x, y);
}
	
EWXWEXPORT(void,ELJPrintout_GetPPIPrinter)(void* _obj,int* x,int* y)
{
	((ELJPrintout*)_obj)->GetPPIPrinter(x,y);
}
	
EWXWEXPORT(bool,ELJPrintout_IsPreview)(ELJPrintout* self)
{
	return self->IsPreview();
}

EWXWEXPORT(void*,wxPreviewCanvas_Create)(void* preview,wxWindow* parent,int x,int y,int w,int h,int style)
{
	return (void*) new wxPreviewCanvas(	(wxPrintPreviewBase*)preview,parent,
                    				 wxPoint(x, y),wxSize(w, h),
                    					(long)style);
}

EWXWEXPORT(void*,ELJPreviewFrame_Create)(void* _obj,void* _init,void* _create_canvas,void* _create_toolbar,void* preview,void* parent,void* title,int x,int y,int w,int h,int style)
{
    return (void*) new ELJPreviewFrame(_obj, _init, _create_canvas, _create_toolbar, preview, parent, title, x, y, w, h, style);
}

EWXWEXPORT(void,ELJPreviewFrame_Initialize)(ELJPreviewFrame* self)
{
	self->Initialize();
}
	
EWXWEXPORT(void,ELJPreviewFrame_SetPreviewCanvas)(ELJPreviewFrame* self,void* obj)
{
	self->SetPreviewCanvas (obj);
}
	
EWXWEXPORT(void,ELJPreviewFrame_SetControlBar)(ELJPreviewFrame* self,void* obj)
{
	self->SetControlBar (obj);
}
	
EWXWEXPORT(void,ELJPreviewFrame_SetPrintPreview)(ELJPreviewFrame* self,void* obj)
{
	self->SetPrintPreview (obj);
}
	
EWXWEXPORT(void*,ELJPreviewFrame_GetPreviewCanvas)(ELJPreviewFrame* self)
{
	return (void*)self->GetPreviewCanvas ();
}
	
EWXWEXPORT(void*,ELJPreviewFrame_GetControlBar)(ELJPreviewFrame* self)
{
	return (void*)self->GetControlBar ();
}
	
EWXWEXPORT(void*,ELJPreviewFrame_GetPrintPreview)(ELJPreviewFrame* self)
{
	return (void*)self->GetPrintPreview ();
}
	
EWXWEXPORT(void*, ELJPreviewControlBar_Create)(void* preview,int buttons,wxWindow* parent,void* title,int x,int y,int w,int h,int style)
{
    return (void*) new wxPreviewControlBar((wxPrintPreviewBase*)preview, (long)buttons, parent, wxPoint(x, y), wxSize(w, h), (long)style);
}

EWXWEXPORT(void*,wxPrintPreview_CreateFromDialogData)(void* printout,void* printoutForPrinting,void* data)
{
    return (void*) new wxPrintPreview((wxPrintout*)printout, (wxPrintout*)printoutForPrinting, (wxPrintDialogData*)data);
}

EWXWEXPORT(void*,wxPrintPreview_CreateFromData)(void* printout,void* printoutForPrinting,void* data)
{
    return (void*) new wxPrintPreview((wxPrintout*)printout, (wxPrintout*)printoutForPrinting, (wxPrintData*)data);
}

EWXWEXPORT(void,wxPrintPreview_Delete)(wxPrintPreview* self)
{
	delete self;
}

EWXWEXPORT(void,wxPrintPreview_SetCurrentPage)(wxPrintPreview* self,int pageNum)
{
	self->SetCurrentPage(pageNum);
}
	
EWXWEXPORT(int,wxPrintPreview_GetCurrentPage)(wxPrintPreview* self)
{
	return self->GetCurrentPage();
}
	
EWXWEXPORT(void,wxPrintPreview_SetPrintout)(wxPrintPreview* self,wxPrintout* printout)
{
	self->SetPrintout(printout);
}
	
EWXWEXPORT(void*,wxPrintPreview_GetPrintout)(wxPrintPreview* self)
{
	return (void*)self->GetPrintout();
}
	
EWXWEXPORT(void*,wxPrintPreview_GetPrintoutForPrinting)(wxPrintPreview* self)
{
	return (void*)self->GetPrintoutForPrinting();
}
	
EWXWEXPORT(void,wxPrintPreview_SetFrame)(wxPrintPreview* self,wxFrame* frame)
{
	self->SetFrame(frame);
}
	
EWXWEXPORT(void,wxPrintPreview_SetCanvas)(wxPrintPreview* self,wxPreviewCanvas* canvas)
{
	self->SetCanvas(canvas);
}
	
EWXWEXPORT(void*,wxPrintPreview_GetFrame)(wxPrintPreview* self)
{
	return (void*)self->GetFrame();
}
	
EWXWEXPORT(void*,wxPrintPreview_GetCanvas)(wxPrintPreview* self)
{
	return (void*)self->GetCanvas();
}
	
EWXWEXPORT(bool,wxPrintPreview_PaintPage)(wxPrintPreview* self,wxPreviewCanvas* canvas,wxDC* dc)
{
	return self->PaintPage(canvas,*dc);
}
	
EWXWEXPORT(bool,wxPrintPreview_DrawBlankPage)(wxPrintPreview* self,wxPreviewCanvas* canvas,wxDC* dc)
{
	return self->DrawBlankPage(canvas,*dc);
}
	
EWXWEXPORT(bool,wxPrintPreview_RenderPage)(wxPrintPreview* self,int pageNum)
{
	return self->RenderPage(pageNum);
}
	
EWXWEXPORT(void,wxPrintPreview_GetPrintDialogData)(wxPrintPreview* self,wxPrintDialogData* _ref)
{
	*_ref = self->GetPrintDialogData();
}
	
EWXWEXPORT(void,wxPrintPreview_SetZoom)(wxPrintPreview* self,int percent)
{
	self->SetZoom(percent);
}
	
EWXWEXPORT(int,wxPrintPreview_GetZoom)(wxPrintPreview* self)
{
	return self->GetZoom();
}
	
EWXWEXPORT(int,wxPrintPreview_GetMaxPage)(wxPrintPreview* self)
{
	return self->GetMaxPage();
}
	
EWXWEXPORT(int,wxPrintPreview_GetMinPage)(wxPrintPreview* self)
{
	return self->GetMinPage();
}
	
EWXWEXPORT(bool,wxPrintPreview_IsOk)(wxPrintPreview* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(void,wxPrintPreview_SetOk)(wxPrintPreview* self,bool ok)
{
	self->SetOk(ok);
}
	
EWXWEXPORT(bool,wxPrintPreview_Print)(wxPrintPreview* self,bool interactive)
{
	return self->Print(interactive);
}
	
EWXWEXPORT(void,wxPrintPreview_DetermineScaling)(wxPrintPreview* self)
{
	self->DetermineScaling();
}
	
EWXWEXPORT(void*,wxPrintData_Create)()
{
	return (void*) new wxPrintData();
}

EWXWEXPORT(void,wxPrintData_Delete)(wxPrintData* self)
{
	delete self;
}

EWXWEXPORT(void*,wxPostScriptPrintNativeData_Create)()
{
#ifdef wxUSE_POSTSCRIPT
	return (void*)new wxPostScriptPrintNativeData();
#else
	return NULL;
#endif
}

EWXWEXPORT(void,wxPostScriptPrintNativeData_Delete)(void* self)
{
#ifdef wxUSE_POSTSCRIPT
	delete (wxPostScriptPrintNativeData*)self;
#endif
}

EWXWEXPORT(int,wxPrintData_GetNoCopies)(wxPrintData* self)
{
	return self->GetNoCopies();
}
	
EWXWEXPORT(bool,wxPrintData_GetCollate)(wxPrintData* self)
{
	return self->GetCollate();
}
	
EWXWEXPORT(int,wxPrintData_GetOrientation)(wxPrintData* self)
{
	return self->GetOrientation();
}
	
EWXWEXPORT(wxString*,wxPrintData_GetPrinterName)(wxPrintData* self)
{
	return new wxString(self->GetPrinterName());
}
	
EWXWEXPORT(bool,wxPrintData_GetColour)(wxPrintData* self)
{
	return self->GetColour();
}
	
EWXWEXPORT(int,wxPrintData_GetDuplex)(wxPrintData* self)
{
	return (int)self->GetDuplex();
}
	
EWXWEXPORT(int,wxPrintData_GetPaperId)(wxPrintData* self)
{
	return (int)self->GetPaperId();
}
	
EWXWEXPORT(wxSize*,wxPrintData_GetPaperSize)(wxPrintData* self)
{
	wxSize* s = new wxSize();
	*s = self->GetPaperSize();
	return s;
}
	
EWXWEXPORT(int,wxPrintData_GetQuality)(wxPrintData* self)
{
	return (int)self->GetQuality();
}
	
EWXWEXPORT(void,wxPrintData_SetNoCopies)(wxPrintData* self,int v)
{
	self->SetNoCopies(v);
}
	
EWXWEXPORT(void,wxPrintData_SetCollate)(wxPrintData* self,bool flag)
{
	self->SetCollate(flag);
}
	
EWXWEXPORT(void,wxPrintData_SetOrientation)(wxPrintData* self,int orient)
{
	self->SetOrientation((wxPrintOrientation) orient);
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterName)(wxPrintData* self,wxString* name)
{
	self->SetPrinterName(*name);
}
	
EWXWEXPORT(void,wxPrintData_SetColour)(wxPrintData* self,bool colour)
{
	self->SetColour(colour);
}
	
EWXWEXPORT(void,wxPrintData_SetDuplex)(wxPrintData* self,int duplex)
{
	self->SetDuplex((wxDuplexMode)duplex);
}
	
EWXWEXPORT(void,wxPrintData_SetPaperId)(wxPrintData* self,int sizeId)
{
	self->SetPaperId((wxPaperSize)sizeId);
}
	
EWXWEXPORT(void,wxPrintData_SetPaperSize)(wxPrintData* self,int w,int h)
{
	self->SetPaperSize(wxSize(w, h));
}
	
EWXWEXPORT(void,wxPrintData_SetQuality)(wxPrintData* self,int quality)
{
	self->SetQuality((wxPrintQuality)quality);
}
	
EWXWEXPORT(wxString*,wxPrintData_GetPrinterCommand)(void* self)
{
#if wxVERSION_NUMBER < 2600 || defined (wxUSE_POSTSCRIPT)
#ifdef wxUSE_POSTSCRIPT
	wxString tmp = ((wxPostScriptPrintNativeData*)self)->GetPrinterCommand();
#else
	wxString tmp = ((wxPrintData*)self)->GetPrinterCommand();
#endif
	return new wxString(tmp);
#else
	return NULL;
#endif
}
	
EWXWEXPORT(wxString*,wxPrintData_GetPrinterOptions)(void* self)
{
#if wxVERSION_NUMBER < 2600 || defined (wxUSE_POSTSCRIPT)
#ifdef wxUSE_POSTSCRIPT
	wxString tmp = ((wxPostScriptPrintNativeData*)self)->GetPrinterOptions();
#else
	wxString tmp = ((wxPrintData*)self)->GetPrinterOptions();
#endif
	return new wxString(tmp);
#else
	return NULL;
#endif
}
	
EWXWEXPORT(wxString*,wxPrintData_GetPreviewCommand)(void* self)
{
#if wxVERSION_NUMBER < 2600 || defined (wxUSE_POSTSCRIPT)
#ifdef wxUSE_POSTSCRIPT
	wxString tmp = ((wxPostScriptPrintNativeData*)self)->GetPreviewCommand();
#else
	wxString tmp = ((wxPrintData*)self)->GetPreviewCommand();
#endif
	return new wxString(tmp);
#else
	return NULL;
#endif
}
	
EWXWEXPORT(wxString*,wxPrintData_GetFilename)(wxPrintData* self)
{
	wxString tmp = self->GetFilename();
	return new wxString(tmp);
}
	
EWXWEXPORT(wxString*,wxPrintData_GetFontMetricPath)(void* self)
{
#if wxVERSION_NUMBER < 2600 || defined (wxUSE_POSTSCRIPT)
#ifdef wxUSE_POSTSCRIPT
	wxString tmp = ((wxPostScriptPrintNativeData*)self)->GetFontMetricPath();
#else
	wxString tmp = ((wxPrintData*)self)->GetFontMetricPath();
#endif
	return new wxString(tmp);
#else
	return NULL;
#endif
}
	
EWXWEXPORT(double,wxPrintData_GetPrinterScaleX)(void* self)
{
#ifdef wxUSE_POSTSCRIPT
	return ((wxPostScriptPrintNativeData*)self)->GetPrinterScaleX();
#elif wxVERSION_NUMBER < 2600
	return ((wxPrintData*)self)->GetPrinterScaleX();
#else
	return 0.0;
#endif
}
	
EWXWEXPORT(double,wxPrintData_GetPrinterScaleY)(void* self)
{
#ifdef wxUSE_POSTSCRIPT
	return ((wxPostScriptPrintNativeData*)self)->GetPrinterScaleY();
#elif wxVERSION_NUMBER < 2600
	return ((wxPrintData*)self)->GetPrinterScaleY();
#else
	return 0.0;
#endif
}
	
EWXWEXPORT(int,wxPrintData_GetPrinterTranslateX)(void* self)
{
#ifdef wxUSE_POSTSCRIPT
	return ((wxPostScriptPrintNativeData*)self)->GetPrinterTranslateX();
#elif wxVERSION_NUMBER < 2600
	return ((wxPrintData*)self)->GetPrinterTranslateX();
#else
	return 0;
#endif
}
	
EWXWEXPORT(int,wxPrintData_GetPrinterTranslateY)(void* self)
{
#ifdef wxUSE_POSTSCRIPT
	return ((wxPostScriptPrintNativeData*)self)->GetPrinterTranslateY();
#elif wxVERSION_NUMBER < 2600
	return ((wxPrintData*)self)->GetPrinterTranslateY();
#else
	return 0;
#endif
}
	
EWXWEXPORT(int,wxPrintData_GetPrintMode)(wxPrintData* self)
{
	return (int)self->GetPrintMode();
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterCommand)(void* self,wxString* command)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)self)->SetPrinterCommand(*command);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)self)->SetPrinterCommand(*command);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterOptions)(void* self,wxString* options)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)self)->SetPrinterOptions(*options);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)self)->SetPrinterOptions(*options);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPreviewCommand)(void* self,wxString* command)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)self)->SetPreviewCommand(*command);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)self)->SetPreviewCommand(*command);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetFilename)(wxPrintData* self,wxString* filename)
{
	self->SetFilename(*filename);
}
	
EWXWEXPORT(void,wxPrintData_SetFontMetricPath)(void* self,wxString* path)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)self)->SetFontMetricPath(*path);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)self)->SetFontMetricPath(*path);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterScaleX)(void* self,double x)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)self)->SetPrinterScaleX(x);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)self)->SetPrinterScaleX(x);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterScaleY)(void* self,double y)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)self)->SetPrinterScaleY(y);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)self)->SetPrinterScaleY(y);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterScaling)(void* self,double x,double y)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)self)->SetPrinterScaling(x, y);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)self)->SetPrinterScaling(x, y);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterTranslateX)(void* self,int x)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)self)->SetPrinterTranslateX((int)x);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)self)->SetPrinterTranslateX((int)x);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterTranslateY)(void* self,int y)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)self)->SetPrinterTranslateY((int)y);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)self)->SetPrinterTranslateY((long)y);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrinterTranslation)(void* self,int x,int y)
{
#ifdef wxUSE_POSTSCRIPT
	((wxPostScriptPrintNativeData*)self)->SetPrinterTranslation((long)x, (long)y);
#elif wxVERSION_NUMBER < 2600
	((wxPrintData*)self)->SetPrinterTranslation((long)x, (long)y);
#endif
}
	
EWXWEXPORT(void,wxPrintData_SetPrintMode)(void* self,int printMode)
{
	((wxPrintData*)self)->SetPrintMode((wxPrintMode)printMode);
}
	
EWXWEXPORT(void,wxPrintData_Assign)(void* self,void* data)
{
	*((wxPrintData*)self) = *((wxPrintData*)data);
}
	
EWXWEXPORT(void*,wxPrintDialogData_CreateDefault)()
{
	return (void*)new wxPrintDialogData();
}

EWXWEXPORT(void*,wxPrintDialogData_CreateFromData)(void* printData)
{
	return (void*)new wxPrintDialogData(*((wxPrintData*)printData));
}

EWXWEXPORT(void,wxPrintDialogData_Delete)(void* self)
{
	delete (wxPrintDialogData*)self;
}

EWXWEXPORT(int,wxPrintDialogData_GetFromPage)(void* self)
{
	return ((wxPrintDialogData*)self)->GetFromPage();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetToPage)(void* self)
{
	return ((wxPrintDialogData*)self)->GetToPage();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetMinPage)(void* self)
{
	return ((wxPrintDialogData*)self)->GetMinPage();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetMaxPage)(void* self)
{
	return ((wxPrintDialogData*)self)->GetMaxPage();
}
	
EWXWEXPORT(int,wxPrintDialogData_GetNoCopies)(void* self)
{
	return ((wxPrintDialogData*)self)->GetNoCopies();
}
	
EWXWEXPORT(bool,wxPrintDialogData_GetAllPages)(wxPrintDialogData* self)
{
	return self->GetAllPages();
}
	
EWXWEXPORT(bool,wxPrintDialogData_GetSelection)(wxPrintDialogData* self)
{
	return self->GetSelection();
}
	
EWXWEXPORT(bool,wxPrintDialogData_GetCollate)(wxPrintDialogData* self)
{
	return self->GetCollate();
}
	
EWXWEXPORT(bool,wxPrintDialogData_GetPrintToFile)(wxPrintDialogData* self)
{
	return self->GetPrintToFile();
}

EWXWEXPORT(void,wxPrintDialogData_SetFromPage)(wxPrintDialogData* self,int v)
{
	self->SetFromPage(v);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetToPage)(wxPrintDialogData* self,int v)
{
	self->SetToPage(v);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetMinPage)(wxPrintDialogData* self,int v)
{
	self->SetMinPage(v);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetMaxPage)(wxPrintDialogData* self,int v)
{
	self->SetMaxPage(v);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetNoCopies)(wxPrintDialogData* self,int v)
{
	self->SetNoCopies(v);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetAllPages)(wxPrintDialogData* self,bool flag)
{
	self->SetAllPages(flag);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetSelection)(wxPrintDialogData* self,bool flag)
{
	self->SetSelection(flag);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetCollate)(wxPrintDialogData* self,bool flag)
{
	self->SetCollate(flag);
}
	
EWXWEXPORT(void,wxPrintDialogData_SetPrintToFile)(wxPrintDialogData* self,bool flag)
{
	self->SetPrintToFile(flag);
}

EWXWEXPORT(void,wxPrintDialogData_EnablePrintToFile)(wxPrintDialogData* self,bool flag)
{
	self->EnablePrintToFile(flag);
}
	
EWXWEXPORT(void,wxPrintDialogData_EnableSelection)(wxPrintDialogData* self,bool flag)
{
	self->EnableSelection(flag);
}
	
EWXWEXPORT(void,wxPrintDialogData_EnablePageNumbers)(wxPrintDialogData* self,bool flag)
{
	self->EnablePageNumbers(flag);
}
	
EWXWEXPORT(void,wxPrintDialogData_EnableHelp)(wxPrintDialogData* self,bool flag)
{
	self->EnableHelp(flag);
}
	
EWXWEXPORT(bool,wxPrintDialogData_GetEnablePrintToFile)(wxPrintDialogData* self)
{
	return self->GetEnablePrintToFile();
}
	
EWXWEXPORT(bool,wxPrintDialogData_GetEnableSelection)(wxPrintDialogData* self)
{
	return self->GetEnableSelection();
}
	
EWXWEXPORT(bool,wxPrintDialogData_GetEnablePageNumbers)(wxPrintDialogData* self)
{
	return self->GetEnablePageNumbers();
}
	
EWXWEXPORT(bool,wxPrintDialogData_GetEnableHelp)(wxPrintDialogData* self)
{
	return self->GetEnableHelp();
}
	
EWXWEXPORT(void,wxPrintDialogData_GetPrintData)(wxPrintDialogData* self,wxPrintData* _ref)
{
	*_ref = self->GetPrintData();
}
	
EWXWEXPORT(void,wxPrintDialogData_SetPrintData)(wxPrintDialogData* self,wxPrintData* printData)
{
	self->SetPrintData(*printData);
}
	
EWXWEXPORT(void,wxPrintDialogData_Assign)(wxPrintDialogData* self,wxPrintDialogData* data)
{
	*self = *data;
}
	
EWXWEXPORT(void,wxPrintDialogData_AssignData)(wxPrintDialogData* self,wxPrintData* data)
{
	*self = *data;
}
	
EWXWEXPORT(wxPostScriptDC*,wxPostScriptDC_Create)(wxPrintData* printData)
{
#ifdef wxUSE_POSTSCRIPT
	return new wxPostScriptDC(*printData);
#else
	return NULL;
#endif
}

EWXWEXPORT(void,wxPostScriptDC_Delete)(wxPostScriptDC* self)
{
#ifdef wxUSE_POSTSCRIPT
	if (self) delete self;
#endif
}

EWXWEXPORT(void,wxPostScriptDC_SetResolution)(wxPostScriptDC* self,int ppi)
{
#if (defined(wxUSE_POSTSCRIPT)) && (wxVERSION_NUMBER < 2900)
	self->SetResolution(ppi);
#endif
}

EWXWEXPORT(int,wxPostScriptDC_GetResolution)(wxPostScriptDC* self,int ppi)
{
#ifdef wxUSE_POSTSCRIPT
	return self->GetResolution();
#else
	return 0;
#endif
}

EWXWEXPORT(void*,wxPrinterDC_Create)(wxPrintData* printData)
{
#if defined(__WXGTK__) 
	return NULL;
#else
	return new wxPrinterDC(*printData);
#endif
}

EWXWEXPORT(void,wxPrinterDC_Delete)(void* self)
{
#if !defined(__WXGTK__)
	delete (wxPrinterDC*)self;
#endif
}


EWXWEXPORT(wxRect*,wxPrinterDC_GetPaperRect)(void* self)
{
#if !defined(__WXGTK__)
	wxRect* rct = new wxRect();
	*rct = ((wxPrinterDC*)self)->GetPaperRect();
	return rct;
#else
	return 0;
#endif
}

}
