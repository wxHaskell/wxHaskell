#include "wrapper.h"
#include <wx/metafile.h>
#include <wx/dcmirror.h>
#include <wx/dcbuffer.h>

extern "C"
{

EWXWEXPORT(void,wxDC_Delete)(wxDC* self)
{
	delete  self;
}

EWXWEXPORT(void,wxDC_FloodFill)(wxDC* self,int x,int y,wxColour* col,int style)
{
#if (wxVERSION_NUMBER < 2900)
    int _style = style;
#else
    wxFloodFillStyle _style = (wxFloodFillStyle) style;
#endif
	self->FloodFill((wxCoord)x, (wxCoord)y,*col, _style);
}
	
EWXWEXPORT(int,wxDC_GetPixel)(wxDC* self,int x,int y,wxColour* col)
{
	return self->GetPixel((wxCoord)x, (wxCoord)y, col);
}
	
EWXWEXPORT(void,wxDC_DrawLine)(wxDC* self,int x1,int y1,int x2,int y2)
{
	self->DrawLine((wxCoord)x1, (wxCoord)y1, (wxCoord)x2, (wxCoord)y2);
}
	
EWXWEXPORT(void,wxDC_CrossHair)(wxDC* self,int x,int y)
{
	self->CrossHair((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void,wxDC_DrawArc)(wxDC* self,int x1,int y1,int x2,int y2,int xc,int yc)
{
	self->DrawArc((wxCoord)x1, (wxCoord)y1, (wxCoord)x2, (wxCoord)y2, (wxCoord)xc, (wxCoord)yc);
}
	
EWXWEXPORT(void,wxDC_DrawCheckMark)(wxDC* self,int x,int y,int width,int height)
{
	self->DrawCheckMark((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(void,wxDC_DrawEllipticArc)(wxDC* self,int x,int y,int w,int h,double sa,double ea)
{
	self->DrawEllipticArc((wxCoord)x, (wxCoord)y, (wxCoord)w, (wxCoord)h, sa, ea);
}
	
EWXWEXPORT(void,wxDC_DrawPoint)(wxDC* self,int x,int y)
{
	self->DrawPoint((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void,wxDC_DrawLines)(wxDC* self,int n,void* x,void* y,int xoffset,int yoffset)
{
	wxPoint* lst = (wxPoint*)malloc (n * sizeof(wxPoint));
	
	for (int i = 0; i < n; i++)
		lst[i] = wxPoint((int)((intptr_t*)x)[i], (int)((intptr_t*)y)[i]);
	
	self->DrawLines(n, lst, (wxCoord)xoffset, (wxCoord)yoffset);
	
	free (lst);
}
	
EWXWEXPORT(void,wxDC_DrawPolygon)(wxDC* self,int n,void* x,void* y,int xoffset,int yoffset,int fillStyle)
{
#if (wxVERSION_NUMBER < 2900)
    int _style = fillStyle;
#else
    wxPolygonFillMode _style = (wxPolygonFillMode) fillStyle;
#endif
	wxPoint* lst = (wxPoint*)malloc (n * sizeof(wxPoint));
	
	for (int i = 0; i < n; i++)
		lst[i] = wxPoint(((intptr_t*)x)[i], ((intptr_t*)y)[i]);
	
	self->DrawPolygon(n, lst, (wxCoord)xoffset, (wxCoord)yoffset, _style);
	
	free (lst);
}
	
EWXWEXPORT(void,wxDC_DrawRectangle)(wxDC* self,int x,int y,int width,int height)
{
	self->DrawRectangle((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(void,wxDC_DrawRoundedRectangle)(wxDC* self,int x,int y,int width,int height,double radius)
{
	self->DrawRoundedRectangle((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height, radius);
}
	
EWXWEXPORT(void,wxDC_DrawCircle)(wxDC* self,int x,int y,int radius)
{
	self->DrawCircle((wxCoord)x, (wxCoord)y, (wxCoord)radius);
}
	
EWXWEXPORT(void,wxDC_DrawEllipse)(wxDC* self,int x,int y,int width,int height)
{
	self->DrawEllipse((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(void,wxDC_DrawIcon)(wxDC* self,wxIcon* icon,int x,int y)
{
	self->DrawIcon(*icon, (wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void,wxDC_DrawBitmap)(wxDC* self,wxBitmap* bmp,int x,int y,bool useMask)
{
	self->DrawBitmap(*bmp, (wxCoord)x, (wxCoord)y, useMask);
}
	
EWXWEXPORT(void,wxDC_DrawText)(wxDC* self,wxString* text,int x,int y)
{
	self->DrawText(*text, (wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void,wxDC_DrawRotatedText)(wxDC* self,wxString* text,int x,int y,double angle)
{
	self->DrawRotatedText(*text, (wxCoord)x, (wxCoord)y, angle);
}
	
EWXWEXPORT(bool,wxDC_Blit)(wxDC* self,int xdest,int ydest,int width,int height,wxDC* source,int xsrc,int ysrc,int rop,bool useMask)
{
#if (wxVERSION_NUMBER < 2900)
    int _rop = rop;
#else
    wxRasterOperationMode _rop = (wxRasterOperationMode) rop;
#endif
	return self->Blit((wxCoord)xdest, (wxCoord)ydest, (wxCoord)width, (wxCoord)height, source, (wxCoord)xsrc, (wxCoord)ysrc, _rop, useMask);
}
	
EWXWEXPORT(void,wxDC_Clear)(wxDC* self)
{
	self->Clear();
}
	
EWXWEXPORT(void,wxDC_ComputeScaleAndOrigin)(wxDC* dc)
{
#if (wxVERSION_NUMBER < 2900)
	dc->ComputeScaleAndOrigin();
#endif
}
	
EWXWEXPORT(bool,wxDC_StartDoc)(wxDC* self,wxString* msg)
{
	return self->StartDoc(*msg);
}
	
EWXWEXPORT(void,wxDC_EndDoc)(wxDC* self)
{
	self->EndDoc();
}
	
EWXWEXPORT(void,wxDC_StartPage)(wxDC* self)
{
	self->StartPage();
}
	
EWXWEXPORT(void,wxDC_EndPage)(wxDC* self)
{
	self->EndPage();
}
	
EWXWEXPORT(void,wxDC_SetFont)(wxDC* self,wxFont* font)
{
	self->SetFont(*font);
}
	
EWXWEXPORT(void,wxDC_SetPen)(wxDC* self,wxPen* pen)
{
	self->SetPen(*pen);
}
	
EWXWEXPORT(void,wxDC_SetBrush)(wxDC* self,wxBrush* brush)
{
	self->SetBrush(*brush);
}
	
EWXWEXPORT(void,wxDC_SetBackground)(wxDC* self,wxBrush* brush)
{
	self->SetBackground(*brush);
}
	
EWXWEXPORT(void,wxDC_SetBackgroundMode)(wxDC* self,int mode)
{
	self->SetBackgroundMode(mode);
}
	
EWXWEXPORT(void,wxDC_SetPalette)(wxDC* self,wxPalette* palette)
{
	self->SetPalette(*palette);
}
	
EWXWEXPORT(void,wxDC_SetClippingRegion)(wxDC* self,int x,int y,int width,int height)
{
	self->SetClippingRegion((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(void,wxDC_SetClippingRegionFromRegion)(wxDC* self,wxRegion* region)
{
# if wxCHECK_VERSION(2,9,5)
	self->SetDeviceClippingRegion(*region);
# else
	self->SetClippingRegion(*region);
# endif
}
	
#if wxCHECK_VERSION(2,9,5)
EWXWEXPORT(void,wxDC_SetDeviceClippingRegion)(wxDC* self,wxRegion* region)
{
	self->SetDeviceClippingRegion(*region);
}
#endif
	
EWXWEXPORT(void,wxDC_DestroyClippingRegion)(wxDC* self)
{
	self->DestroyClippingRegion();
}
	
EWXWEXPORT(void,wxDC_GetClippingBox)(wxDC* self,wxCoord* x,wxCoord* y,wxCoord* w,wxCoord* h)
{
	self->GetClippingBox(x,y,w,h);
}
	
EWXWEXPORT(wxCoord,wxDC_GetCharHeight)(wxDC* self)
{
	return self->GetCharHeight();
}
	
EWXWEXPORT(wxCoord,wxDC_GetCharWidth)(wxDC* self)
{
	return self->GetCharWidth();
}
	
EWXWEXPORT(void,wxDC_GetTextExtent)(wxDC* self,wxString* string,wxCoord* w,wxCoord* h,wxCoord* descent,wxCoord* externalLeading,wxFont* theFont)
{
	self->GetTextExtent(*string,w,h,descent,externalLeading,theFont);
}
	
EWXWEXPORT(void,wxDC_GetMultiLineTextExtent)(wxDC* self,wxString* string,wxCoord* w,wxCoord* h,wxCoord* heightLine,wxFont* theFont)
{
	self->GetMultiLineTextExtent(*string, w, h, heightLine, theFont);
}

EWXWEXPORT(wxSize*,wxDC_GetSize)(wxDC* self)
{
	wxSize* s = new wxSize();
	*s = self->GetSize();
	return s;
}
	
EWXWEXPORT(wxSize*,wxDC_GetSizeMM)(wxDC* self)
{
	wxSize* s = new wxSize();
	*s = self->GetSizeMM();
	return s;
}
	
EWXWEXPORT(wxCoord,wxDC_DeviceToLogicalX)(wxDC* self,wxCoord x)
{
	return self->DeviceToLogicalX(x);
}
	
EWXWEXPORT(wxCoord,wxDC_DeviceToLogicalY)(wxDC* self,wxCoord y)
{
	return self->DeviceToLogicalY(y);
}
	
EWXWEXPORT(wxCoord,wxDC_DeviceToLogicalXRel)(wxDC* self,wxCoord x)
{
	return self->DeviceToLogicalXRel(x);
}
	
EWXWEXPORT(wxCoord,wxDC_DeviceToLogicalYRel)(wxDC* self,int y)
{
	return self->DeviceToLogicalYRel((wxCoord)y);
}
	
EWXWEXPORT(wxCoord,wxDC_LogicalToDeviceX)(wxDC* self,int x)
{
	return self->LogicalToDeviceX((wxCoord)x);
}
	
EWXWEXPORT(wxCoord,wxDC_LogicalToDeviceY)(wxDC* self,int y)
{
	return self->LogicalToDeviceY((wxCoord)y);
}
	
EWXWEXPORT(wxCoord,wxDC_LogicalToDeviceXRel)(wxDC* self,int x)
{
	return self->LogicalToDeviceXRel((wxCoord)x);
}
	
EWXWEXPORT(wxCoord,wxDC_LogicalToDeviceYRel)(wxDC* self,int y)
{
	return self->LogicalToDeviceYRel((wxCoord)y);
}
	
EWXWEXPORT(bool,wxDC_CanDrawBitmap)(wxDC* self)
{
	return self->CanDrawBitmap();
}
	
EWXWEXPORT(bool,wxDC_CanGetTextExtent)(wxDC* self)
{
	return self->CanGetTextExtent();
}
	
EWXWEXPORT(int,wxDC_GetDepth)(wxDC* self)
{
	return self->GetDepth();
}
	
EWXWEXPORT(wxSize*,wxDC_GetPPI)(wxDC* self)
{
	wxSize* s = new wxSize();
	*s = self->GetPPI();
	return s;
}
	
EWXWEXPORT(bool,wxDC_IsOk)(wxDC* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(int,wxDC_GetBackgroundMode)(wxDC* self)
{
	return self->GetBackgroundMode();
}
	
EWXWEXPORT(void,wxDC_GetBackground)(wxDC* self,wxBrush* _ref)
{
	*_ref = self->GetBackground();
}
	
EWXWEXPORT(void,wxDC_GetBrush)(wxDC* self,wxBrush* _ref)
{
	*_ref = self->GetBrush();
}
	
EWXWEXPORT(void,wxDC_GetFont)(wxDC* self,wxFont* _ref)
{
	*_ref = self->GetFont();
}
	
EWXWEXPORT(void,wxDC_GetPen)(wxDC* self,wxPen* _ref)
{
	*_ref = self->GetPen();
}
	
EWXWEXPORT(void,wxDC_GetTextBackground)(wxDC* self,wxColour* _ref)
{
	*_ref = self->GetTextBackground();
}
	
EWXWEXPORT(void,wxDC_GetTextForeground)(wxDC* self,wxColour* _ref)
{
	*_ref = self->GetTextForeground();
}
	
EWXWEXPORT(void,wxDC_SetTextForeground)(wxDC* self,wxColour* colour)
{
	self->SetTextForeground(*colour);
}
	
EWXWEXPORT(void,wxDC_SetTextBackground)(wxDC* self,wxColour* colour)
{
	self->SetTextBackground(*colour);
}
	
EWXWEXPORT(int,wxDC_GetMapMode)(wxDC* self)
{
	return self->GetMapMode();
}
	
EWXWEXPORT(void,wxDC_SetMapMode)(wxDC* self,int mode)
{
#if (wxVERSION_NUMBER < 2900)
    int _mode = mode;
#else
    wxMappingMode _mode = (wxMappingMode) mode;
#endif
	self->SetMapMode(_mode);
}
	
EWXWEXPORT(void,wxDC_GetUserScale)(wxDC* self,double* x,double* y)
{
	self->GetUserScale(x,y);
}
	
EWXWEXPORT(void,wxDC_SetUserScale)(wxDC* self,double x,double y)
{
	self->SetUserScale(x, y);
}
	
EWXWEXPORT(void,wxDC_GetLogicalScale)(wxDC* self,double* x,double* y)
{
	self->GetLogicalScale(x,y);
}
	
EWXWEXPORT(void,wxDC_SetLogicalScale)(wxDC* self,double x,double y)
{
	self->SetLogicalScale(x, y);
}
	
EWXWEXPORT(void,wxDC_GetLogicalOrigin)(wxDC* self,wxCoord* x,wxCoord* y)
{
	self->GetLogicalOrigin(x,y);
}
	
EWXWEXPORT(void,wxDC_SetLogicalOrigin)(wxDC* self,int x,int y)
{
	self->SetLogicalOrigin((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void,wxDC_GetDeviceOrigin)(wxDC* self,wxCoord* x,wxCoord* y)
{
	self->GetDeviceOrigin(x,y);
}
	
EWXWEXPORT(void,wxDC_SetDeviceOrigin)(wxDC* self,int x,int y)
{
	self->SetDeviceOrigin((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void,wxDC_SetAxisOrientation)(wxDC* self,bool xLeftRight,bool yBottomUp)
{
	self->SetAxisOrientation(xLeftRight, yBottomUp);
}
	
EWXWEXPORT(int,wxDC_GetLogicalFunction)(wxDC* self)
{
	return self->GetLogicalFunction();
}
	
EWXWEXPORT(void,wxDC_SetLogicalFunction)(wxDC* self,int function)
{
#if (wxVERSION_NUMBER < 2900)
    int _function = function;
#else
    wxRasterOperationMode _function = (wxRasterOperationMode) function;
#endif
	self->SetLogicalFunction(_function);
}
	
EWXWEXPORT(void,wxDC_CalcBoundingBox)(wxDC* self,int x,int y)
{
	self->CalcBoundingBox((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(void,wxDC_ResetBoundingBox)(wxDC* self)
{
	self->ResetBoundingBox();
}
	
EWXWEXPORT(wxCoord,wxDC_MinX)(wxDC* self)
{
	return self->MinX();
}
	
EWXWEXPORT(wxCoord,wxDC_MaxX)(wxDC* self)
{
	return self->MaxX();
}
	
EWXWEXPORT(wxCoord,wxDC_MinY)(wxDC* self)
{
	return self->MinY();
}
	
EWXWEXPORT(wxCoord,wxDC_MaxY)(wxDC* self)
{
	return self->MaxY();
}

EWXWEXPORT(wxWindowDC*,wxWindowDC_Create)(wxWindow* win)
{
	return new wxWindowDC(win);
}

EWXWEXPORT(void,wxWindowDC_Delete)(wxWindowDC* self)
{
	delete self;
}

EWXWEXPORT(wxClientDC*,wxClientDC_Create)(wxWindow* win)
{
	return new wxClientDC(win);
}

EWXWEXPORT(void,wxClientDC_Delete)(wxClientDC* self)
{
	delete self;
}

EWXWEXPORT(wxPaintDC*,wxPaintDC_Create)(wxWindow* win)
{
	return new wxPaintDC(win);
}

EWXWEXPORT(void,wxPaintDC_Delete)(wxPaintDC* self)
{
	delete self;
}

EWXWEXPORT(wxMemoryDC*,wxMemoryDC_Create)()
{
	return new wxMemoryDC();
}

EWXWEXPORT(wxMemoryDC*,wxMemoryDC_CreateCompatible)(wxDC* dc)
{
	return new wxMemoryDC(dc);
}

EWXWEXPORT(wxMemoryDC*,wxMemoryDC_CreateWithBitmap)(wxBitmap* bitmap)
{
	return new wxMemoryDC(*bitmap);
}

EWXWEXPORT(void,wxMemoryDC_Delete)(wxMemoryDC* self)
{
	delete self;
}

EWXWEXPORT(void,wxMemoryDC_SelectObject)(wxMemoryDC* self,wxBitmap* bitmap)
{
	self->SelectObject(*bitmap);
}

EWXWEXPORT(wxMirrorDC*,wxMirrorDC_Create)(wxDC* dc,bool mirror)
{
	return new wxMirrorDC(*dc, mirror);
}

EWXWEXPORT(void,wxMirrorDC_Delete)(wxMirrorDC* self)
{
	if (self) delete self;
}

EWXWEXPORT(wxScreenDC*,wxScreenDC_Create)()
{
	return new wxScreenDC();
}

EWXWEXPORT(void,wxScreenDC_Delete)(wxScreenDC* self)
{
	delete self;
}

EWXWEXPORT(bool,wxScreenDC_StartDrawingOnTopOfWin)(wxScreenDC* self,wxWindow* win)
{
	return self->StartDrawingOnTop(win);
}
	
EWXWEXPORT(bool,wxScreenDC_StartDrawingOnTop)(wxScreenDC* self,int l,int t,int w,int h)
{
	wxRect rect(l, t, w, h);
	return self->StartDrawingOnTop(&rect);
}
	
EWXWEXPORT(bool,wxScreenDC_EndDrawingOnTop)(wxScreenDC* self)
{
	return self->EndDrawingOnTop();
}

EWXWEXPORT(wxBufferedDC*,wxBufferedDC_CreateByDCAndSize)(wxDC* dc,int width,int hight,int style)
{
	return new wxBufferedDC(dc, wxSize(width, hight), style);
}

EWXWEXPORT(wxBufferedDC*,wxBufferedDC_CreateByDCAndBitmap)(wxDC* dc,wxBitmap* buffer,int style)
{
	return new wxBufferedDC(dc,*buffer, style);
}

EWXWEXPORT(void,wxBufferedDC_Delete)(wxBufferedDC* self)
{
	if (self) delete self;
}

EWXWEXPORT(wxBufferedPaintDC*,wxBufferedPaintDC_Create)(wxWindow* window,int style)
{
	return new wxBufferedPaintDC(window, style);
}

EWXWEXPORT(wxBufferedPaintDC*,wxBufferedPaintDC_CreateWithBitmap)(wxWindow* window,wxBitmap* buffer,int style)
{
	return new wxBufferedPaintDC(window,*buffer, style);
}

EWXWEXPORT(void,wxBufferedPaintDC_Delete)(wxBufferedPaintDC* self)
{
	if (self) delete self;
}

EWXWEXPORT(wxAutoBufferedPaintDC*,wxAutoBufferedPaintDC_Create)(wxWindow* window)
{
	return new wxAutoBufferedPaintDC(window);
}

EWXWEXPORT(void,wxAutoBufferedPaintDC_Delete)(wxAutoBufferedPaintDC* self)
{
	if (self) delete self;
}

EWXWEXPORT(void*,wxMetafileDC_Create)(wxString* _file)
{
#if defined(__WXGTK__)
	return NULL;
#else
	wxString file;
	
	if (_file) file = (wxChar*)_file;

	return (void*)new wxMetafileDC(file);
#endif
}

EWXWEXPORT(void*, wxMetafileDC_Close) (void* self)
{
#if defined(__WXGTK__)
	return NULL;
#else
	return (void*)((wxMetafileDC*)self)->Close();
#endif
}

EWXWEXPORT(void, wxMetafileDC_Delete) (void* self)
{
#if !defined(__WXGTK__)
	delete (wxMetafileDC*)self;
#endif
}

EWXWEXPORT(void*,wxMetafile_Create)(wxString* _file)
{
#if defined(__WXGTK__)
	return NULL;
#else
	wxString file;
	
	if (_file) file = (wxChar*)_file;

	return (void*)new wxMetafile(file);
#endif
}

EWXWEXPORT(bool,wxMetafile_SetClipboard)(void* self,int width,int height)
{
#if defined(__WXGTK__)
	return false;
#else
	return ((wxMetafile*)self)->SetClipboard(width, height);
#endif
}
	
EWXWEXPORT(bool,wxMetafile_Play)(void* self,wxDC* _dc)
{
#if defined(__WXGTK__)
	return false;
#else
	return ((wxMetafile*)self)->Play(_dc);
#endif
}
	
EWXWEXPORT(bool,wxMetafile_IsOk)(void* self)
{
#if defined(__WXGTK__)
	return false;
#else
	return ((wxMetafile*)self)->IsOk();
#endif
}
	
EWXWEXPORT(void,wxMetafile_Delete)(void* self)
{
#if !defined(__WXGTK__)
	delete (wxMetafile*)self;
#endif
}

#if wxCHECK_VERSION (2,8,0)

EWXWEXPORT(void,wxDC_DrawLabel)(wxDC* self,wxString* str,int x,int y,int w,int h,int align,int indexAccel)
{
  wxRect rect(x, y, w, h);
  self->DrawLabel(*str, rect, align, indexAccel);
}

EWXWEXPORT(wxRect*,wxDC_DrawLabelBitmap)(wxDC* self,wxString* str,wxBitmap* bmp,int x,int y,int w,int h,int align,int indexAccel)
{
  wxRect rect(x, y, w, h);
  wxRect* r = new wxRect();
  self->DrawLabel(*str,*bmp, rect, align, indexAccel, r);
  return r;
}

EWXWEXPORT(void,wxDC_DrawPolyPolygon)(wxDC* self,int n,int* count,void* x,void* y,int xoffset,int yoffset,int fillStyle)
{
#if (wxVERSION_NUMBER < 2900)
    int _fillStyle = fillStyle;
#else
    wxPolygonFillMode _fillStyle = (wxPolygonFillMode) fillStyle;
#endif
    int     *tmp = count;
    int     *cnt = new int[n];
    int      i, j;
    int      totalItems = 0;
    int      item = 0;

    // Work out the size of wxPoint array required
    for (i = 0; i < n; i++)
    {
      cnt[i] = *tmp++;
      totalItems += cnt[i];
    }
	wxPoint* lst = new wxPoint[totalItems];

	for (i = 0; i < n; i++)
    {
      for(j = 0; j < cnt[i]; j++)
      {
		lst[item] = wxPoint(((intptr_t*)x)[item], ((intptr_t*)y)[item]);
        item++;
      }
    }
	
	self->DrawPolyPolygon(n, cnt, lst, (wxCoord)xoffset, (wxCoord)yoffset, _fillStyle);
	
	free (lst);
    delete cnt;
}
#endif
}
