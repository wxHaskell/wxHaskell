#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxSashWindow_Create)(wxWindow* _par,int _id,int _x,int _y,int _w,int _h,int _stl)
{
	return (void*)new wxSashWindow (_par, _id, wxPoint(_x, _y), wxSize (_w, _h), (long)_stl);
}

EWXWEXPORT(void,wxSashWindow_SetSashVisible)(wxSashWindow* self,int edge,bool sash)
{
	self->SetSashVisible((wxSashEdgePosition)edge, sash);
}
	
EWXWEXPORT(bool,wxSashWindow_GetSashVisible)(wxSashWindow* self,int edge)
{
	return self->GetSashVisible((wxSashEdgePosition)edge);
}
	
EWXWEXPORT(void,wxSashWindow_SetSashBorder)(wxSashWindow* self,int edge,bool border)
{
#if WXWIN_COMPATIBILITY_2_6
	self->SetSashBorder((wxSashEdgePosition)edge, border);
#endif
}
	
EWXWEXPORT(bool,wxSashWindow_HasBorder)(wxSashWindow* self,int edge)
{
#if WXWIN_COMPATIBILITY_2_6
	return self->HasBorder((wxSashEdgePosition)edge);
#else
	return false;
#endif
}
	
EWXWEXPORT(int,wxSashWindow_GetEdgeMargin)(wxSashWindow* self,int edge)
{
	return self->GetEdgeMargin((wxSashEdgePosition)edge);
}
	
EWXWEXPORT(void,wxSashWindow_SetDefaultBorderSize)(wxSashWindow* self,int width)
{
	self->SetDefaultBorderSize(width);
}
	
EWXWEXPORT(int,wxSashWindow_GetDefaultBorderSize)(wxSashWindow* self)
{
	return self->GetDefaultBorderSize();
}
	
EWXWEXPORT(void,wxSashWindow_SetExtraBorderSize)(wxSashWindow* self,int width)
{
	self->SetExtraBorderSize(width);
}
	
EWXWEXPORT(int,wxSashWindow_GetExtraBorderSize)(wxSashWindow* self)
{
	return self->GetExtraBorderSize();
}
	
EWXWEXPORT(void,wxSashWindow_SetMinimumSizeX)(wxSashWindow* self,int min)
{
	self->SetMinimumSizeX(min);
}
	
EWXWEXPORT(void,wxSashWindow_SetMinimumSizeY)(wxSashWindow* self,int min)
{
	self->SetMinimumSizeY(min);
}
	
EWXWEXPORT(int,wxSashWindow_GetMinimumSizeX)(wxSashWindow* self)
{
	return self->GetMinimumSizeX();
}
	
EWXWEXPORT(int,wxSashWindow_GetMinimumSizeY)(wxSashWindow* self)
{
	return self->GetMinimumSizeY();
}
	
EWXWEXPORT(void,wxSashWindow_SetMaximumSizeX)(wxSashWindow* self,int max)
{
	self->SetMaximumSizeX(max);
}
	
EWXWEXPORT(void,wxSashWindow_SetMaximumSizeY)(wxSashWindow* self,int max)
{
	self->SetMaximumSizeY(max);
}
	
EWXWEXPORT(int,wxSashWindow_GetMaximumSizeX)(wxSashWindow* self)
{
	return self->GetMaximumSizeX();
}
	
EWXWEXPORT(int,wxSashWindow_GetMaximumSizeY)(wxSashWindow* self)
{
	return self->GetMaximumSizeY();
}


EWXWEXPORT(void*,wxSashEvent_Create)(int id,int edge)
{
	return (void*)new wxSashEvent(id, (wxSashEdgePosition)edge);
}

EWXWEXPORT(void,wxSashEvent_SetEdge)(wxSashEvent* self,int edge)
{
	self->SetEdge((wxSashEdgePosition)edge);
}
	
EWXWEXPORT(int,wxSashEvent_GetEdge)(wxSashEvent* self)
{
	return (int)self->GetEdge();
}
	
EWXWEXPORT(void,wxSashEvent_SetDragRect)(wxSashEvent* self,int x,int y,int w,int h)
{
	self->SetDragRect(wxRect (x, y , w, h));
}
	
EWXWEXPORT(wxRect*,wxSashEvent_GetDragRect)(wxSashEvent* self)
{
	wxRect* rct = new wxRect();
	*rct = self->GetDragRect();
	return rct;
}
	
EWXWEXPORT(void,wxSashEvent_SetDragStatus)(wxSashEvent* self,int status)
{
	self->SetDragStatus((wxSashDragStatus)status);
}
	
EWXWEXPORT(int,wxSashEvent_GetDragStatus)(wxSashEvent* self)
{
	return (int)self->GetDragStatus();
}
	

EWXWEXPORT(void*,wxSashLayoutWindow_Create)(wxWindow* _par,int _id,int _stl)
{
	return (void*)new wxSashLayoutWindow (_par, _id, wxDefaultPosition, wxDefaultSize, (long)_stl);
}

EWXWEXPORT(int,wxSashLayoutWindow_GetAlignment)(wxSashLayoutWindow* self)
{
	return (int)self->GetAlignment();
}
	
EWXWEXPORT(int,wxSashLayoutWindow_GetOrientation)(wxSashLayoutWindow* self)
{
	return (int)self->GetOrientation();
}
	
EWXWEXPORT(void,wxSashLayoutWindow_SetAlignment)(wxSashLayoutWindow* self,int align)
{
	self->SetAlignment((wxLayoutAlignment)align);
}
	
EWXWEXPORT(void,wxSashLayoutWindow_SetOrientation)(wxSashLayoutWindow* self,int orient)
{
	self->SetOrientation((wxLayoutOrientation)orient);
}
	
EWXWEXPORT(void,wxSashLayoutWindow_SetDefaultSize)(wxSashLayoutWindow* self,int w,int h)
{
	self->SetDefaultSize(wxSize(w, h));
}
	

EWXWEXPORT(void*,wxQueryLayoutInfoEvent_Create)(int id)
{
	return (void*)new wxQueryLayoutInfoEvent(id);
}

EWXWEXPORT(void,wxQueryLayoutInfoEvent_SetRequestedLength)(wxQueryLayoutInfoEvent* self,int length)
{
	self->SetRequestedLength(length);
}
	
EWXWEXPORT(int,wxQueryLayoutInfoEvent_GetRequestedLength)(wxQueryLayoutInfoEvent* self)
{
	return self->GetRequestedLength();
}
	
EWXWEXPORT(void,wxQueryLayoutInfoEvent_SetFlags)(wxQueryLayoutInfoEvent* self,int flags)
{
	self->SetFlags(flags);
}
	
EWXWEXPORT(int,wxQueryLayoutInfoEvent_GetFlags)(wxQueryLayoutInfoEvent* self)
{
	return self->GetFlags();
}
	
EWXWEXPORT(void,wxQueryLayoutInfoEvent_SetSize)(wxQueryLayoutInfoEvent* self,int w,int h)
{
	self->SetSize(wxSize(w, h));
}
	
EWXWEXPORT(wxSize*,wxQueryLayoutInfoEvent_GetSize)(wxQueryLayoutInfoEvent* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetSize();
	return sz;
}
	
EWXWEXPORT(void,wxQueryLayoutInfoEvent_SetOrientation)(wxQueryLayoutInfoEvent* self,int orient)
{
	self->SetOrientation((wxLayoutOrientation)orient);
}
	
EWXWEXPORT(int,wxQueryLayoutInfoEvent_GetOrientation)(wxQueryLayoutInfoEvent* self)
{
	return (int)self->GetOrientation();
}
	
EWXWEXPORT(void,wxQueryLayoutInfoEvent_SetAlignment)(wxQueryLayoutInfoEvent* self,int align)
{
	self->SetAlignment((wxLayoutAlignment)align);
}
	
EWXWEXPORT(int,wxQueryLayoutInfoEvent_GetAlignment)(wxQueryLayoutInfoEvent* self)
{
	return (int)self->GetAlignment();
}
	

EWXWEXPORT(void*,wxCalculateLayoutEvent_Create)(int id)
{
	return (void*)new wxCalculateLayoutEvent(id);
}

EWXWEXPORT(void,wxCalculateLayoutEvent_SetFlags)(wxCalculateLayoutEvent* self,int flags)
{
	self->SetFlags(flags);
}
	
EWXWEXPORT(int,wxCalculateLayoutEvent_GetFlags)(wxCalculateLayoutEvent* self)
{
	return self->GetFlags();
}
	
EWXWEXPORT(void, wxCalculateLayoutEvent_SetRect)(wxCalculateLayoutEvent* self, int x, int y , int w, int h)
{
	self->SetRect(wxRect(x, y, w, h));
}
	
EWXWEXPORT(wxRect*,wxCalculateLayoutEvent_GetRect)(wxCalculateLayoutEvent* self)
{
	wxRect* rct = new wxRect();
	*rct = self->GetRect();
	return rct;
}

EWXWEXPORT(void*,wxLayoutAlgorithm_Create)()
{
	return (void*)new wxLayoutAlgorithm();
}

EWXWEXPORT(void,wxLayoutAlgorithm_Delete)(wxLayoutAlgorithm* self)
{
	delete self;
}

EWXWEXPORT(bool,wxLayoutAlgorithm_LayoutMDIFrame)(wxLayoutAlgorithm* self,wxMDIParentFrame* frame,int x,int y,int w,int h,int use)
{
	wxRect* r = NULL;
	if (use) r = new wxRect(x, y, w, h);
	
	bool result = self->LayoutMDIFrame(frame, r);
	
	if (r) delete r;
	return result;
}
	
EWXWEXPORT(bool,wxLayoutAlgorithm_LayoutFrame)(wxLayoutAlgorithm* self,wxFrame* frame,wxWindow* mainWindow)
{
	return self->LayoutFrame(frame, mainWindow);
}
	
EWXWEXPORT(bool,wxLayoutAlgorithm_LayoutWindow)(wxLayoutAlgorithm* self,wxFrame* frame,wxWindow* mainWindow)
{
	return self->LayoutWindow(frame, mainWindow);
}
	
}
