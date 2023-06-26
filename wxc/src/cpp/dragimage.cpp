#include "wxc/wrapper.h"
#include <wx/dragimag.h>
#include <wx/generic/dragimgg.h>

extern "C" {
/*-----------------------------------------------------------------------------
  DragImage
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxDragImage*,wxDragImage_Create)(wxBitmap* image,int x,int y)
{
  return new wxDragImage(*image, wxNullCursor);
}

EWXWEXPORT(wxDragImage*,wxDragIcon)(wxIcon* icon,int x,int y)
{
  return new wxDragImage(*icon, wxNullCursor);
}

EWXWEXPORT(wxDragImage*,wxDragString)(wxString* text,int x,int y)
{
  return new wxDragImage(*text, wxNullCursor);
}

EWXWEXPORT(wxDragImage*,wxDragTreeItem)(wxTreeCtrl* treeCtrl,wxTreeItemId* id)
{
  return new wxDragImage(*treeCtrl,*id);
}

EWXWEXPORT(wxDragImage*,wxDragListItem)(wxListCtrl* listCtrl,long id)
{
  return new wxDragImage(*listCtrl, id);
}

EWXWEXPORT(wxGenericDragImage*,wxGenericDragImage_Create)(wxCursor* cursor)
{
  return new wxGenericDragImage(*cursor);
}

EWXWEXPORT(wxGenericDragImage*,wxGenericDragIcon)(wxIcon* icon)
{
  return new wxGenericDragImage(*icon, wxNullCursor);
}

EWXWEXPORT(wxGenericDragImage*,wxGenericDragString)(wxString* text)
{
  return new wxGenericDragImage(*text, wxNullCursor);
}

EWXWEXPORT(wxGenericDragImage*,wxGenericDragTreeItem)(wxTreeCtrl* treeCtrl,wxTreeItemId* id)
{
  return new wxGenericDragImage(*treeCtrl,*id);
}

EWXWEXPORT(wxGenericDragImage*,wxGenericDragListItem)(wxListCtrl* listCtrl,long id)
{
  return new wxGenericDragImage(*listCtrl, id);
}

EWXWEXPORT(void,wxDragImage_Delete)(wxDragImage* self)
{
  if (self) delete self;
}

EWXWEXPORT(bool,wxDragImage_BeginDragFullScreen)(wxDragImage* self,int x_pos,int y_pos,wxWindow* window,bool fullScreen,wxRect* rect)
{
  return self->BeginDrag(wxPoint(x_pos, y_pos), window, fullScreen, rect);
}

EWXWEXPORT(bool,wxDragImage_BeginDrag)(wxDragImage* self,int x,int y,wxWindow* window,wxWindow* boundingWindow)
{
  return self->BeginDrag(wxPoint(x, y), window, boundingWindow);
}

EWXWEXPORT(bool,wxGenericDragImage_DoDrawImage)(wxGenericDragImage* self,wxDC* dc,int x,int y)
{
  return self->DoDrawImage(*dc, wxPoint(x, y));
}

EWXWEXPORT(bool,wxDragImage_EndDrag)(wxDragImage* self)
{
  return self->EndDrag();
}

EWXWEXPORT(wxRect*,wxGenericDragImage_GetImageRect)(wxGenericDragImage* self,int x_pos,int y_pos)
{
  wxRect* r = new wxRect();
  *r = self->GetImageRect(wxPoint(x_pos, y_pos));
  return r;
}

EWXWEXPORT(bool,wxDragImage_Hide)(wxDragImage* self)
{
  return self->Hide();
}

EWXWEXPORT(bool,wxDragImage_Move)(wxDragImage* self,int x,int y)
{
  return self->Move(wxPoint(x, y));
}

EWXWEXPORT(bool,wxDragImage_Show)(wxDragImage* self)
{
  return self->Show();
}

EWXWEXPORT(bool,wxGenericDragImage_UpdateBackingFromWindow)(wxGenericDragImage* self,wxDC* windowDC,wxMemoryDC* destDC,int x,int y,int w,int h,int xdest,int ydest,int width,int height)
{
  return self->UpdateBackingFromWindow(*windowDC,*destDC,
                                       wxRect(x, y, w, h),
                                       wxRect(xdest, ydest, width, height));
}

}


