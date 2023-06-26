#include <wx/string.h>

#include "wxc/wrapper.h"
#include "wx/gizmos/dynamicsash.h"
#include "wx/gizmos/editlbox.h"
#include "wx/gizmos/ledctrl.h"
#include "wx/gizmos/multicell.h"
#include "wx/gizmos/splittree.h"

class ELJSCTreeControl : public wxRemotelyScrolledTreeCtrl
{
	private:
		TreeCompareFunc compare_func;
		void* EiffelObject;

	public:
	    ELJSCTreeControl(void* _obj,
		                 void* _cmp,
		                 wxWindow *parent,
		                 wxWindowID id = -1,
                         const wxPoint& pos = wxDefaultPosition,
                         const wxSize& size = wxDefaultSize,
                         long style = wxTR_HAS_BUTTONS | wxTR_LINES_AT_ROOT) :
		wxRemotelyScrolledTreeCtrl (parent, id, pos, size, style)
		{
			EiffelObject = _obj;
			compare_func = (TreeCompareFunc)_cmp;
		};

		virtual int OnCompareItems(const wxTreeItemId& item1, const wxTreeItemId& item2)
		{
			return EiffelObject ? compare_func (EiffelObject, (void*)&item1, (void*)&item2) : wxRemotelyScrolledTreeCtrl::OnCompareItems(item1, item2);
		}

};

extern "C"
{

EWXWEXPORT(void*,wxDynamicSashWindow_Create)(void* parent, int id, int x, int y, int w, int h, int style)
{
	return (void*) new wxDynamicSashWindow((wxWindow*)parent, (wxWindowID)id, wxPoint(x, y), wxSize(w, h), (long)style);
}
	
EWXWEXPORT(void,wxDynamicSashWindow_Delete)(void* _obj)
{
	delete (wxDynamicSashWindow*)_obj;
}

EWXWEXPORT(void*,wxDynamicSashWindow_GetHScrollBar)(void* _obj, void* child)
{
	return (void*)((wxDynamicSashWindow*)_obj)->GetHScrollBar((wxWindow*)child);
}
	
EWXWEXPORT(void*,wxDynamicSashWindow_GetVScrollBar)(void* _obj, void* child)
{
	return (void*)((wxDynamicSashWindow*)_obj)->GetVScrollBar((wxWindow*)child);
}
	
EWXWEXPORT(void*,wxEditableListBox_Create)(void* parent, int id, void* label, int x, int y, int w, int h, int style)
{
	return (void*) new wxEditableListBox((wxWindow*)parent, (wxWindowID)id, wxString::FromAscii((char*)label), wxPoint(x, y), wxSize(w, h), (long)style);
}
	
EWXWEXPORT(void,wxEditableListBox_SetStrings)(void* _obj, void* strings, int _n)
{
	wxArrayString list;

	for (int i = 0; i < _n; i++) {
		wxString s = wxString::FromAscii(((char**)strings)[i]);
		list.Add(s);
	}

	((wxEditableListBox*)_obj)->SetStrings(list);
}
	
EWXWEXPORT(int,wxEditableListBox_GetStrings)(void* _obj, void* _ref)
{
	wxArrayString list;
	((wxEditableListBox*)_obj)->GetStrings(list);
	
	if (_ref)
	{
		for (unsigned int i = 0; i < list.GetCount(); i++)
			((char**)_ref)[i] = strdup (list.Item(i).mb_str());
	}

	return list.GetCount();
}
	
EWXWEXPORT(void*,wxEditableListBox_GetListCtrl)(void* _obj)
{
	return (void*)((wxEditableListBox*)_obj)->GetListCtrl();
}
	
EWXWEXPORT(void*,wxEditableListBox_GetDelButton)(void* _obj)
{
	return (void*)((wxEditableListBox*)_obj)->GetDelButton();
}
	
EWXWEXPORT(void*,wxEditableListBox_GetNewButton)(void* _obj)
{
	return (void*)((wxEditableListBox*)_obj)->GetNewButton();
}
	
EWXWEXPORT(void*,wxEditableListBox_GetUpButton)(void* _obj)
{
	return (void*)((wxEditableListBox*)_obj)->GetUpButton();
}
	
EWXWEXPORT(void*,wxEditableListBox_GetDownButton)(void* _obj)
{
	return (void*)((wxEditableListBox*)_obj)->GetDownButton();
}
	
EWXWEXPORT(void*,wxEditableListBox_GetEditButton)(void* _obj)
{
	return (void*)((wxEditableListBox*)_obj)->GetEditButton();
}
	
EWXWEXPORT(void*,wxLEDNumberCtrl_Create)(void* parent, int id, int x, int y, int w, int h, int style)
{
	return (void*) new wxLEDNumberCtrl((wxWindow*)parent, (wxWindowID)id, wxPoint(x, y), wxSize(w, h), (long)style);
}
	
EWXWEXPORT(int,wxLEDNumberCtrl_GetAlignment)(void* _obj)
{
	return (int)((wxLEDNumberCtrl*)_obj)->GetAlignment();
}
	
EWXWEXPORT(int,wxLEDNumberCtrl_GetDrawFaded)(void* _obj)
{
	return (int)((wxLEDNumberCtrl*)_obj)->GetDrawFaded();
}
	
EWXWEXPORT(int,wxLEDNumberCtrl_GetValue)(void* _obj, void* _ref)
{
	wxString res = ((wxLEDNumberCtrl*)_obj)->GetValue();
        return copyStrToBuf(_ref, res);
}
	
EWXWEXPORT(void,wxLEDNumberCtrl_SetAlignment)(void* _obj, int Alignment, int Redraw)
{
	((wxLEDNumberCtrl*)_obj)->SetAlignment((wxLEDValueAlign)Alignment, Redraw != 0);
}
	
EWXWEXPORT(void,wxLEDNumberCtrl_SetDrawFaded)(void* _obj, int DrawFaded, int Redraw)
{
	((wxLEDNumberCtrl*)_obj)->SetDrawFaded(DrawFaded != 0, Redraw != 0);
}
	
EWXWEXPORT(void,wxLEDNumberCtrl_SetValue)(void* _obj, void* Value, int Redraw)
{
	((wxLEDNumberCtrl*)_obj)->SetValue(wxString::FromAscii((char*)Value), Redraw != 0);
}
	
EWXWEXPORT(void*,wxMultiCellItemHandle_Create)(int row, int column, int height, int width, int sx, int sy, int style, int wx, int wy, int align)
{
	return (void*) new wxMultiCellItemHandle(row, column, height, width, wxSize(sx, sy), (wxResizable)style, wxSize(wx, wy), align);
}
	
EWXWEXPORT(void*,wxMultiCellItemHandle_CreateWithSize)(void* _obj, int row, int column, int sx, int sy, int style, int wx, int wy, int align)
{
	return (void*) new wxMultiCellItemHandle(row, column, wxSize(sx, sy), (wxResizable)style, wxSize(wx, wy), align);
}
	
EWXWEXPORT(void*,wxMultiCellItemHandle_CreateWithStyle)(void* _obj, int row, int column, int style, int wx, int wy, int align)
{
	return (void*) new wxMultiCellItemHandle(row, column, (wxResizable)style, wxSize(wx, wy), align);
}
	
EWXWEXPORT(int,wxMultiCellItemHandle_GetColumn)(void* _obj)
{
	return ((wxMultiCellItemHandle*)_obj)->GetColumn();
}
	
EWXWEXPORT(int,wxMultiCellItemHandle_GetRow)(void* _obj)
{
	return ((wxMultiCellItemHandle*)_obj)->GetRow();
}
	
EWXWEXPORT(int,wxMultiCellItemHandle_GetWidth)(void* _obj)
{
	return ((wxMultiCellItemHandle*)_obj)->GetWidth();
}
	
EWXWEXPORT(int,wxMultiCellItemHandle_GetHeight)(void* _obj)
{
	return ((wxMultiCellItemHandle*)_obj)->GetHeight();
}
	
EWXWEXPORT(int,wxMultiCellItemHandle_GetStyle)(void* _obj)
{
	return (int)((wxMultiCellItemHandle*)_obj)->GetStyle();
}
	
EWXWEXPORT(void,wxMultiCellItemHandle_GetLocalSize)(void* _obj, void* w, void* h)
{
	wxSize size = ((wxMultiCellItemHandle*)_obj)->GetLocalSize();
	*((int*)w) = size.x;
	*((int*)h) = size.y;
}
	
EWXWEXPORT(int,wxMultiCellItemHandle_GetAlignment)(void* _obj)
{
	return ((wxMultiCellItemHandle*)_obj)->GetAlignment();
}
	
EWXWEXPORT(void,wxMultiCellItemHandle_GetWeight)(void* _obj, void* w, void* h)
{
	wxSize size = ((wxMultiCellItemHandle*)_obj)->GetWeight();
	*((int*)w) = size.x;
	*((int*)h) = size.y;
}
	
EWXWEXPORT(void*,wxMultiCellSizer_Create)(int rows, int cols)
{
	return (void*) new wxMultiCellSizer(rows, cols);
}
	
EWXWEXPORT(void,wxMultiCellSizer_Delete)(void* _obj)
{
	delete (wxMultiCellSizer*)_obj;
}

EWXWEXPORT(void,wxMultiCellSizer_RecalcSizes)(void* _obj)
{
	((wxMultiCellSizer*)_obj)->RecalcSizes();
}
	
EWXWEXPORT(void,wxMultiCellSizer_CalcMin)(void* _obj, void* w, void* h)
{
	wxSize size = ((wxMultiCellSizer*)_obj)->CalcMin();
	*((int*)w) = size.x;
	*((int*)h) = size.y;
}
	
EWXWEXPORT(int,wxMultiCellSizer_SetDefaultCellSize)(void* _obj, int w, int h)
{
	return (int)((wxMultiCellSizer*)_obj)->SetDefaultCellSize(wxSize(w, h));
}
	
EWXWEXPORT(int,wxMultiCellSizer_SetColumnWidth)(void* _obj, int column, int colSize, int expandable)
{
	return (int)((wxMultiCellSizer*)_obj)->SetColumnWidth(column, colSize, expandable != 0);
}
	
EWXWEXPORT(int,wxMultiCellSizer_SetRowHeight)(void* _obj, int row, int rowSize, int expandable)
{
	return (int)((wxMultiCellSizer*)_obj)->SetRowHeight(row, rowSize, expandable != 0);
}
	
EWXWEXPORT(int,wxMultiCellSizer_EnableGridLines)(void* _obj, void* win)
{
	return (int)((wxMultiCellSizer*)_obj)->EnableGridLines((wxWindow*)win);
}
	
EWXWEXPORT(int,wxMultiCellSizer_SetGridPen)(void* _obj, void* pen)
{
	return (int)((wxMultiCellSizer*)_obj)->SetGridPen((wxPen*)pen);
}
	
EWXWEXPORT(void*,wxMultiCellCanvas_Create)(void* parent, int numRows, int numCols)
{
	return (void*) new wxMultiCellCanvas((wxWindow*)parent, numRows, numCols);
}
	
EWXWEXPORT(void,wxMultiCellCanvas_Add)(void* _obj, void* win, int row, int col)
{
	((wxMultiCellCanvas*)_obj)->Add((wxWindow*)win, (unsigned int)row, (unsigned int)col);
}
	
EWXWEXPORT(int,wxMultiCellCanvas_MaxRows)(void* _obj)
{
	return ((wxMultiCellCanvas*)_obj)->MaxRows();
}
	
EWXWEXPORT(int,wxMultiCellCanvas_MaxCols)(void* _obj)
{
	return ((wxMultiCellCanvas*)_obj)->MaxCols();
}
	
EWXWEXPORT(void,wxMultiCellCanvas_CalculateConstraints)(void* _obj)
{
	((wxMultiCellCanvas*)_obj)->CalculateConstraints();
}
	
EWXWEXPORT(void,wxMultiCellCanvas_SetMinCellSize)(void* _obj, int w, int h)
{
	((wxMultiCellCanvas*)_obj)->SetMinCellSize(wxSize(w, h));
}
	
EWXWEXPORT(void*,wxSplitterScrolledWindow_Create)(void* parent, int id, int x, int y, int w, int h, int style)
{
	return (void*) new wxSplitterScrolledWindow((wxWindow*)parent, (wxWindowID)id, wxPoint(x, y), wxSize(w, h), (long)style);
}
	
EWXWEXPORT(void*,wxThinSplitterWindow_Create)(void* parent, int id, int x, int y, int w, int h, int style)
{
	return (void*) new wxThinSplitterWindow((wxWindow*)parent, (wxWindowID)id, wxPoint(x, y), wxSize(w, h), (long)style);
}
	
EWXWEXPORT(void,wxThinSplitterWindow_SizeWindows)(void* _obj)
{
	((wxThinSplitterWindow*)_obj)->SizeWindows();
}
	
EWXWEXPORT(int,wxThinSplitterWindow_SashHitTest)(void* _obj, int x, int y, int tolerance)
{
	return (int)((wxThinSplitterWindow*)_obj)->SashHitTest(x, y, tolerance);
}
	
EWXWEXPORT(void,wxThinSplitterWindow_DrawSash)(void* _obj, void* dc)
{
	((wxThinSplitterWindow*)_obj)->DrawSash(*((wxDC*)dc));
}
	
EWXWEXPORT(void*,wxTreeCompanionWindow_Create)(void* parent, int id, int x, int y, int w, int h, int style)
{
	return (void*) new wxTreeCompanionWindow((wxWindow*)parent, (wxWindowID)id, wxPoint(x, y), wxSize(w, h), (long)style);
}
	
EWXWEXPORT(void,wxTreeCompanionWindow_DrawItem)(void* _obj, void* dc, void* id, int x, int y, int w, int h)
{
	((wxTreeCompanionWindow*)_obj)->DrawItem(*((wxDC*)dc), *((wxTreeItemId*)id), wxRect(x, y, w, h));
}
	
EWXWEXPORT(void*,wxTreeCompanionWindow_GetTreeCtrl)(void* _obj)
{
	return (void*)((wxTreeCompanionWindow*)_obj)->GetTreeCtrl();
}
	
EWXWEXPORT(void,wxTreeCompanionWindow_SetTreeCtrl)(void* _obj, void* treeCtrl)
{
	((wxTreeCompanionWindow*)_obj)->SetTreeCtrl((wxRemotelyScrolledTreeCtrl*)treeCtrl);
}
	
EWXWEXPORT(void*,wxRemotelyScrolledTreeCtrl_Create)(void* _obj, void* _cmp, void* parent, int id, int x, int y, int w, int h, int style)
{
	return (void*) new ELJSCTreeControl(_obj, _cmp, (wxWindow*)parent, (wxWindowID)id, wxPoint(x, y), wxSize(w, h), (long)style);
}
	
EWXWEXPORT(void,wxRemotelyScrolledTreeCtrl_Delete)(void* _obj)
{
	delete (ELJSCTreeControl*)_obj;
}

EWXWEXPORT(void,wxRemotelyScrolledTreeCtrl_SetScrollbars)(void* _obj, int pixelsPerUnitX, int pixelsPerUnitY, int noUnitsX, int noUnitsY, int xPos, int yPos, int noRefresh)
{
	((ELJSCTreeControl*)_obj)->SetScrollbars(pixelsPerUnitX, pixelsPerUnitY, noUnitsX, noUnitsY, xPos, yPos, noRefresh != 0);
}
	
EWXWEXPORT(void,wxRemotelyScrolledTreeCtrl_GetViewStart)(void* _obj, void* x, void* y)
{
	((ELJSCTreeControl*)_obj)->GetViewStart((int*)x, (int*)y);
}
	
EWXWEXPORT(void,wxRemotelyScrolledTreeCtrl_PrepareDC)(void* _obj, void* dc)
{
	((ELJSCTreeControl*)_obj)->PrepareDC(*((wxDC*)dc));
}
	
EWXWEXPORT(int,wxRemotelyScrolledTreeCtrl_GetScrollPos)(void* _obj, int orient)
{
	return ((ELJSCTreeControl*)_obj)->GetScrollPos(orient);
}
	
EWXWEXPORT(void,wxRemotelyScrolledTreeCtrl_HideVScrollbar)(void* _obj)
{
	((ELJSCTreeControl*)_obj)->HideVScrollbar();
}
	
EWXWEXPORT(void,wxRemotelyScrolledTreeCtrl_CalcTreeSize)(void* _obj, void* x, void* y, void* w, void* h)
{
	wxRect rect;
	((ELJSCTreeControl*)_obj)->CalcTreeSize(rect);
	*((int*)x) = rect.x;
	*((int*)y) = rect.y;
	*((int*)w) = rect.width;
	*((int*)h) = rect.height;
}
	
EWXWEXPORT(void,wxRemotelyScrolledTreeCtrl_CalcTreeSizeItem)(void* _obj, void* id, void* x, void* y, void* w, void* h)
{
	wxRect rect;
	((ELJSCTreeControl*)_obj)->CalcTreeSize(*((wxTreeItemId*)id), rect);
	*((int*)x) = rect.x;
	*((int*)y) = rect.y;
	*((int*)w) = rect.width;
	*((int*)h) = rect.height;
}
	
EWXWEXPORT(void,wxRemotelyScrolledTreeCtrl_AdjustRemoteScrollbars)(void* _obj)
{
	((ELJSCTreeControl*)_obj)->AdjustRemoteScrollbars();
}
	
EWXWEXPORT(void*,wxRemotelyScrolledTreeCtrl_GetScrolledWindow)(void* _obj)
{
	return (void*)((ELJSCTreeControl*)_obj)->GetScrolledWindow();
}
	
EWXWEXPORT(void,wxRemotelyScrolledTreeCtrl_ScrollToLine)(void* _obj, int posHoriz, int posVert)
{
	((ELJSCTreeControl*)_obj)->ScrollToLine(posHoriz, posVert);
}
	
EWXWEXPORT(void,wxRemotelyScrolledTreeCtrl_SetCompanionWindow)(void* _obj, void* companion)
{
	((ELJSCTreeControl*)_obj)->SetCompanionWindow((wxWindow*)companion);
}
	
EWXWEXPORT(void*,wxRemotelyScrolledTreeCtrl_GetCompanionWindow)(void* _obj)
{
	return (void*)((ELJSCTreeControl*)_obj)->GetCompanionWindow();
}
	
}
