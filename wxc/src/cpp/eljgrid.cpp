#include "wrapper.h"
#include "eljgrid.h"

extern "C"
{

EWXWEXPORT(wxGridCellCoordsArray*,wxGridCellCoordsArray_Create)()
{
	return new wxGridCellCoordsArray();
}

EWXWEXPORT(void,wxGridCellCoordsArray_Delete)(wxGridCellCoordsArray* self)
{
	delete self;
}

EWXWEXPORT(int,wxGridCellCoordsArray_GetCount)(wxGridCellCoordsArray* self)
{
	return self->GetCount();
}

EWXWEXPORT(void,wxGridCellCoordsArray_Item)(wxGridCellCoordsArray* self,int _idx,int* _c,int* _r)
{
	*_c = self->Item(_idx).GetCol();
	*_r = self->Item(_idx).GetRow();
}


EWXWEXPORT(bool,wxGridCellEditor_IsCreated)(wxGridCellEditor* self)
{
	return self->IsCreated();
}
	
EWXWEXPORT(void*,wxGridCellEditor_GetControl)(wxGridCellEditor* self)
{
	return (void*)self->GetControl();
}
	
EWXWEXPORT(void,wxGridCellEditor_SetControl)(wxGridCellEditor* self,wxControl* control)
{
	self->SetControl(control);
}
	
EWXWEXPORT(void,wxGridCellEditor_Create)(wxGridCellEditor* self,wxWindow* parent,int id,wxEvtHandler* evtHandler)
{
	self->Create(parent, (wxWindowID) id, evtHandler);
}
	
EWXWEXPORT(void,wxGridCellEditor_SetSize)(wxGridCellEditor* self,int x,int y,int w,int h)
{
	self->SetSize(wxRect(x, y, w, h));
}
	
EWXWEXPORT(void,wxGridCellEditor_Show)(wxGridCellEditor* self,bool show,void* attr)
{
	self->Show(show, (wxGridCellAttr*)attr);
}
	
#if (wxVERSION_NUMBER >= 2905)
EWXWEXPORT(void,wxGridCellEditor_PaintBackground)(wxGridCellEditor* self,wxDC* dc,int x,int y,int w,int h,wxGridCellAttr* attr)
{
	self->PaintBackground(*dc, wxRect(x, y, w, h), *attr);
}
#else
EWXWEXPORT(void,wxGridCellEditor_PaintBackground)(wxGridCellEditor* self,int x,int y,int w,int h,void* attr)
{
	self->PaintBackground(wxRect(x, y, w, h), (wxGridCellAttr*)attr);
}
#endif
	
EWXWEXPORT(void,wxGridCellEditor_BeginEdit)(wxGridCellEditor* self,int row,int col,void* grid)
{
	self->BeginEdit(row, col, (wxGrid*)grid);
}

EWXWEXPORT(bool,wxGridCellEditor_EndEdit)(wxGridCellEditor* self,int row,int col,wxGrid* grid, wxString* oldCell, wxString* newCell)
{
#if (wxVERSION_NUMBER < 2900)
  return self->EndEdit(row, col,  grid);
#else
  return self->EndEdit(row, col, grid, *oldCell, newCell);
#endif
}

EWXWEXPORT(void,wxGridCellEditor_Reset)(wxGridCellEditor* self)
{
	self->Reset();
}
	
EWXWEXPORT(bool,wxGridCellEditor_IsAcceptedKey)(wxGridCellEditor* self,wxKeyEvent* event)
{
	return self->IsAcceptedKey(*event);
}
	
EWXWEXPORT(void,wxGridCellEditor_StartingKey)(wxGridCellEditor* self,wxKeyEvent* event)
{
	self->StartingKey(*event);
}
	
EWXWEXPORT(void,wxGridCellEditor_StartingClick)(wxGridCellEditor* self)
{
	self->StartingClick();
}
	
EWXWEXPORT(void,wxGridCellEditor_HandleReturn)(wxGridCellEditor* self,wxKeyEvent* event)
{
	self->HandleReturn(*event);
}
	
EWXWEXPORT(void,wxGridCellEditor_Destroy)(wxGridCellEditor* self)
{
	self->Destroy();
}
	
EWXWEXPORT(void,wxGridCellEditor_SetParameters)(wxGridCellEditor* self,wxString* params)
{
	self->SetParameters(*params);
}
	
EWXWEXPORT(void*,wxGridCellTextEditor_Ctor)()
{
	return (void*)new wxGridCellTextEditor();
}

EWXWEXPORT(void*,wxGridCellNumberEditor_Ctor)(int min,int max)
{
	return (void*)new wxGridCellNumberEditor(min, max);
}

EWXWEXPORT(void*,wxGridCellFloatEditor_Ctor)(int width,int precision)
{
	return (void*)new wxGridCellFloatEditor(width, precision);
}

EWXWEXPORT(void*,wxGridCellBoolEditor_Ctor)()
{
	return (void*)new wxGridCellBoolEditor();
}

EWXWEXPORT(void*,wxGridCellChoiceEditor_Ctor)(int count,void* choices,bool allowOthers)
{
	wxString items[256];

	for (int i = 0; i < count; i++)
		items[i] = ((wxChar**)choices)[i];

	return (void*)new wxGridCellChoiceEditor (count, items, allowOthers);
}

EWXWEXPORT(void*,wxGridCellNumberRenderer_Ctor)()
{
	return (void*)new wxGridCellNumberRenderer();
}

EWXWEXPORT(void*,wxGridCellAutoWrapStringRenderer_Ctor)()
{
	return (void*)new wxGridCellAutoWrapStringRenderer();
}


EWXWEXPORT(void*,wxGridCellAttr_Ctor)()
{
	return (void*)new wxGridCellAttr();
}

EWXWEXPORT(void,wxGridCellAttr_IncRef)(wxGridCellAttr* self)
{
	self->IncRef();
}
	
EWXWEXPORT(void,wxGridCellAttr_DecRef)(wxGridCellAttr* self)
{
	self->DecRef();
}
	
EWXWEXPORT(void,wxGridCellAttr_SetTextColour)(wxGridCellAttr* self,wxColour* colText)
{
	self->SetTextColour(*colText);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetBackgroundColour)(wxGridCellAttr* self,wxColour* colBack)
{
	self->SetBackgroundColour(*colBack);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetFont)(wxGridCellAttr* self,wxFont* font)
{
	self->SetFont(*font);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetAlignment)(wxGridCellAttr* self,int hAlign,int vAlign)
{
	self->SetAlignment(hAlign, vAlign);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetReadOnly)(wxGridCellAttr* self,bool isReadOnly)
{
	self->SetReadOnly(isReadOnly);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetRenderer)(wxGridCellAttr* self,void* renderer)
{
	self->SetRenderer((wxGridCellRenderer*)renderer);
}
	
EWXWEXPORT(void,wxGridCellAttr_SetEditor)(wxGridCellAttr* self,wxGridCellEditor* editor)
{
	self->SetEditor(editor);
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasTextColour)(wxGridCellAttr* self)
{
	return self->HasTextColour();
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasBackgroundColour)(wxGridCellAttr* self)
{
	return self->HasBackgroundColour();
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasFont)(wxGridCellAttr* self)
{
	return self->HasFont();
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasAlignment)(wxGridCellAttr* self)
{
	return self->HasAlignment();
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasRenderer)(wxGridCellAttr* self)
{
	return self->HasRenderer();
}
	
EWXWEXPORT(bool,wxGridCellAttr_HasEditor)(wxGridCellAttr* self)
{
	return self->HasEditor();
}
	
EWXWEXPORT(void,wxGridCellAttr_GetTextColour)(wxGridCellAttr* self,wxColour* colour)
{
	*colour = self->GetTextColour();
}
	
EWXWEXPORT(void,wxGridCellAttr_GetBackgroundColour)(wxGridCellAttr* self,wxColour* colour)
{
	*colour = self->GetBackgroundColour();
}
	
EWXWEXPORT(void,wxGridCellAttr_GetFont)(wxGridCellAttr* self,wxFont* font)
{
	*font = self->GetFont();
}
	
EWXWEXPORT(void,wxGridCellAttr_GetAlignment)(wxGridCellAttr* self,int* hAlign,int* vAlign)
{
	self->GetAlignment(hAlign, vAlign);
}
	
EWXWEXPORT(void*,wxGridCellAttr_GetRenderer)(wxGridCellAttr* self,wxGrid* grid,int row,int col)
{
	return (void*)self->GetRenderer(grid, row, col);
}
	
EWXWEXPORT(void*,wxGridCellAttr_GetEditor)(wxGridCellAttr* self,wxGrid* grid,int row,int col)
{
	return (void*)self->GetEditor(grid, row, col);
}
	
EWXWEXPORT(bool,wxGridCellAttr_IsReadOnly)(wxGridCellAttr* self)
{
	return self->IsReadOnly();
}
	
EWXWEXPORT(void,wxGridCellAttr_SetDefAttr)(wxGridCellAttr* self,wxGridCellAttr* defAttr)
{
	self->SetDefAttr(defAttr);
}
	
EWXWEXPORT(void*,wxGrid_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*)new wxGrid (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl | wxWANTS_CHARS);
}

EWXWEXPORT(void,wxGrid_CreateGrid)(wxGrid* self,int rows,int cols,int selmode)
{
	self->CreateGrid (rows, cols, (wxGrid::wxGridSelectionModes)selmode);
}

EWXWEXPORT(void,wxGrid_SetSelectionMode)(wxGrid* self,int selmode)
{
	self->SetSelectionMode((wxGrid::wxGridSelectionModes) selmode);
}
	
EWXWEXPORT(int,wxGrid_GetNumberRows)(wxGrid* self)
{
	return self->GetNumberRows();
}
	
EWXWEXPORT(int,wxGrid_GetNumberCols)(wxGrid* self)
{
	return self->GetNumberCols();
}
	
EWXWEXPORT(bool,wxGrid_ProcessTableMessage)(wxGrid* self,wxGridTableMessage* evt)
{
	return self->ProcessTableMessage(*evt);
}

EWXWEXPORT(void*,wxGrid_GetTable)(wxGrid* self)
{
	return (void*)self->GetTable();
}
	
EWXWEXPORT(bool,wxGrid_SetTable)(wxGrid* self,wxGridTableBase* table,bool takeOwnership,int selmode)
{
	return self->SetTable(table, takeOwnership , (wxGrid::wxGridSelectionModes) selmode);
}
	
EWXWEXPORT(void,wxGrid_ClearGrid)(wxGrid* self)
{
	self->ClearGrid();
}
	
EWXWEXPORT(bool,wxGrid_InsertRows)(wxGrid* self,int pos,int numRows,bool updateLabels)
{
	return self->InsertRows(pos, numRows, updateLabels);
}
	
EWXWEXPORT(bool,wxGrid_AppendRows)(wxGrid* self,int numRows,bool updateLabels)
{
	return self->AppendRows(numRows, updateLabels);
}
	
EWXWEXPORT(bool,wxGrid_DeleteRows)(wxGrid* self,int pos,int numRows,bool updateLabels)
{
	return self->DeleteRows(pos, numRows, updateLabels);
}
	
EWXWEXPORT(bool,wxGrid_InsertCols)(wxGrid* self,int pos,int numCols,bool updateLabels)
{
	return self->InsertCols(pos, numCols, updateLabels);
}
	
EWXWEXPORT(bool,wxGrid_AppendCols)(wxGrid* self,int numCols,int updateLabels)
{
	return self->AppendCols( numCols, updateLabels);
}
	
EWXWEXPORT(bool,wxGrid_DeleteCols)(wxGrid* self,int pos,int numCols,bool updateLabels)
{
	return self->DeleteCols(pos, numCols, updateLabels);
}
	
EWXWEXPORT(void,wxGrid_DrawGridSpace)(wxGrid* self,wxDC* dc)
{
	self->DrawGridSpace(*dc);
}
	
EWXWEXPORT(void,wxGrid_DrawCellBorder)(wxGrid* self,wxDC* dc,int _row,int _col)
{
	self->DrawCellBorder(*dc, wxGridCellCoords(_row, _col));
}
	
EWXWEXPORT(void,wxGrid_DrawAllGridLines)(wxGrid* self,wxDC* dc,void* reg)
{
	self->DrawAllGridLines(*dc,*((wxRegion*)reg));
}
	
EWXWEXPORT(void,wxGrid_DrawCell)(wxGrid* self,wxDC* dc,int _row,int _col)
{
	self->DrawCell(*dc, wxGridCellCoords(_row, _col));
}
	
EWXWEXPORT(void,wxGrid_DrawCellHighlight)(wxGrid* self,wxDC* dc,void* attr)
{
	self->DrawCellHighlight(*dc, (const wxGridCellAttr*)attr);
}
	
EWXWEXPORT(void,wxGrid_DrawRowLabels)(wxGrid* self,wxDC* dc)
{
#if wxVERSION_NUMBER >= 2400
	wxArrayInt arr;
	self->DrawRowLabels(*dc, arr);
#else
	self->DrawRowLabels(*dc);
#endif
}
	
EWXWEXPORT(void,wxGrid_DrawRowLabel)(wxGrid* self,wxDC* dc,int row)
{
	self->DrawRowLabel(*dc, row);
}
	
EWXWEXPORT(void,wxGrid_DrawColLabels)(wxGrid* self,wxDC* dc)
{
#if wxVERSION_NUMBER >= 2400
	wxArrayInt arr;
	self->DrawColLabels(*dc, arr);
#else
	self->DrawColLabels(*dc);
#endif
}
	
EWXWEXPORT(void,wxGrid_DrawColLabel)(wxGrid* self,wxDC* dc,int col)
{
	self->DrawColLabel(*dc, col);
}
	
EWXWEXPORT(void,wxGrid_DrawTextRectangle)(wxGrid* self,wxDC* dc,wxString* txt,int x,int y,int w,int h,int horizontalAlignment,int verticalAlignment)
{
	self->DrawTextRectangle(*dc,*txt, wxRect(x, y, w, h), horizontalAlignment, verticalAlignment);
}
	
EWXWEXPORT(int,wxGrid_StringToLines)(wxGrid* self,wxString* value,void* lines)
{
	int result = 0;
	wxArrayString arr;
	
	self->StringToLines(*value, arr);
	
	result = arr.GetCount();
	
	if (lines)
	{
		for (int i = 0; i < result; i++)
			((const wxChar**)lines)[i] = wxStrdup (arr[i].wchar_str());
	}
	return result;
}
	
EWXWEXPORT(void,wxGrid_GetTextBoxSize)(wxGrid* self,wxDC* dc,int count,void* lines,void* width,void* height)
{
	wxArrayString arr;

	for (int i = 0; i < count; i++)
		arr[i] = ((wxChar**)lines)[i];

	self->GetTextBoxSize(*dc, arr, (long*)width, (long*)height);
}
	
EWXWEXPORT(void,wxGrid_BeginBatch)(wxGrid* self)
{
	self->BeginBatch();
}
	
EWXWEXPORT(void,wxGrid_EndBatch)(wxGrid* self)
{
	self->EndBatch();
}
	
EWXWEXPORT(int,wxGrid_GetBatchCount)(wxGrid* self)
{
	return self->GetBatchCount();
}
	
EWXWEXPORT(bool,wxGrid_IsEditable)(wxGrid* self)
{
	return self->IsEditable();
}
	
EWXWEXPORT(void,wxGrid_EnableEditing)(wxGrid* self,bool edit)
{
	self->EnableEditing(edit);
}
	
EWXWEXPORT(void,wxGrid_EnableCellEditControl)(wxGrid* self,bool enable)
{
	self->EnableCellEditControl(enable);
}
	
EWXWEXPORT(void,wxGrid_DisableCellEditControl)(wxGrid* self)
{
	self->DisableCellEditControl();
}
	
EWXWEXPORT(bool,wxGrid_CanEnableCellControl)(wxGrid* self)
{
	return self->CanEnableCellControl();
}
	
EWXWEXPORT(bool,wxGrid_IsCellEditControlEnabled)(wxGrid* self)
{
	return self->IsCellEditControlEnabled();
}
	
EWXWEXPORT(bool,wxGrid_IsCellEditControlShown)(wxGrid* self)
{
	return self->IsCellEditControlShown();
}
	
EWXWEXPORT(bool,wxGrid_IsCurrentCellReadOnly)(wxGrid* self)
{
	return self->IsCurrentCellReadOnly();
}
	
EWXWEXPORT(void,wxGrid_ShowCellEditControl)(wxGrid* self)
{
	self->ShowCellEditControl();
}
	
EWXWEXPORT(void,wxGrid_HideCellEditControl)(wxGrid* self)
{
	self->HideCellEditControl();
}
	
EWXWEXPORT(void,wxGrid_SaveEditControlValue)(wxGrid* self)
{
	self->SaveEditControlValue();
}
	
EWXWEXPORT(void,wxGrid_XYToCell)(wxGrid* self,int x,int y,int* r,int* c)
{
	wxGridCellCoords cds;
	self->XYToCell(x, y, cds);
	*r = cds.GetRow();
	*c = cds.GetCol();
}
	
EWXWEXPORT(int,wxGrid_YToRow)(wxGrid* self,int y)
{
	return self->YToRow(y);
}
	
EWXWEXPORT(int,wxGrid_XToCol)(wxGrid* self,int x)
{
	return self->XToCol(x);
}
	
EWXWEXPORT(int,wxGrid_YToEdgeOfRow)(wxGrid* self,int y)
{
	return self->YToEdgeOfRow(y);
}
	
EWXWEXPORT(int,wxGrid_XToEdgeOfCol)(wxGrid* self,int x)
{
	return self->XToEdgeOfCol(x);
}
	
EWXWEXPORT(wxRect*,wxGrid_CellToRect)(wxGrid* self,int top,int left,int bottom,int right)
{
	wxRect* rct = new wxRect();
	*rct = self->BlockToDeviceRect(wxGridCellCoords(top, left), wxGridCellCoords(bottom, right));
	return rct;
}
	
EWXWEXPORT(int,wxGrid_GetGridCursorRow)(wxGrid* self)
{
	return self->GetGridCursorRow();
}
	
EWXWEXPORT(int,wxGrid_GetGridCursorCol)(wxGrid* self)
{
	return self->GetGridCursorCol();
}
	
EWXWEXPORT(bool,wxGrid_IsVisible)(wxGrid* self,int row,int col,bool wholeCellVisible)
{
	return self->IsVisible(row, col, wholeCellVisible);
}
	
EWXWEXPORT(void,wxGrid_MakeCellVisible)(wxGrid* self,int row,int col)
{
	self->MakeCellVisible(row, col);
}
	
EWXWEXPORT(void,wxGrid_SetGridCursor)(wxGrid* self,int row,int col)
{
	self->SetGridCursor(row, col);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorUp)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorUp(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorDown)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorDown(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorLeft)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorLeft(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorRight)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorRight(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MovePageDown)(wxGrid* self)
{
	return self->MovePageDown();
}
	
EWXWEXPORT(bool,wxGrid_MovePageUp)(wxGrid* self)
{
	return self->MovePageUp();
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorUpBlock)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorUpBlock(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorDownBlock)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorDownBlock(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorLeftBlock)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorLeftBlock(expandSelection);
}
	
EWXWEXPORT(bool,wxGrid_MoveCursorRightBlock)(wxGrid* self,bool expandSelection)
{
	return self->MoveCursorRightBlock(expandSelection);
}
	
EWXWEXPORT(int,wxGrid_GetDefaultRowLabelSize)(wxGrid* self)
{
	return self->GetDefaultRowLabelSize();
}
	
EWXWEXPORT(int,wxGrid_GetRowLabelSize)(wxGrid* self)
{
	return self->GetRowLabelSize();
}
	
EWXWEXPORT(int,wxGrid_GetDefaultColLabelSize)(wxGrid* self)
{
	return self->GetDefaultColLabelSize();
}
	
EWXWEXPORT(int,wxGrid_GetColLabelSize)(wxGrid* self)
{
	return self->GetColLabelSize();
}
	
EWXWEXPORT(void,wxGrid_GetLabelBackgroundColour)(wxGrid* self,wxColour* colour)
{
	*colour = self->GetLabelBackgroundColour();
}
	
EWXWEXPORT(void,wxGrid_GetLabelTextColour)(wxGrid* self,wxColour* colour)
{
	*colour = self->GetLabelTextColour();
}
	
EWXWEXPORT(void,wxGrid_GetLabelFont)(wxGrid* self,wxFont* font)
{
	*font = self->GetLabelFont();
}
	
EWXWEXPORT(void,wxGrid_GetRowLabelAlignment)(wxGrid* self,int* horiz,int* vert)
{
	self->GetRowLabelAlignment(horiz,vert);
}
	
EWXWEXPORT(void,wxGrid_GetColLabelAlignment)(wxGrid* self,int* horiz,int* vert)
{
	self->GetColLabelAlignment(horiz,vert);
}
	
EWXWEXPORT(wxString*,wxGrid_GetRowLabelValue)(wxGrid* self,int row)
{
	wxString *result = new wxString();
	*result = self->GetRowLabelValue(row);
	return result;
}
	
EWXWEXPORT(wxString*,wxGrid_GetColLabelValue)(wxGrid* self,int col)
{
	wxString *result = new wxString();
	*result = self->GetColLabelValue(col);
	return result;
}
	
EWXWEXPORT(void,wxGrid_GetGridLineColour)(wxGrid* self,wxColour* colour)
{
	*colour = self->GetGridLineColour();
}
	
EWXWEXPORT(void,wxGrid_GetCellHighlightColour)(wxGrid* self,wxColour* colour)
{
	*colour = self->GetCellHighlightColour();
}
	
EWXWEXPORT(void,wxGrid_SetRowLabelSize)(wxGrid* self,int width)
{
	self->SetRowLabelSize(width);
}
	
EWXWEXPORT(void,wxGrid_SetColLabelSize)(wxGrid* self,int height)
{
	self->SetColLabelSize(height);
}
	
EWXWEXPORT(void,wxGrid_SetLabelBackgroundColour)(wxGrid* self,wxColour* colour)
{
	self->SetLabelBackgroundColour(*colour);
}
	
EWXWEXPORT(void,wxGrid_SetLabelTextColour)(wxGrid* self,wxColour* colour)
{
	self->SetLabelTextColour(*colour);
}
	
EWXWEXPORT(void,wxGrid_SetLabelFont)(wxGrid* self,wxFont* font)
{
	self->SetLabelFont(*font);
}
	
EWXWEXPORT(void,wxGrid_SetRowLabelAlignment)(wxGrid* self,int horiz,int vert)
{
	self->SetRowLabelAlignment(horiz, vert);
}
	
EWXWEXPORT(void,wxGrid_SetColLabelAlignment)(wxGrid* self,int horiz,int vert)
{
	self->SetColLabelAlignment(horiz, vert);
}
	
EWXWEXPORT(void,wxGrid_SetRowLabelValue)(wxGrid* self,int row,wxString* label)
{
	self->SetRowLabelValue(row,*label);
}
	
EWXWEXPORT(void,wxGrid_SetColLabelValue)(wxGrid* self,int col,wxString* label)
{
	self->SetColLabelValue(col,*label);
}
	
EWXWEXPORT(void,wxGrid_SetGridLineColour)(wxGrid* self,wxColour* col)
{
	self->SetGridLineColour(*col);
}
	
EWXWEXPORT(void,wxGrid_SetCellHighlightColour)(wxGrid* self,wxColour* col)
{
	self->SetCellHighlightColour(*col);
}
	
EWXWEXPORT(void,wxGrid_EnableDragRowSize)(wxGrid* self,bool enable)
{
	self->EnableDragRowSize(enable);
}
	
EWXWEXPORT(void,wxGrid_DisableDragRowSize)(wxGrid* self)
{
	self->DisableDragRowSize();
}
	
EWXWEXPORT(bool, wxGrid_CanDragRowSize)(wxGrid* self, int row)
{
	return self->CanDragRowSize(row);
}
	
EWXWEXPORT(void,wxGrid_EnableDragColSize)(wxGrid* self,bool enable)
{
	self->EnableDragColSize(enable);
}
	
EWXWEXPORT(void,wxGrid_DisableDragColSize)(wxGrid* self)
{
	self->DisableDragColSize();
}
	
EWXWEXPORT(bool, wxGrid_CanDragColSize)(wxGrid* self, int col)
{
	return self->CanDragColSize(col);
}
	
EWXWEXPORT(void,wxGrid_EnableDragGridSize)(wxGrid* self,bool enable)
{
	self->EnableDragGridSize(enable);
}
	
EWXWEXPORT(void,wxGrid_DisableDragGridSize)(wxGrid* self)
{
	self->DisableDragGridSize();
}
	
EWXWEXPORT(bool,wxGrid_CanDragGridSize)(wxGrid* self)
{
	return self->CanDragGridSize();
}
	
EWXWEXPORT(void,wxGrid_SetRowAttr)(wxGrid* self,int row,wxGridCellAttr* attr)
{
	self->SetRowAttr(row, attr);
}
	
EWXWEXPORT(void,wxGrid_SetColAttr)(wxGrid* self,int col,wxGridCellAttr* attr)
{
	self->SetColAttr(col, attr);
}
	
EWXWEXPORT(void,wxGrid_SetColFormatBool)(wxGrid* self,int col)
{
	self->SetColFormatBool(col);
}
	
EWXWEXPORT(void,wxGrid_SetColFormatNumber)(wxGrid* self,int col)
{
	self->SetColFormatNumber(col);
}
	
EWXWEXPORT(void,wxGrid_SetColFormatFloat)(wxGrid* self,int col,int width,int precision)
{
	self->SetColFormatFloat(col, width, precision);
}
	
EWXWEXPORT(void,wxGrid_SetColFormatCustom)(wxGrid* self,int col,wxString* typeName)
{
	self->SetColFormatCustom(col,*typeName);
}
	
EWXWEXPORT(void,wxGrid_EnableGridLines)(wxGrid* self,bool enable)
{
	self->EnableGridLines(enable);
}
	
EWXWEXPORT(bool,wxGrid_GridLinesEnabled)(wxGrid* self)
{
	return self->GridLinesEnabled();
}
	
EWXWEXPORT(int,wxGrid_GetDefaultRowSize)(wxGrid* self)
{
	return self->GetDefaultRowSize();
}
	
EWXWEXPORT(int,wxGrid_GetRowSize)(wxGrid* self,int row)
{
	return self->GetRowSize(row);
}
	
EWXWEXPORT(int,wxGrid_GetDefaultColSize)(wxGrid* self)
{
	return self->GetDefaultColSize();
}
	
EWXWEXPORT(int,wxGrid_GetColSize)(wxGrid* self,int col)
{
	return self->GetColSize(col);
}
	
EWXWEXPORT(void,wxGrid_GetDefaultCellBackgroundColour)(wxGrid* self,wxColour* colour)
{
	*colour = self->GetDefaultCellBackgroundColour();
}
	
EWXWEXPORT(void,wxGrid_GetCellBackgroundColour)(wxGrid* self,int row,int col,wxColour* colour)
{
	*colour = self->GetCellBackgroundColour(row, col);
}
	
EWXWEXPORT(void,wxGrid_GetDefaultCellTextColour)(wxGrid* self,wxColour* colour)
{
	*colour = self->GetDefaultCellTextColour();
}
	
EWXWEXPORT(void,wxGrid_GetCellTextColour)(wxGrid* self,int row,int col,wxColour* colour)
{
	*colour = self->GetCellTextColour(row, col);
}
	
EWXWEXPORT(void,wxGrid_GetDefaultCellFont)(wxGrid* self,wxFont* font)
{
	*font = self->GetDefaultCellFont();
}
	
EWXWEXPORT(void,wxGrid_GetCellFont)(wxGrid* self,int row,int col,wxFont* font)
{
	*font = self->GetCellFont(row, col);
}
	
EWXWEXPORT(void,wxGrid_GetDefaultCellAlignment)(wxGrid* self,int* horiz,int* vert)
{
	self->GetDefaultCellAlignment(horiz,vert);
}
	
EWXWEXPORT(void,wxGrid_GetCellAlignment)(wxGrid* self,int row,int col,int* horiz,int* vert)
{
	self->GetCellAlignment(row, col, horiz,vert);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultRowSize)(wxGrid* self,int height,bool resizeExistingRows)
{
	self->SetDefaultRowSize(height, resizeExistingRows);
}
	
EWXWEXPORT(void,wxGrid_SetRowSize)(wxGrid* self,int row,int height)
{
	self->SetRowSize(row, height);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultColSize)(wxGrid* self,int width,bool resizeExistingCols)
{
	self->SetDefaultColSize(width, resizeExistingCols);
}
	
EWXWEXPORT(void,wxGrid_SetColSize)(wxGrid* self,int col,int width)
{
	self->SetColSize(col, width);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeColumn)(wxGrid* self,int col,bool setAsMin)
{
	self->AutoSizeColumn(col, setAsMin);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeRow)(wxGrid* self,int row,bool setAsMin)
{
	self->AutoSizeRow(row, setAsMin);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeColumns)(wxGrid* self,bool setAsMin)
{
	self->AutoSizeColumns(setAsMin);
}
	
EWXWEXPORT(void,wxGrid_AutoSizeRows)(wxGrid* self,bool setAsMin)
{
	self->AutoSizeRows(setAsMin);
}
	
EWXWEXPORT(void,wxGrid_AutoSize)(wxGrid* self)
{
	self->AutoSize();
}
	
EWXWEXPORT(void,wxGrid_SetColMinimalWidth)(wxGrid* self,int col,int width)
{
	self->SetColMinimalWidth(col, width);
}
	
EWXWEXPORT(void,wxGrid_SetRowMinimalHeight)(wxGrid* self,int row,int width)
{
	self->SetRowMinimalHeight(row, width);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultCellBackgroundColour)(wxGrid* self,wxColour* colour)
{
	self->SetDefaultCellBackgroundColour(*colour);
}
	
EWXWEXPORT(void,wxGrid_SetCellBackgroundColour)(wxGrid* self,int row,int col,wxColour* colour)
{
	self->SetCellBackgroundColour(row, col,* colour);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultCellTextColour)(wxGrid* self,wxColour* colour)
{
	self->SetDefaultCellTextColour(*colour);
}
	
EWXWEXPORT(void,wxGrid_SetCellTextColour)(wxGrid* self,int row,int col,wxColour* colour)
{
	self->SetCellTextColour(row, col,* colour);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultCellFont)(wxGrid* self,wxFont* font)
{
	self->SetDefaultCellFont(*font);
}
	
EWXWEXPORT(void,wxGrid_SetCellFont)(wxGrid* self,int row,int col,wxFont* font)
{
	self->SetCellFont(row, col,*font );
}
	
EWXWEXPORT(void,wxGrid_SetDefaultCellAlignment)(wxGrid* self,int horiz,int vert)
{
	self->SetDefaultCellAlignment(horiz, vert);
}
	
EWXWEXPORT(void,wxGrid_SetCellAlignment)(wxGrid* self,int row,int col,int horiz,int vert)
{
	self->SetCellAlignment(row, col, horiz, vert);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultRenderer)(wxGrid* self,void* renderer)
{
	self->SetDefaultRenderer((wxGridCellRenderer*)renderer);
}
	
EWXWEXPORT(void,wxGrid_SetCellRenderer)(wxGrid* self,int row,int col,void* renderer)
{
	self->SetCellRenderer(row, col, (wxGridCellRenderer*)renderer);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultRenderer)(wxGrid* self)
{
	return (void*)self->GetDefaultRenderer();
}
	
EWXWEXPORT(void*,wxGrid_GetCellRenderer)(wxGrid* self,int row,int col)
{
	return (void*)self->GetCellRenderer(row, col);
}
	
EWXWEXPORT(void,wxGrid_SetDefaultEditor)(wxGrid* self,wxGridCellEditor* editor)
{
	self->SetDefaultEditor(editor);
}
	
EWXWEXPORT(void,wxGrid_SetCellEditor)(wxGrid* self,int row,int col,wxGridCellEditor* editor)
{
	self->SetCellEditor(row, col,editor);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultEditor)(wxGrid* self)
{
	return (void*)self->GetDefaultEditor();
}
	
EWXWEXPORT(void*,wxGrid_GetCellEditor)(wxGrid* self,int row,int col)
{
	return (void*)self->GetCellEditor(row, col);
}
	
EWXWEXPORT(wxString*,wxGrid_GetCellValue)(wxGrid* self,int row,int col)
{
	wxString *result = new wxString();
	*result = self->GetCellValue(row, col);
	return result;
}
	
EWXWEXPORT(void,wxGrid_SetCellValue)(wxGrid* self,int row,int col,wxString* s)
{
	self->SetCellValue(row, col,* s);
}
	
EWXWEXPORT(bool,wxGrid_IsReadOnly)(wxGrid* self,int row,int col)
{
	return self->IsReadOnly(row, col);
}
	
EWXWEXPORT(void,wxGrid_SetReadOnly)(wxGrid* self,int row,int col,bool isReadOnly)
{
	self->SetReadOnly(row, col, isReadOnly);
}
	
EWXWEXPORT(void,wxGrid_SelectRow)(wxGrid* self,int row,bool addToSelected)
{
	self->SelectRow(row, addToSelected);
}
	
EWXWEXPORT(void,wxGrid_SelectCol)(wxGrid* self,int col,bool addToSelected)
{
	self->SelectCol(col, addToSelected);
}
	
EWXWEXPORT(void,wxGrid_SelectBlock)(wxGrid* self,int topRow,int leftCol,int bottomRow,int rightCol,bool addToSelected)
{
	self->SelectBlock(topRow, leftCol, bottomRow, rightCol, addToSelected);
}
	
EWXWEXPORT(void,wxGrid_SelectAll)(wxGrid* self)
{
	self->SelectAll();
}
	
EWXWEXPORT(bool,wxGrid_IsSelection)(wxGrid* self)
{
	return self->IsSelection();
}
	
EWXWEXPORT(void,wxGrid_ClearSelection)(wxGrid* self)
{
	self->ClearSelection();
}
	
EWXWEXPORT(bool,wxGrid_IsInSelection)(wxGrid* self,int row,int col)
{
	return self->IsInSelection(row, col );
}
	
EWXWEXPORT(wxRect*,wxGrid_BlockToDeviceRect)(wxGrid* self,int top,int left,int bottom,int right)
{
	wxRect* rct = new wxRect();
	*rct = self->BlockToDeviceRect(wxGridCellCoords(top, left), wxGridCellCoords(bottom, right));
	return rct;
}
	
EWXWEXPORT(void,wxGrid_GetSelectionBackground)(wxGrid* self,wxColour* colour)
{
	*colour = self->GetSelectionBackground();
}
	
EWXWEXPORT(void,wxGrid_GetSelectionForeground)(wxGrid* self,wxColour* colour)
{
	*colour = self->GetSelectionForeground();
}
	
EWXWEXPORT(void,wxGrid_SetSelectionBackground)(wxGrid* self,wxColour* c)
{
	self->SetSelectionBackground(*c);
}
	
EWXWEXPORT(void,wxGrid_SetSelectionForeground)(wxGrid* self,wxColour* c)
{
	self->SetSelectionForeground(*c);
}
	
EWXWEXPORT(void,wxGrid_RegisterDataType)(wxGrid* self,wxString* typeName,void* renderer,wxGridCellEditor* editor)
{
	self->RegisterDataType(*typeName, (wxGridCellRenderer*)renderer, editor);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultEditorForCell)(wxGrid* self,int row,int col)
{
	return (void*)self->GetDefaultEditorForCell(row, col);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultRendererForCell)(wxGrid* self,int row,int col)
{
	return (void*)self->GetDefaultRendererForCell(row, col);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultEditorForType)(wxGrid* self,wxString* typeName)
{
	return (void*)self->GetDefaultEditorForType(*typeName);
}
	
EWXWEXPORT(void*,wxGrid_GetDefaultRendererForType)(wxGrid* self,wxString* typeName)
{
	return (void*)self->GetDefaultRendererForType(*typeName);
}
	
EWXWEXPORT(void,wxGrid_SetMargins)(wxGrid* self,int extraWidth,int extraHeight)
{
	self->SetMargins(extraWidth, extraHeight);
}

EWXWEXPORT(void,wxGrid_GetSelectedCells)(wxGrid* self,wxGridCellCoordsArray* _arr)
{
	*_arr = self->GetSelectedCells();
}
	
EWXWEXPORT(void,wxGrid_GetSelectionBlockTopLeft)(wxGrid* self,wxGridCellCoordsArray* _arr)
{
	*_arr = self->GetSelectionBlockTopLeft();
}
	
EWXWEXPORT(void,wxGrid_GetSelectionBlockBottomRight)(wxGrid* self,wxGridCellCoordsArray* _arr)
{
	*_arr = self->GetSelectionBlockBottomRight();
}
	
EWXWEXPORT(int,wxGrid_GetSelectedRows)(wxGrid* self,void* _arr)
{
	wxArrayInt arr = self->GetSelectedRows();
	if (_arr)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((int*)_arr)[i] = arr.Item(i);
	}
	return arr.GetCount();
}
	
EWXWEXPORT(int,wxGrid_GetSelectedCols)(wxGrid* self,void* _arr)
{
	wxArrayInt arr = self->GetSelectedCols();
	if (_arr)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((int*)_arr)[i] = arr.Item(i);
	}
	return arr.GetCount();
}
	
EWXWEXPORT(void,wxGrid_GetCellSize)(wxGrid* self,int r, int c, int* sr, int* sc){
	self->GetCellSize(r,c,sr,sc);
}
EWXWEXPORT(void,wxGrid_SetCellSize)(wxGrid* self,int r, int c, int sr, int sc){
	self->SetCellSize(r,c,sr,sc);
}


EWXWEXPORT(void*,ELJGridTable_Create)(void* self,void* _EifGetNumberRows,void* _EifGetNumberCols,void* _EifGetValue,void* _EifSetValue,void* _EifIsEmptyCell,void* _EifClear,void* _EifInsertRows,void* _EifAppendRows,void* _EifDeleteRows,void* _EifInsertCols,void* _EifAppendCols,void* _EifDeleteCols,void* _EifSetRowLabelValue,void* _EifSetColLabelValue,void* _EifGetRowLabelValue,void* _EifGetColLabelValue)
{
	return (void*)new ELJGridTable (self,
	                                _EifGetNumberRows,
	                                _EifGetNumberCols,
	                                _EifGetValue,
	                                _EifSetValue,
	                                _EifIsEmptyCell,
	                                _EifClear,
	                                _EifInsertRows,
	                                _EifAppendRows,
	                                _EifDeleteRows,
	                                _EifInsertCols,
	                                _EifAppendCols,
	                                _EifDeleteCols,
	                                _EifSetRowLabelValue,
	                                _EifSetColLabelValue,
	                                _EifGetRowLabelValue,
	                                _EifGetColLabelValue);
}
	
EWXWEXPORT(void,ELJGridTable_Delete)(ELJGridTable* self)
{
	delete self;
}

EWXWEXPORT(void*,ELJGridTable_GetView)(ELJGridTable* self)
{
	return (void*)self->GetView();
}

EWXWEXPORT(void,ELJGridTable_SendTableMessage)(ELJGridTable* self,int id,int val1,int val2)
{
	wxGridTableMessage msg(self, id, val1, val2);
	self->GetView()->ProcessTableMessage(msg);
}

EWXWEXPORT(int,wxGridEvent_GetRow)(wxGridEvent* self)
{
	return self->GetRow();
}
	
EWXWEXPORT(int,wxGridEvent_GetCol)(wxGridEvent* self)
{
	return self->GetCol();
}
	
EWXWEXPORT(wxPoint*,wxGridEvent_GetPosition)(wxGridEvent* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetPosition();
	return pt;
}
	
EWXWEXPORT(bool,wxGridEvent_Selecting)(wxGridEvent* self)
{
	return self->Selecting();
}
	
EWXWEXPORT(bool,wxGridEvent_ControlDown)(wxGridEvent* self)
{
	return self->ControlDown();
}
	
EWXWEXPORT(bool,wxGridEvent_MetaDown)(wxGridEvent* self)
{
	return self->MetaDown();
}
	
EWXWEXPORT(bool,wxGridEvent_ShiftDown)(wxGridEvent* self)
{
	return self->ShiftDown();
}
	
EWXWEXPORT(bool,wxGridEvent_AltDown)(wxGridEvent* self)
{
	return self->AltDown();
}
	

EWXWEXPORT(int,wxGridSizeEvent_GetRowOrCol)(wxGridSizeEvent* self)
{
	return self->GetRowOrCol();
}
	
EWXWEXPORT(wxPoint*,wxGridSizeEvent_GetPosition)(wxGridSizeEvent* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetPosition();
	return pt;
}
	
EWXWEXPORT(bool,wxGridSizeEvent_ControlDown)(wxGridSizeEvent* self)
{
	return self->ControlDown();
}
	
EWXWEXPORT(bool,wxGridSizeEvent_MetaDown)(wxGridSizeEvent* self)
{
	return self->MetaDown();
}
	
EWXWEXPORT(bool,wxGridSizeEvent_ShiftDown)(wxGridSizeEvent* self)
{
	return self->ShiftDown();
}
	
EWXWEXPORT(bool,wxGridSizeEvent_AltDown)(wxGridSizeEvent* self)
{
	return self->AltDown();
}
	

EWXWEXPORT(void,wxGridRangeSelectEvent_GetTopLeftCoords)(wxGridRangeSelectEvent* self,int* _c,int* _r)
{
	wxGridCellCoords crd = self->GetTopLeftCoords();
	*_c = crd.GetRow();
	*_r = crd.GetCol();
}
	
EWXWEXPORT(void,wxGridRangeSelectEvent_GetBottomRightCoords)(wxGridRangeSelectEvent* self,int* _c,int* _r)
{
	wxGridCellCoords crd = self->GetBottomRightCoords();
	*_c = crd.GetRow();
	*_r = crd.GetCol();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_GetTopRow)(wxGridRangeSelectEvent* self)
{
	return self->GetTopRow();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_GetBottomRow)(wxGridRangeSelectEvent* self)
{
	return self->GetBottomRow();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_GetLeftCol)(wxGridRangeSelectEvent* self)
{
	return self->GetLeftCol();
}
	
EWXWEXPORT(int,wxGridRangeSelectEvent_GetRightCol)(wxGridRangeSelectEvent* self)
{
	return self->GetRightCol();
}
	
EWXWEXPORT(bool,wxGridRangeSelectEvent_Selecting)(wxGridRangeSelectEvent* self)
{
	return self->Selecting();
}
	
EWXWEXPORT(bool,wxGridRangeSelectEvent_ControlDown)(wxGridRangeSelectEvent* self)
{
	return self->ControlDown();
}
	
EWXWEXPORT(bool,wxGridRangeSelectEvent_MetaDown)(wxGridRangeSelectEvent* self)
{
	return self->MetaDown();
}
	
EWXWEXPORT(bool,wxGridRangeSelectEvent_ShiftDown)(wxGridRangeSelectEvent* self)
{
	return self->ShiftDown();
}
	
EWXWEXPORT(bool,wxGridRangeSelectEvent_AltDown)(wxGridRangeSelectEvent* self)
{
	return self->AltDown();
}
	

EWXWEXPORT(int,wxGridEditorCreatedEvent_GetRow)(wxGridEditorCreatedEvent* self)
{
	return self->GetRow();
}
	
EWXWEXPORT(int,wxGridEditorCreatedEvent_GetCol)(wxGridEditorCreatedEvent* self)
{
	return self->GetCol();
}
	
EWXWEXPORT(void*,wxGridEditorCreatedEvent_GetControl)(wxGridEditorCreatedEvent* self)
{
	return (void*)self->GetControl();
}
	
EWXWEXPORT(void,wxGridEditorCreatedEvent_SetRow)(wxGridEditorCreatedEvent* self,int row)
{
	self->SetRow(row);
}
	
EWXWEXPORT(void,wxGridEditorCreatedEvent_SetCol)(wxGridEditorCreatedEvent* self,int col)
{
	self->SetCol(col);
}
	
EWXWEXPORT(void,wxGridEditorCreatedEvent_SetControl)(wxGridEditorCreatedEvent* self,wxControl* ctrl)
{
	self->SetControl(ctrl);
}
	

} 
