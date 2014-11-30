#include "wrapper.h"
#include "wx/tooltip.h"

extern "C"
{


/* wxAuiDefaultTabArt */

EWXWEXPORT(void*,wxAuiDefaultTabArt_Create)()
{
   return (void*)new wxAuiDefaultTabArt();
}

EWXWEXPORT(void*,wxAuiDefaultTabArt_Clone)(wxAuiDefaultTabArt* self)
{
   return (void*)self->Clone();
}


EWXWEXPORT(void,wxAuiDefaultTabArt_SetFlags)(wxAuiDefaultTabArt* self, int _flags)
{
    self->SetFlags(_flags);
}


EWXWEXPORT(void,wxAuiDefaultTabArt_SetSizingInfo)(wxAuiDefaultTabArt* self, int _wdt, int _hgt, size_t tabCount)
{
  self->SetSizingInfo(wxSize(_wdt, _hgt),tabCount);
}


EWXWEXPORT(void,wxAuiDefaultTabArt_SetNormalFont)(wxAuiDefaultTabArt* self, wxFont* font)
{
    self->SetNormalFont(*font);
}


EWXWEXPORT(void,wxAuiDefaultTabArt_SetSelectedFont)(wxAuiDefaultTabArt* self, wxFont* font)
{
    self->SetSelectedFont(*font);
}


EWXWEXPORT(void,wxAuiDefaultTabArt_SetMeasuringFont)(wxAuiDefaultTabArt* self, wxFont* font)
{
    self->SetMeasuringFont(*font);
}


EWXWEXPORT(void,wxAuiDefaultTabArt_SetColour)(wxAuiDefaultTabArt* self, wxColour* _colour)
{
    self->SetColour(*_colour);
}


EWXWEXPORT(void,wxAuiDefaultTabArt_SetActiveColour)(wxAuiDefaultTabArt* self, wxColour* _colour)
{
    self->SetActiveColour(*_colour);
}


EWXWEXPORT(void,wxAuiDefaultTabArt_DrawBackground)(wxAuiDefaultTabArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawBackground(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiDefaultTabArt_DrawTab)(wxAuiDefaultTabArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiNotebookPage* _pane, wxRect* _inRect, int closeButtonState, wxRect* _outTabRect, wxRect* _outButtonRect, int* xExtent)
{
 self->DrawTab(*_dc, _wnd, *_pane, *_inRect, closeButtonState, _outTabRect, _outButtonRect, xExtent);
}


EWXWEXPORT(void,wxAuiDefaultTabArt_DrawButton)(wxAuiDefaultTabArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _inRect, int bitmapId, int buttonState, int orientation, wxRect* _outRect)
{
 self->DrawButton(*_dc, _wnd, *_inRect, bitmapId, buttonState, orientation, _outRect);
}


EWXWEXPORT(int,wxAuiDefaultTabArt_GetIndentSize)(wxAuiDefaultTabArt* self)
{
   return self->GetIndentSize();
}

EWXWEXPORT(wxSize*,wxAuiDefaultTabArt_GetTabSize)(wxAuiDefaultTabArt* self, wxDC* _dc, wxWindow* _wnd, wxString* _caption, wxBitmap* _bitmap, bool active, int closeButtonState, int *xExtent )
{
	wxSize* sz = new wxSize();
	*sz = self->GetTabSize(*_dc,_wnd,*_caption,*_bitmap,active,closeButtonState,xExtent );
	return sz;
}

EWXWEXPORT(int,wxAuiDefaultTabArt_ShowDropDown)(wxAuiDefaultTabArt* self, wxWindow* _wnd, wxAuiNotebookPageArray* _items, int activeIdx)
{
  return self->ShowDropDown(_wnd,*_items,activeIdx);
}


EWXWEXPORT(int,wxAuiDefaultTabArt_GetBestTabCtrlSize)(wxAuiDefaultTabArt* self, wxWindow* _wnd, wxAuiNotebookPageArray* _pages, int _wdt, int _hgt)
{
  return self->GetBestTabCtrlSize(_wnd,*_pages,wxSize(_wdt, _hgt));
}


/* wxAuiToolBarEvent */

EWXWEXPORT(bool,wxAuiToolBarEvent_IsDropDownClicked)(wxAuiToolBarEvent* self)
{
   return self->IsDropDownClicked();
}

EWXWEXPORT(wxPoint*,wxAuiToolBarEvent_GetClickPoint)(wxAuiToolBarEvent* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetClickPoint();
	return pt;
}

EWXWEXPORT(wxRect*,wxAuiToolBarEvent_GetItemRect)(wxAuiToolBarEvent* self)
{
	wxRect* rect = new wxRect();
	*rect = self->GetItemRect();
	return rect;
}

EWXWEXPORT(int,wxAuiToolBarEvent_GetToolId)(wxAuiToolBarEvent* self)
{
   return self->GetToolId();
}


/* wxAuiToolBarItem */

EWXWEXPORT(void*,wxAuiToolBarItem_CreateDefault)()
{
   return (void*)new wxAuiToolBarItem();
}


EWXWEXPORT(void*,wxAuiToolBarItem_Create)(wxAuiToolBarItem* _c)
{
 return (void*)new wxAuiToolBarItem(*_c);
}


EWXWEXPORT(wxAuiToolBarItem*,wxAuiToolBarItem_Copy)(wxAuiToolBarItem* self, wxAuiToolBarItem* _c)
{
  wxAuiToolBarItem *result = new wxAuiToolBarItem();
  *result = self->operator=(*_c);
  return result;
}


EWXWEXPORT(void,wxAuiToolBarItem_Assign)(wxAuiToolBarItem* self, wxAuiToolBarItem* _c)
{
   self->Assign(*_c);
}


EWXWEXPORT(void,wxAuiToolBarItem_SetWindow)(wxAuiToolBarItem* self, wxWindow* _w)
{
   self->SetWindow(_w);
}


EWXWEXPORT(void*,wxAuiToolBarItem_GetWindow)(wxAuiToolBarItem* self)
{
   return (void*)self->GetWindow();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetId)(wxAuiToolBarItem* self, int new_id)
{
    self->SetId(new_id);
}


EWXWEXPORT(int,wxAuiToolBarItem_GetId)(wxAuiToolBarItem* self)
{
   return self->GetId();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetKind)(wxAuiToolBarItem* self, int new_kind)
{
    self->SetKind(new_kind);
}


EWXWEXPORT(int,wxAuiToolBarItem_GetKind)(wxAuiToolBarItem* self)
{
   return self->GetKind();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetState)(wxAuiToolBarItem* self, int new_state)
{
    self->SetState(new_state);
}


EWXWEXPORT(int,wxAuiToolBarItem_GetState)(wxAuiToolBarItem* self)
{
   return self->GetState();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetSizerItem)(wxAuiToolBarItem* self, wxSizerItem* _s)
{
   self->SetSizerItem(_s);
}


EWXWEXPORT(void*,wxAuiToolBarItem_GetSizerItem)(wxAuiToolBarItem* self)
{
   return (void*)self->GetSizerItem();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetLabel)(wxAuiToolBarItem* self, wxString* _s)
{
    self->SetLabel(*_s);
}

EWXWEXPORT(wxString*,wxAuiToolBarItem_GetLabel)(wxAuiToolBarItem* self)
{
	wxString *result = new wxString();
	*result = self->GetLabel();
	return result;
}

EWXWEXPORT(void,wxAuiToolBarItem_SetBitmap)(wxAuiToolBarItem* self, wxBitmap* _bmp)
{
   self->SetBitmap(*_bmp);
}

EWXWEXPORT(void,wxAuiToolBarItem_GetBitmap)(wxAuiToolBarItem* self, wxBitmap* _bmp)
{
   *_bmp = self->GetBitmap();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetDisabledBitmap)(wxAuiToolBarItem* self, wxBitmap* _bmp)
{
   self->SetDisabledBitmap(*_bmp);
}


EWXWEXPORT(void,wxAuiToolBarItem_GetDisabledBitmap)(wxAuiToolBarItem* self, wxBitmap* _bmp)
{
   *_bmp = self->GetDisabledBitmap();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetHoverBitmap)(wxAuiToolBarItem* self, wxBitmap* _bmp)
{
   self->SetHoverBitmap(*_bmp);
}


EWXWEXPORT(void,wxAuiToolBarItem_GetHoverBitmap)(wxAuiToolBarItem* self, wxBitmap* _bmp)
{
   *_bmp = self->GetHoverBitmap();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetShortHelp)(wxAuiToolBarItem* self, wxString* _s)
{
    self->SetShortHelp(*_s);
}

EWXWEXPORT(wxString*,wxAuiToolBarItem_GetShortHelp)(wxAuiToolBarItem* self)
{
	wxString *result = new wxString();
	*result = self->GetShortHelp();
	return result;
}

EWXWEXPORT(void,wxAuiToolBarItem_SetLongHelp)(wxAuiToolBarItem* self, wxString* _s)
{
    self->SetLongHelp(*_s);
}

EWXWEXPORT(wxString*,wxAuiToolBarItem_GetLongHelp)(wxAuiToolBarItem* self)
{
	wxString *result = new wxString();
	*result = self->GetLongHelp();
	return result;
}

EWXWEXPORT(void,wxAuiToolBarItem_SetMinSize)(wxAuiToolBarItem* self, int _wdt, int _hgt)
{
  self->SetMinSize(wxSize(_wdt, _hgt));
}

EWXWEXPORT(wxSize*,wxAuiToolBarItem_GetMinSize)(wxAuiToolBarItem* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetMinSize();
	return sz;
}

EWXWEXPORT(void,wxAuiToolBarItem_SetSpacerPixels)(wxAuiToolBarItem* self, int s)
{
    self->SetSpacerPixels(s);
}


EWXWEXPORT(int,wxAuiToolBarItem_GetSpacerPixels)(wxAuiToolBarItem* self)
{
   return self->GetSpacerPixels();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetProportion)(wxAuiToolBarItem* self, int p)
{
    self->SetProportion(p);
}


EWXWEXPORT(int,wxAuiToolBarItem_GetProportion)(wxAuiToolBarItem* self)
{
   return self->GetProportion();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetActive)(wxAuiToolBarItem* self, bool b)
{
    self->SetActive(b);
}


EWXWEXPORT(bool,wxAuiToolBarItem_IsActive)(wxAuiToolBarItem* self)
{
   return self->IsActive();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetHasDropDown)(wxAuiToolBarItem* self, bool b)
{
    self->SetHasDropDown(b);
}


EWXWEXPORT(bool,wxAuiToolBarItem_HasDropDown)(wxAuiToolBarItem* self)
{
   return self->HasDropDown();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetSticky)(wxAuiToolBarItem* self, bool b)
{
    self->SetSticky(b);
}


EWXWEXPORT(bool,wxAuiToolBarItem_IsSticky)(wxAuiToolBarItem* self)
{
   return self->IsSticky();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetUserData)(wxAuiToolBarItem* self, long l)
{
    self->SetUserData(l);
}


EWXWEXPORT(long,wxAuiToolBarItem_GetUserData)(wxAuiToolBarItem* self)
{
   return self->GetUserData();
}


EWXWEXPORT(void,wxAuiToolBarItem_SetAlignment)(wxAuiToolBarItem* self, int l)
{
    self->SetAlignment(l);
}


EWXWEXPORT(int,wxAuiToolBarItem_GetAlignment)(wxAuiToolBarItem* self)
{
   return self->GetAlignment();
}


/* wxAuiToolBarArt */


EWXWEXPORT(void*,wxAuiToolBarArt_Clone)(wxAuiToolBarArt* self)
{
   return (void*)self->Clone();
}


EWXWEXPORT(void,wxAuiToolBarArt_SetFlags)(wxAuiToolBarArt* self, int _flags)
{
    self->SetFlags(_flags);
}


EWXWEXPORT(int,wxAuiToolBarArt_GetFlags)(wxAuiToolBarArt* self)
{
 return self->GetFlags();
}


EWXWEXPORT(void,wxAuiToolBarArt_SetFont)(wxAuiToolBarArt* self, wxFont* font)
{
    self->SetFont(*font);
}

EWXWEXPORT(wxFont*,wxAuiToolBarArt_GetFont)(wxAuiToolBarArt* self)
{
	wxFont* font = new wxFont();
	*font = self->GetFont();
	return font;
}

EWXWEXPORT(void,wxAuiToolBarArt_SetTextOrientation)(wxAuiToolBarArt* self, int orientation)
{
    self->SetTextOrientation(orientation);
}


EWXWEXPORT(int,wxAuiToolBarArt_GetTextOrientation)(wxAuiToolBarArt* self)
{
   return self->GetTextOrientation();
}


EWXWEXPORT(void,wxAuiToolBarArt_DrawBackground)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawBackground(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiToolBarArt_DrawPlainBackground)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawPlainBackground(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiToolBarArt_DrawLabel)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item, wxRect* _rect)
{
 self->DrawLabel(*_dc,_wnd,*_item,*_rect);
}


EWXWEXPORT(void,wxAuiToolBarArt_DrawButton)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item, wxRect* _rect)
{
 self->DrawButton(*_dc,_wnd,*_item,*_rect);
}


EWXWEXPORT(void,wxAuiToolBarArt_DrawDropDownButton)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item, wxRect* _rect)
{
 self->DrawDropDownButton(*_dc,_wnd,*_item,*_rect);
}


EWXWEXPORT(void,wxAuiToolBarArt_DrawControlLabel)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item, wxRect* _rect)
{
 self->DrawControlLabel(*_dc,_wnd,*_item,*_rect);
}


EWXWEXPORT(void,wxAuiToolBarArt_DrawSeparator)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawSeparator(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiToolBarArt_DrawGripper)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawGripper(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiToolBarArt_DrawOverflowButton)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect, int state)
{
  self->DrawOverflowButton(*_dc,_wnd,*_rect,state);
}

EWXWEXPORT(wxSize*,wxAuiToolBarArt_GetLabelSize)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item )
{
	wxSize* sz = new wxSize();
	*sz = self->GetLabelSize(*_dc,_wnd,*_item );
	return sz;
}

EWXWEXPORT(wxSize*,wxAuiToolBarArt_GetToolSize)(wxAuiToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item )
{
	wxSize* sz = new wxSize();
	*sz = self->GetToolSize(*_dc,_wnd,*_item );
	return sz;
}

EWXWEXPORT(int,wxAuiToolBarArt_GetElementSize)(wxAuiToolBarArt* self, int element_id)
{
   return self->GetElementSize(element_id);
}


EWXWEXPORT(void,wxAuiToolBarArt_SetElementSize)(wxAuiToolBarArt* self, int element_id, int size)
{
    self->SetElementSize(element_id, size);
}


EWXWEXPORT(int,wxAuiToolBarArt_ShowDropDown)(wxAuiToolBarArt* self, wxWindow* _wnd, wxAuiToolBarItemArray* _items)
{
  return self->ShowDropDown(_wnd,*_items);
}


/* wxAuiDefaultToolBarArt */

EWXWEXPORT(void*,wxAuiDefaultToolBarArt_Create)()
{
   return (void*)new wxAuiDefaultToolBarArt();
}

EWXWEXPORT(void*,wxAuiDefaultToolBarArt_Clone)(wxAuiDefaultToolBarArt* self)
{
   return (void*)self->Clone();
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_SetFlags)(wxAuiDefaultToolBarArt* self, int _flags)
{
    self->SetFlags(_flags);
}


EWXWEXPORT(int,wxAuiDefaultToolBarArt_GetFlags)(wxAuiDefaultToolBarArt* self)
{
 return self->GetFlags();
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_SetFont)(wxAuiDefaultToolBarArt* self, wxFont* font)
{
    self->SetFont(*font);
}

EWXWEXPORT(wxFont*,wxAuiDefaultToolBarArt_GetFont)(wxAuiDefaultToolBarArt* self)
{
	wxFont* font = new wxFont();
	*font = self->GetFont();
	return font;
}

EWXWEXPORT(void,wxAuiDefaultToolBarArt_SetTextOrientation)(wxAuiDefaultToolBarArt* self, int orientation)
{
    self->SetTextOrientation(orientation);
}


EWXWEXPORT(int,wxAuiDefaultToolBarArt_GetTextOrientation)(wxAuiDefaultToolBarArt* self)
{
   return self->GetTextOrientation();
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_DrawBackground)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawBackground(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_DrawPlainBackground)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawPlainBackground(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_DrawLabel)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item, wxRect* _rect)
{
 self->DrawLabel(*_dc,_wnd,*_item,*_rect);
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_DrawButton)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item, wxRect* _rect)
{
 self->DrawButton(*_dc,_wnd,*_item,*_rect);
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_DrawDropDownButton)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item, wxRect* _rect)
{
 self->DrawDropDownButton(*_dc,_wnd,*_item,*_rect);
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_DrawControlLabel)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item, wxRect* _rect)
{
 self->DrawControlLabel(*_dc,_wnd,*_item,*_rect);
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_DrawSeparator)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawSeparator(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_DrawGripper)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawGripper(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_DrawOverflowButton)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect, int state)
{
  self->DrawOverflowButton(*_dc,_wnd,*_rect,state);
}

EWXWEXPORT(wxSize*,wxAuiDefaultToolBarArt_GetLabelSize)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item )
{
	wxSize* sz = new wxSize();
	*sz = self->GetLabelSize(*_dc,_wnd,*_item );
	return sz;
}


EWXWEXPORT(wxSize*,wxAuiDefaultToolBarArt_GetToolSize)(wxAuiDefaultToolBarArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiToolBarItem* _item )
{
	wxSize* sz = new wxSize();
	*sz = self->GetToolSize(*_dc,_wnd,*_item );
	return sz;
}

EWXWEXPORT(int,wxAuiDefaultToolBarArt_GetElementSize)(wxAuiDefaultToolBarArt* self, int element)
{
   return self->GetElementSize(element);
}


EWXWEXPORT(void,wxAuiDefaultToolBarArt_SetElementSize)(wxAuiDefaultToolBarArt* self, int element_id, int size)
{
    self->SetElementSize(element_id, size);
}


EWXWEXPORT(int,wxAuiDefaultToolBarArt_ShowDropDown)(wxAuiDefaultToolBarArt* self, wxWindow* _wnd, wxAuiToolBarItemArray* _items)
{
  return self->ShowDropDown(_wnd,*_items);
}


/* wxAuiToolBar */

EWXWEXPORT(void*,wxAuiToolBar_CreateDefault)()
{
   return (void*)new wxAuiToolBar();
}

EWXWEXPORT(void*,wxAuiToolBar_Create)(wxWindow* _parent, int id, int _lft,int _top, int _wdt, int _hgt, long style)
{
 return (void*)new wxAuiToolBar(_parent, id,wxPoint(_lft, _top),wxSize(_wdt, _hgt),style);
}


EWXWEXPORT(bool,wxAuiToolBar_CreateFromDefault)(wxAuiToolBar* self, wxWindow* _parent, int id, int _lft,int _top, int _wdt, int _hgt, long style )
{
 return self->Create(_parent, id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), style );
}

EWXWEXPORT(void,wxAuiToolBar_SetWindowStyleFlag)(wxAuiToolBar* self, long style)
{
    self->SetWindowStyleFlag(style);
}


EWXWEXPORT(long,wxAuiToolBar_GetWindowStyleFlag)(wxAuiToolBar* self)
{
   return self->GetWindowStyleFlag();
}


EWXWEXPORT(void,wxAuiToolBar_SetArtProvider)(wxAuiToolBar* self, wxAuiToolBarArt* _art)
{
   self->SetArtProvider(_art);
}


EWXWEXPORT(void*,wxAuiToolBar_GetArtProvider)(wxAuiToolBar* self)
{
   return (void*)self->GetArtProvider();
}


EWXWEXPORT(bool,wxAuiToolBar_SetFont)(wxAuiToolBar* self, wxFont* _font)
{
  return self->SetFont(*_font);
}

EWXWEXPORT(void*,wxAuiToolBar_AddToolByLabel)(wxAuiToolBar* self, int tool_id, wxString* _label, wxBitmap* _bitmap, wxString* _short_help_string, int kind)
{
 return (void*)self->AddTool(tool_id,*_label,*_bitmap,*_short_help_string,static_cast<wxItemKind>(kind));
}

EWXWEXPORT(void*,wxAuiToolBar_AddTool)(wxAuiToolBar* self, int tool_id, wxString* _label, wxBitmap* _bitmap, wxBitmap* _disabled_bitmap, int kind, wxString* _short_help_string, wxString* _long_help_string, wxObject* _client_data)
{
 return (void*)self->AddTool(tool_id,*_label,*_bitmap,*_disabled_bitmap,static_cast<wxItemKind>(kind),*_short_help_string,*_long_help_string,_client_data);
}

EWXWEXPORT(void*,wxAuiToolBar_AddToolByBitmap)(wxAuiToolBar* self, int tool_id, wxBitmap* _bitmap, wxBitmap* _disabled_bitmap, bool toggle, wxObject* _client_data, wxString* _short_help_string, wxString* _long_help_string)
{
 return (void*)self->AddTool(tool_id,*_bitmap,*_disabled_bitmap,toggle,_client_data,*_short_help_string,*_long_help_string);
}

EWXWEXPORT(void*,wxAuiToolBar_AddLabel)(wxAuiToolBar* self, int tool_id, wxString* _label, int width)
{
 return (void*)self->AddLabel(tool_id, *_label, width);
}


EWXWEXPORT(void*,wxAuiToolBar_AddControl)(wxAuiToolBar* self, wxControl* _control, wxString* _label)
{
 return (void*)self->AddControl(_control, *_label);
}


EWXWEXPORT(void*,wxAuiToolBar_AddSeparator)(wxAuiToolBar* self)
{
   return (void*)self->AddSeparator();
}


EWXWEXPORT(void*,wxAuiToolBar_AddSpacer)(wxAuiToolBar* self, int pixels)
{
   return (void*)self->AddSpacer(pixels);
}


EWXWEXPORT(void*,wxAuiToolBar_AddStretchSpacer)(wxAuiToolBar* self, int proportion)
{
   return (void*)self->AddStretchSpacer(proportion);
}


EWXWEXPORT(bool,wxAuiToolBar_Realize)(wxAuiToolBar* self)
{
   return self->Realize();
}


EWXWEXPORT(void*,wxAuiToolBar_FindControl)(wxAuiToolBar* self, int window_id)
{
   return (void*)self->FindControl(window_id);
}


EWXWEXPORT(void*,wxAuiToolBar_FindToolByPosition)(wxAuiToolBar* self, int x, int y)
{
   return (void*)self->FindToolByPosition(x, y);
}


EWXWEXPORT(void*,wxAuiToolBar_FindToolByIndex)(wxAuiToolBar* self, int idx)
{
   return (void*)self->FindToolByIndex(idx);
}


EWXWEXPORT(void*,wxAuiToolBar_FindTool)(wxAuiToolBar* self, int tool_id)
{
   return (void*)self->FindTool(tool_id);
}


EWXWEXPORT(void,wxAuiToolBar_ClearTools)(wxAuiToolBar* self)
{
    self->ClearTools();
}


EWXWEXPORT(void,wxAuiToolBar_Clear)(wxAuiToolBar* self)
{
    self->Clear();
}


EWXWEXPORT(bool,wxAuiToolBar_DeleteTool)(wxAuiToolBar* self, int tool_id)
{
   return self->DeleteTool(tool_id);
}


EWXWEXPORT(bool,wxAuiToolBar_DeleteByIndex)(wxAuiToolBar* self, int tool_id)
{
   return self->DeleteByIndex(tool_id);
}


EWXWEXPORT(size_t,wxAuiToolBar_GetToolCount)(wxAuiToolBar* self)
{
  return self->GetToolCount();
}


EWXWEXPORT(int,wxAuiToolBar_GetToolPos)(wxAuiToolBar* self, int tool_id)
{
   return self->GetToolPos(tool_id);
}


EWXWEXPORT(int,wxAuiToolBar_GetToolIndex)(wxAuiToolBar* self, int tool_id)
{
   return self->GetToolIndex(tool_id);
}


EWXWEXPORT(bool,wxAuiToolBar_GetToolFits)(wxAuiToolBar* self, int tool_id)
{
   return self->GetToolFits(tool_id);
}

EWXWEXPORT(wxRect*,wxAuiToolBar_GetToolRect)(wxAuiToolBar* self, int tool_id )
{
	wxRect* rect = new wxRect();
	*rect = self->GetToolRect(tool_id);
	return rect;
}

EWXWEXPORT(bool,wxAuiToolBar_GetToolFitsByIndex)(wxAuiToolBar* self, int tool_id)
{
   return self->GetToolFitsByIndex(tool_id);
}


EWXWEXPORT(bool,wxAuiToolBar_GetToolBarFits)(wxAuiToolBar* self)
{
   return self->GetToolBarFits();
}


EWXWEXPORT(void,wxAuiToolBar_SetMargins)(wxAuiToolBar* self, int _wdt, int _hgt)
{
  self->SetMargins(wxSize(_wdt, _hgt));
}


EWXWEXPORT(void,wxAuiToolBar_SetMarginsXY)(wxAuiToolBar* self, int x, int y)
{
    self->SetMargins(x, y);
}


EWXWEXPORT(void,wxAuiToolBar_SetMarginsDetailed)(wxAuiToolBar* self, int left, int right, int top, int bottom)
{
    self->SetMargins(left, right, top, bottom);
}


EWXWEXPORT(void,wxAuiToolBar_SetToolBitmapSize)(wxAuiToolBar* self, int _wdt, int _hgt)
{
  self->SetToolBitmapSize(wxSize(_wdt, _hgt));
}

EWXWEXPORT(wxSize*,wxAuiToolBar_GetToolBitmapSize)(wxAuiToolBar* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetToolBitmapSize();
	return sz;
}

EWXWEXPORT(bool,wxAuiToolBar_GetOverflowVisible)(wxAuiToolBar* self)
{
   return self->GetOverflowVisible();
}


EWXWEXPORT(void,wxAuiToolBar_SetOverflowVisible)(wxAuiToolBar* self, bool visible)
{
    self->SetOverflowVisible(visible);
}


EWXWEXPORT(bool,wxAuiToolBar_GetGripperVisible)(wxAuiToolBar* self)
{
   return self->GetGripperVisible();
}


EWXWEXPORT(void,wxAuiToolBar_SetGripperVisible)(wxAuiToolBar* self, bool visible)
{
    self->SetGripperVisible(visible);
}


EWXWEXPORT(void,wxAuiToolBar_ToggleTool)(wxAuiToolBar* self, int tool_id, bool state)
{
    self->ToggleTool(tool_id, state);
}


EWXWEXPORT(bool,wxAuiToolBar_GetToolToggled)(wxAuiToolBar* self, int tool_id)
{
   return self->GetToolToggled(tool_id);
}


EWXWEXPORT(void,wxAuiToolBar_EnableTool)(wxAuiToolBar* self, int tool_id, bool state)
{
    self->EnableTool(tool_id, state);
}


EWXWEXPORT(bool,wxAuiToolBar_GetToolEnabled)(wxAuiToolBar* self, int tool_id)
{
   return self->GetToolEnabled(tool_id);
}


EWXWEXPORT(void,wxAuiToolBar_SetToolDropDown)(wxAuiToolBar* self, int tool_id, bool dropdown)
{
    self->SetToolDropDown(tool_id, dropdown);
}


EWXWEXPORT(bool,wxAuiToolBar_GetToolDropDown)(wxAuiToolBar* self, int tool_id)
{
   return self->GetToolDropDown(tool_id);
}


EWXWEXPORT(void,wxAuiToolBar_SetToolBorderPadding)(wxAuiToolBar* self, int padding)
{
    self->SetToolBorderPadding(padding);
}


EWXWEXPORT(int,wxAuiToolBar_GetToolBorderPadding)(wxAuiToolBar* self)
{
   return self->GetToolBorderPadding();
}


EWXWEXPORT(void,wxAuiToolBar_SetToolTextOrientation)(wxAuiToolBar* self, int orientation)
{
    self->SetToolTextOrientation(orientation);
}


EWXWEXPORT(int,wxAuiToolBar_GetToolTextOrientation)(wxAuiToolBar* self)
{
   return self->GetToolTextOrientation();
}


EWXWEXPORT(void,wxAuiToolBar_SetToolPacking)(wxAuiToolBar* self, int packing)
{
    self->SetToolPacking(packing);
}


EWXWEXPORT(int,wxAuiToolBar_GetToolPacking)(wxAuiToolBar* self)
{
   return self->GetToolPacking();
}


EWXWEXPORT(void,wxAuiToolBar_SetToolProportion)(wxAuiToolBar* self, int tool_id, int proportion)
{
    self->SetToolProportion(tool_id, proportion);
}


EWXWEXPORT(int,wxAuiToolBar_GetToolProportion)(wxAuiToolBar* self, int tool_id)
{
   return self->GetToolProportion(tool_id);
}


EWXWEXPORT(void,wxAuiToolBar_SetToolSeparation)(wxAuiToolBar* self, int separation)
{
    self->SetToolSeparation(separation);
}


EWXWEXPORT(int,wxAuiToolBar_GetToolSeparation)(wxAuiToolBar* self)
{
   return self->GetToolSeparation();
}


EWXWEXPORT(void,wxAuiToolBar_SetToolSticky)(wxAuiToolBar* self, int tool_id, bool sticky)
{
    self->SetToolSticky(tool_id, sticky);
}


EWXWEXPORT(bool,wxAuiToolBar_GetToolSticky)(wxAuiToolBar* self, int tool_id)
{
   return self->GetToolSticky(tool_id);
}

EWXWEXPORT(wxString*,wxAuiToolBar_GetToolLabel)(wxAuiToolBar* self, int tool_id )
{
	wxString *result = new wxString();
	*result = self->GetToolLabel(tool_id);
	return result;
}

EWXWEXPORT(void,wxAuiToolBar_SetToolLabel)(wxAuiToolBar* self, int tool_id, wxString* _label)
{
  self->SetToolLabel(tool_id,*_label);
}


EWXWEXPORT(void,wxAuiToolBar_GetToolBitmap)(wxAuiToolBar* self, int tool_id, wxBitmap* bmp)
{
   *bmp = self->GetToolBitmap(tool_id);
}


EWXWEXPORT(void,wxAuiToolBar_SetToolBitmap)(wxAuiToolBar* self, int tool_id, wxBitmap* _bitmap)
{
  self->SetToolBitmap(tool_id,*_bitmap);
}

EWXWEXPORT(wxString*,wxAuiToolBar_GetToolShortHelp)(wxAuiToolBar* self, int tool_id )
{
	wxString *result = new wxString();
	*result = self->GetToolShortHelp(tool_id);
	return result;
}

EWXWEXPORT(void,wxAuiToolBar_SetToolShortHelp)(wxAuiToolBar* self, int tool_id, wxString* _help_string)
{
  self->SetToolShortHelp(tool_id,*_help_string);
}

EWXWEXPORT(wxString*,wxAuiToolBar_GetToolLongHelp)(wxAuiToolBar* self, int tool_id )
{
	wxString *result = new wxString();
	*result = self->GetToolLongHelp(tool_id);
	return result;
}

EWXWEXPORT(void,wxAuiToolBar_SetToolLongHelp)(wxAuiToolBar* self, int tool_id, wxString* _help_string)
{
  self->SetToolLongHelp(tool_id,*_help_string);
}


EWXWEXPORT(void,wxAuiToolBar_SetCustomOverflowItems)(wxAuiToolBar* self, wxAuiToolBarItemArray* _prepend, wxAuiToolBarItemArray* _append)
{
  self->SetCustomOverflowItems(*_prepend,*_append);
}

EWXWEXPORT(wxSize*,wxAuiToolBar_GetHintSize)(wxAuiToolBar* self, int dock_direction)
{
	wxSize* sz = new wxSize();
	*sz = self->GetHintSize(dock_direction);
	return sz;
}

EWXWEXPORT(bool,wxAuiToolBar_IsPaneValid)(wxAuiToolBar* self, wxAuiPaneInfo* _pane)
{
  return self->IsPaneValid(*_pane);
}


/* wxAuiNotebook */

EWXWEXPORT(void*,wxAuiNotebook_CreateDefault)()
{
   return (void*)new wxAuiNotebook();
}

EWXWEXPORT(void*,wxAuiNotebook_Create)(wxWindow* _parent, int id, int _lft,int _top, int _wdt, int _hgt, long style )
{
 return (void*)new wxAuiNotebook(_parent, id,wxPoint(_lft, _top),wxSize(_wdt, _hgt),style);
}


EWXWEXPORT(bool,wxAuiNotebook_CreateFromDefault)(wxAuiNotebook* self, wxWindow* _parent, int id, int _lft,int _top, int _wdt, int _hgt, long style )
{
 return self->Create(_parent, id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), style);
}


EWXWEXPORT(bool,wxAuiNotebook_AddPageWithBitmap)(wxAuiNotebook* self, wxWindow* _page, wxString* _caption, bool select, wxBitmap* _bitmap)
{
  return self->AddPage(_page,*_caption,select,*_bitmap);
}


EWXWEXPORT(bool,wxAuiNotebook_AddPage)(wxAuiNotebook* self, wxWindow* _page, wxString* _text, bool select, int imageId )
{
  return self->AddPage(_page, *_text, select, imageId);
}


EWXWEXPORT(void,wxAuiNotebook_AdvanceSelection)(wxAuiNotebook* self, bool forward)
{
   self->AdvanceSelection(forward);
}


EWXWEXPORT(int,wxAuiNotebook_ChangeSelection)(wxAuiNotebook* self, size_t n)
{
  return self->ChangeSelection(n);
}


EWXWEXPORT(bool,wxAuiNotebook_DeleteAllPages)(wxAuiNotebook* self)
{
   return self->DeleteAllPages();
}


EWXWEXPORT(bool,wxAuiNotebook_DeletePage)(wxAuiNotebook* self, size_t page)
{
  return self->DeletePage(page);
}


EWXWEXPORT(void*,wxAuiNotebook_GetArtProvider)(wxAuiNotebook* self)
{
   return (void*)self->GetArtProvider();
}


EWXWEXPORT(void*,wxAuiNotebook_GetCurrentPage)(wxAuiNotebook* self)
{
   return (void*)self->GetCurrentPage();
}


EWXWEXPORT(int,wxAuiNotebook_GetHeightForPageHeight)(wxAuiNotebook* self, int pageHeight)
{
   return self->GetHeightForPageHeight(pageHeight);
}

EWXWEXPORT(void*,wxAuiNotebook_GetPage)(wxAuiNotebook* self, size_t page_idx)
{
 return (void*)self->GetPage(page_idx);
}

EWXWEXPORT(void,wxAuiNotebook_GetPageBitmap)(wxAuiNotebook* self, size_t page, wxBitmap* bmp)
{
    *bmp = self->GetPageBitmap(page);
}


EWXWEXPORT(size_t,wxAuiNotebook_GetPageCount)(wxAuiNotebook* self)
{
  return self->GetPageCount();
}


EWXWEXPORT(int,wxAuiNotebook_GetPageIndex)(wxAuiNotebook* self, wxWindow* _page_wnd)
{
  return self->GetPageIndex(_page_wnd);
}

EWXWEXPORT(wxString*,wxAuiNotebook_GetPageText)(wxAuiNotebook* self, size_t page )
{
	wxString *result = new wxString();
	*result = self->GetPageText(page);
	return result;
}

EWXWEXPORT(wxString*,wxAuiNotebook_GetPageToolTip)(wxAuiNotebook* self, size_t pageIdx )
{
	wxString *result = new wxString();
	*result = self->GetPageToolTip(pageIdx);
	return result;
}

EWXWEXPORT(int,wxAuiNotebook_GetSelection)(wxAuiNotebook* self)
{
   return self->GetSelection();
}

EWXWEXPORT(int,wxAuiNotebook_GetTabCtrlHeight)(wxAuiNotebook* self)
{
   return self->GetTabCtrlHeight();
}

EWXWEXPORT(bool,wxAuiNotebook_InsertPageWithBitmap)(wxAuiNotebook* self, size_t page_idx, wxWindow* _page, wxString* _caption, bool select, wxBitmap* _bitmap )
{
 return self->InsertPage(page_idx, _page, *_caption, select, *_bitmap );
}

EWXWEXPORT(bool,wxAuiNotebook_InsertPage)(wxAuiNotebook* self, size_t index, wxWindow* _page, wxString* _text, bool select, int imageId )
{
 return self->InsertPage(index, _page, *_text, select, imageId );
}


EWXWEXPORT(bool,wxAuiNotebook_RemovePage)(wxAuiNotebook* self, size_t page)
{
  return self->RemovePage(page);
}


EWXWEXPORT(void,wxAuiNotebook_SetArtProvider)(wxAuiNotebook* self, wxAuiTabArt* _art)
{
   self->SetArtProvider(_art);
}


EWXWEXPORT(bool,wxAuiNotebook_SetFont)(wxAuiNotebook* self, wxFont* _font)
{
  return self->SetFont(*_font);
}


EWXWEXPORT(void,wxAuiNotebook_SetMeasuringFont)(wxAuiNotebook* self, wxFont* font)
{
    self->SetMeasuringFont(*font);
}


EWXWEXPORT(void,wxAuiNotebook_SetNormalFont)(wxAuiNotebook* self, wxFont* font)
{
    self->SetNormalFont(*font);
}


EWXWEXPORT(bool,wxAuiNotebook_SetPageBitmap)(wxAuiNotebook* self, size_t page, wxBitmap* _bitmap)
{
  return self->SetPageBitmap(page, *_bitmap);
}


EWXWEXPORT(bool,wxAuiNotebook_SetPageImage)(wxAuiNotebook* self, size_t n, int imageId)
{
 return self->SetPageImage(n, imageId);
}


EWXWEXPORT(bool,wxAuiNotebook_SetPageText)(wxAuiNotebook* self, size_t page, wxString* _text)
{
  return self->SetPageText(page, *_text);
}


EWXWEXPORT(bool,wxAuiNotebook_SetPageToolTip)(wxAuiNotebook* self, size_t page, wxString* _text)
{
  return self->SetPageToolTip(page, *_text);
}


EWXWEXPORT(void,wxAuiNotebook_SetSelectedFont)(wxAuiNotebook* self, wxFont* font)
{
    self->SetSelectedFont(*font);
}


EWXWEXPORT(size_t,wxAuiNotebook_SetSelection)(wxAuiNotebook* self, size_t new_page)
{
  return self->SetSelection(new_page);
}


EWXWEXPORT(void,wxAuiNotebook_SetTabCtrlHeight)(wxAuiNotebook* self, int height)
{
    self->SetTabCtrlHeight(height);
}


EWXWEXPORT(bool,wxAuiNotebook_ShowWindowMenu)(wxAuiNotebook* self)
{
   return self->ShowWindowMenu();
}


EWXWEXPORT(void,wxAuiNotebook_SetUniformBitmapSize)(wxAuiNotebook* self, int _wdt, int _hgt)
{
  self->SetUniformBitmapSize(wxSize(_wdt, _hgt));
}


EWXWEXPORT(void,wxAuiNotebook_Split)(wxAuiNotebook* self, size_t page, int direction)
{
  self->Split(page, direction);
}

/* wxAuiNotebookEvent */

EWXWEXPORT(void*,wxAuiNotebookEvent_Create)(int commandType, int winid)
{
   return (void*)new wxAuiNotebookEvent(commandType, winid);
}

EWXWEXPORT(wxAuiNotebook*,wxAuiNotebookEvent_GetDragSource)(wxAuiNotebookEvent* self)
{
   return self->GetDragSource();
}

/* wxBookCtrlEvent */

EWXWEXPORT(void*,wxBookCtrlEvent_Create)(int commandType, int winid, int nSel, int nOldSel)
{
   return (void*)new wxBookCtrlEvent(commandType, winid, nSel, nOldSel);
}

EWXWEXPORT(int,wxBookCtrlEvent_GetSelection)(wxBookCtrlEvent* self)
{
   return self->GetSelection();
}

EWXWEXPORT(int,wxBookCtrlEvent_GetOldSelection)(wxBookCtrlEvent* self)
{
   return self->GetOldSelection();
}

/* wxAuiTabContainerButton */

EWXWEXPORT(int,wxAuiTabContainerButton_Id)(wxAuiTabContainerButton* self)
{
   return self->id;
}


EWXWEXPORT(int,wxAuiTabContainerButton_CurState)(wxAuiTabContainerButton* self)
{
   return self->curState;
}


EWXWEXPORT(int,wxAuiTabContainerButton_Location)(wxAuiTabContainerButton* self)
{
   return self->location;
}


EWXWEXPORT(void,wxAuiTabContainerButton_Bitmap)(wxAuiTabContainerButton* self, wxBitmap* _bmp)
{
   *_bmp = self->bitmap;
}


EWXWEXPORT(void,wxAuiTabContainerButton_DisBitmap)(wxAuiTabContainerButton* self, wxBitmap* _bmp)
{
   *_bmp = self->disBitmap;
}

EWXWEXPORT(wxRect*,wxAuiTabContainerButton_Rect)(wxAuiTabContainerButton* self)
{
	wxRect* rect = new wxRect();
	*rect = self->rect;
	return rect;
}


/* wxAuiTabContainer */

EWXWEXPORT(void*,wxAuiTabContainer_Create)()
{
   return (void*)new wxAuiTabContainer();
}

EWXWEXPORT(void,wxAuiTabContainer_SetArtProvider)(wxAuiTabContainer* self, wxAuiTabArt* _art)
{
   self->SetArtProvider(_art);
}


EWXWEXPORT(void*,wxAuiTabContainer_GetArtProvider)(wxAuiTabContainer* self)
{
   return (void*)self->GetArtProvider();
}


EWXWEXPORT(void,wxAuiTabContainer_SetFlags)(wxAuiTabContainer* self, int _flags)
{
    self->SetFlags(_flags);
}


EWXWEXPORT(int,wxAuiTabContainer_GetFlags)(wxAuiTabContainer* self)
{
 return self->GetFlags();
}


EWXWEXPORT(bool,wxAuiTabContainer_AddPage)(wxAuiTabContainer* self, wxWindow* _page, wxAuiNotebookPage* _info)
{
  return self->AddPage(_page,*_info);
}


EWXWEXPORT(bool,wxAuiTabContainer_InsertPage)(wxAuiTabContainer* self, wxWindow* _page, wxAuiNotebookPage* _info, size_t idx)
{
  return self->InsertPage(_page,*_info,idx);
}


EWXWEXPORT(bool,wxAuiTabContainer_MovePage)(wxAuiTabContainer* self, wxWindow* _page, size_t newIdx)
{
  return self->MovePage(_page,newIdx);
}


EWXWEXPORT(bool,wxAuiTabContainer_RemovePage)(wxAuiTabContainer* self, wxWindow* _page)
{
  return self->RemovePage(_page);
}


EWXWEXPORT(bool,wxAuiTabContainer_SetActivePageByWindow)(wxAuiTabContainer* self, wxWindow* _page)
{
  return self->SetActivePage(_page);
}


EWXWEXPORT(bool,wxAuiTabContainer_SetActivePage)(wxAuiTabContainer* self, size_t page)
{
  return self->SetActivePage(page);
}


EWXWEXPORT(void,wxAuiTabContainer_SetNoneActive)(wxAuiTabContainer* self)
{
    self->SetNoneActive();
}


EWXWEXPORT(int,wxAuiTabContainer_GetActivePage)(wxAuiTabContainer* self)
{
   return self->GetActivePage();
}

/*
EWXWEXPORT(bool,wxAuiTabContainer_TabHitTest)(wxAuiTabContainer* self, int x, int y, wxWindow** _hit)
{
  return self->TabHitTest(x,y,_hit);
}
*/
/*
EWXWEXPORT(bool,wxAuiTabContainer_ButtonHitTest)(wxAuiTabContainer* self, int x, int y, wxAuiTabContainerButton** _hit)
{
  return self->ButtonHitTest(x,y,_hit);
}
*/
EWXWEXPORT(void*,wxAuiTabContainer_GetWindowFromIdx)(wxAuiTabContainer* self, size_t idx)
{
 return (void*)self->GetWindowFromIdx(idx);
}


EWXWEXPORT(int,wxAuiTabContainer_GetIdxFromWindow)(wxAuiTabContainer* self, wxWindow* _page)
{
  return self->GetIdxFromWindow(_page);
}


EWXWEXPORT(size_t,wxAuiTabContainer_GetPageCount)(wxAuiTabContainer* self)
{
  return self->GetPageCount();
}

EWXWEXPORT(wxAuiNotebookPage*,wxAuiTabContainer_GetPage)(wxAuiTabContainer* self, size_t idx)
{
  wxAuiNotebookPage* page = new wxAuiNotebookPage();
  *page = self->GetPage(idx);
  return page;
}


EWXWEXPORT(wxAuiNotebookPageArray*,wxAuiTabContainer_GetPages)(wxAuiTabContainer* self)
{
  wxAuiNotebookPageArray* pages = new wxAuiNotebookPageArray();
  *pages = self->GetPages();
  return pages;
}


EWXWEXPORT(void,wxAuiTabContainer_SetNormalFont)(wxAuiTabContainer* self, wxFont* normalFont)
{
    self->SetNormalFont(*normalFont);
}


EWXWEXPORT(void,wxAuiTabContainer_SetSelectedFont)(wxAuiTabContainer* self, wxFont* selectedFont)
{
    self->SetSelectedFont(*selectedFont);
}


EWXWEXPORT(void,wxAuiTabContainer_SetMeasuringFont)(wxAuiTabContainer* self, wxFont* measuringFont)
{
    self->SetMeasuringFont(*measuringFont);
}


EWXWEXPORT(void,wxAuiTabContainer_SetColour)(wxAuiTabContainer* self, wxColour* _colour)
{
    self->SetColour(*_colour);
}


EWXWEXPORT(void,wxAuiTabContainer_SetActiveColour)(wxAuiTabContainer* self, wxColour* _colour)
{
    self->SetActiveColour(*_colour);
}


EWXWEXPORT(void,wxAuiTabContainer_DoShowHide)(wxAuiTabContainer* self)
{
    self->DoShowHide();
}


EWXWEXPORT(void,wxAuiTabContainer_SetRect)(wxAuiTabContainer* self, wxRect* _rect)
{
  self->SetRect(*_rect);
}


EWXWEXPORT(void,wxAuiTabContainer_RemoveButton)(wxAuiTabContainer* self, int id)
{
    self->RemoveButton(id);
}


EWXWEXPORT(void,wxAuiTabContainer_AddButton)(wxAuiTabContainer* self, int id, int location, wxBitmap* _normalBitmap, wxBitmap* _disabledBitmap)
{
  self->AddButton(id,location,*_normalBitmap,*_disabledBitmap);
}


EWXWEXPORT(size_t,wxAuiTabContainer_GetTabOffset)(wxAuiTabContainer* self)
{
  return self->GetTabOffset();
}


EWXWEXPORT(void,wxAuiTabContainer_SetTabOffset)(wxAuiTabContainer* self, size_t offset)
{
  self->SetTabOffset(offset);
}


EWXWEXPORT(bool,wxAuiTabContainer_IsTabVisible)(wxAuiTabContainer* self, int tabPage, int tabOffset, wxDC* _dc, wxWindow* _wnd)
{
  return self->IsTabVisible(tabPage,tabOffset,_dc,_wnd);
}


EWXWEXPORT(void,wxAuiTabContainer_MakeTabVisible)(wxAuiTabContainer* self, int tabPage, wxWindow* _win)
{
  self->MakeTabVisible(tabPage,_win);
}

/* wxAuiTabCtrl */

EWXWEXPORT(void,wxAuiTabCtrl_SetArtProvider)(wxAuiTabCtrl* self, wxAuiTabArt* _art)
{
   self->SetArtProvider(_art);
}


EWXWEXPORT(void*,wxAuiTabCtrl_GetArtProvider)(wxAuiTabCtrl* self)
{
   return (void*)self->GetArtProvider();
}


EWXWEXPORT(void,wxAuiTabCtrl_SetFlags)(wxAuiTabCtrl* self, int _flags)
{
    self->SetFlags(_flags);
}


EWXWEXPORT(int,wxAuiTabCtrl_GetFlags)(wxAuiTabCtrl* self)
{
 return self->GetFlags();
}


EWXWEXPORT(bool,wxAuiTabCtrl_AddPage)(wxAuiTabCtrl* self, wxWindow* _page, wxAuiNotebookPage* _info)
{
  return self->AddPage(_page,*_info);
}


EWXWEXPORT(bool,wxAuiTabCtrl_InsertPage)(wxAuiTabCtrl* self, wxWindow* _page, wxAuiNotebookPage* _info, size_t idx)
{
  return self->InsertPage(_page,*_info,idx);
}


EWXWEXPORT(bool,wxAuiTabCtrl_MovePage)(wxAuiTabCtrl* self, wxWindow* _page, size_t newIdx)
{
  return self->MovePage(_page,newIdx);
}


EWXWEXPORT(bool,wxAuiTabCtrl_RemovePage)(wxAuiTabCtrl* self, wxWindow* _page)
{
  return self->RemovePage(_page);
}


EWXWEXPORT(bool,wxAuiTabCtrl_SetActivePageByWindow)(wxAuiTabCtrl* self, wxWindow* _page)
{
  return self->SetActivePage(_page);
}


EWXWEXPORT(bool,wxAuiTabCtrl_SetActivePage)(wxAuiTabCtrl* self, size_t page)
{
  return self->SetActivePage(page);
}


EWXWEXPORT(void,wxAuiTabCtrl_SetNoneActive)(wxAuiTabCtrl* self)
{
    self->SetNoneActive();
}


EWXWEXPORT(int,wxAuiTabCtrl_GetActivePage)(wxAuiTabCtrl* self)
{
   return self->GetActivePage();
}

/*
EWXWEXPORT(bool,wxAuiTabCtrl_TabHitTest)(wxAuiTabCtrl* self, int x, int y, wxWindow** _hit)
{
  return self->TabHitTest(x,y,_hit);
}
*/
/*
EWXWEXPORT(bool,wxAuiTabCtrl_ButtonHitTest)(wxAuiTabCtrl* self, int x, int y, wxAuiTabContainerButton** _hit)
{
  return self->ButtonHitTest(x,y,_hit);
}
*/
EWXWEXPORT(void*,wxAuiTabCtrl_GetWindowFromIdx)(wxAuiTabCtrl* self, size_t idx)
{
 return (void*)self->GetWindowFromIdx(idx);
}


EWXWEXPORT(int,wxAuiTabCtrl_GetIdxFromWindow)(wxAuiTabCtrl* self, wxWindow* _page)
{
  return self->GetIdxFromWindow(_page);
}


EWXWEXPORT(size_t,wxAuiTabCtrl_GetPageCount)(wxAuiTabCtrl* self)
{
  return self->GetPageCount();
}

EWXWEXPORT(wxAuiNotebookPage*,wxAuiTabCtrl_GetPage)(wxAuiTabCtrl* self, size_t idx)
{
  wxAuiNotebookPage* page = new wxAuiNotebookPage();
  *page = self->GetPage(idx);
  return page;
}


EWXWEXPORT(wxAuiNotebookPageArray*,wxAuiTabCtrl_GetPages)(wxAuiTabCtrl* self)
{
  wxAuiNotebookPageArray* pages = new wxAuiNotebookPageArray();
  *pages = self->GetPages();
  return pages;
}


EWXWEXPORT(void,wxAuiTabCtrl_SetNormalFont)(wxAuiTabCtrl* self, wxFont* normalFont)
{
    self->SetNormalFont(*normalFont);
}


EWXWEXPORT(void,wxAuiTabCtrl_SetSelectedFont)(wxAuiTabCtrl* self, wxFont* selectedFont)
{
    self->SetSelectedFont(*selectedFont);
}


EWXWEXPORT(void,wxAuiTabCtrl_SetMeasuringFont)(wxAuiTabCtrl* self, wxFont* measuringFont)
{
    self->SetMeasuringFont(*measuringFont);
}


EWXWEXPORT(void,wxAuiTabCtrl_SetColour)(wxAuiTabCtrl* self, wxColour* _colour)
{
    self->SetColour(*_colour);
}


EWXWEXPORT(void,wxAuiTabCtrl_SetActiveColour)(wxAuiTabCtrl* self, wxColour* _colour)
{
    self->SetActiveColour(*_colour);
}


EWXWEXPORT(void,wxAuiTabCtrl_DoShowHide)(wxAuiTabCtrl* self)
{
    self->DoShowHide();
}


EWXWEXPORT(void,wxAuiTabCtrl_SetRect)(wxAuiTabCtrl* self, wxRect* _rect)
{
  self->SetRect(*_rect);
}


EWXWEXPORT(void,wxAuiTabCtrl_RemoveButton)(wxAuiTabCtrl* self, int id)
{
    self->RemoveButton(id);
}


EWXWEXPORT(void,wxAuiTabCtrl_AddButton)(wxAuiTabCtrl* self, int id, int location, wxBitmap* _normalBitmap, wxBitmap* _disabledBitmap)
{
  self->AddButton(id,location,*_normalBitmap,*_disabledBitmap);
}


EWXWEXPORT(size_t,wxAuiTabCtrl_GetTabOffset)(wxAuiTabCtrl* self)
{
  return self->GetTabOffset();
}


EWXWEXPORT(void,wxAuiTabCtrl_SetTabOffset)(wxAuiTabCtrl* self, size_t offset)
{
  self->SetTabOffset(offset);
}


EWXWEXPORT(bool,wxAuiTabCtrl_IsTabVisible)(wxAuiTabCtrl* self, int tabPage, int tabOffset, wxDC* _dc, wxWindow* _wnd)
{
  return self->IsTabVisible(tabPage,tabOffset,_dc,_wnd);
}


EWXWEXPORT(void,wxAuiTabCtrl_MakeTabVisible)(wxAuiTabCtrl* self, int tabPage, wxWindow* _win)
{
  self->MakeTabVisible(tabPage,_win);
}


/* wxAuiTabArt */


EWXWEXPORT(void*,wxAuiTabArt_Clone)(wxAuiTabArt* self)
{
   return (void*)self->Clone();
}


EWXWEXPORT(void,wxAuiTabArt_DrawBackground)(wxAuiTabArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawBackground(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiTabArt_DrawButton)(wxAuiTabArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _in_rect, int bitmap_id, int button_state, int orientation, wxRect* _out_rect)
{
 self->DrawButton(*_dc, _wnd, *_in_rect, bitmap_id, button_state, orientation, _out_rect);
}


EWXWEXPORT(void,wxAuiTabArt_DrawTab)(wxAuiTabArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiNotebookPage* _page, wxRect* _rect, int close_button_state, wxRect* _out_tab_rect, wxRect* _out_button_rect, int* x_extent )
{
 self->DrawTab(*_dc, _wnd, *_page, *_rect, close_button_state, _out_tab_rect, _out_button_rect, x_extent );
}


EWXWEXPORT(int,wxAuiTabArt_GetBestTabCtrlSize)(wxAuiTabArt* self, wxWindow* _wnd, wxAuiNotebookPageArray* _pages, int _wdt, int _hgt)
{
  return self->GetBestTabCtrlSize(_wnd,*_pages,wxSize(_wdt, _hgt));
}


EWXWEXPORT(int,wxAuiTabArt_GetIndentSize)(wxAuiTabArt* self)
{
   return self->GetIndentSize();
}

EWXWEXPORT(wxSize*,wxAuiTabArt_GetTabSize)(wxAuiTabArt* self, wxDC* _dc, wxWindow* _wnd, wxString* _caption, wxBitmap* _bitmap, bool active, int close_button_state, int *x_extent )
{
	wxSize* sz = new wxSize();
	*sz = self->GetTabSize(*_dc,_wnd,*_caption, *_bitmap,active,close_button_state,x_extent );
	return sz;
}

EWXWEXPORT(void,wxAuiTabArt_SetFlags)(wxAuiTabArt* self, int _flags)
{
    self->SetFlags(_flags);
}


EWXWEXPORT(void,wxAuiTabArt_SetMeasuringFont)(wxAuiTabArt* self, wxFont* font)
{
    self->SetMeasuringFont(*font);
}


EWXWEXPORT(void,wxAuiTabArt_SetNormalFont)(wxAuiTabArt* self, wxFont* font)
{
    self->SetNormalFont(*font);
}


EWXWEXPORT(void,wxAuiTabArt_SetSelectedFont)(wxAuiTabArt* self, wxFont* font)
{
    self->SetSelectedFont(*font);
}


EWXWEXPORT(void,wxAuiTabArt_SetColour)(wxAuiTabArt* self, wxColour* _colour)
{
    self->SetColour(*_colour);
}


EWXWEXPORT(void,wxAuiTabArt_SetActiveColour)(wxAuiTabArt* self, wxColour* _colour)
{
    self->SetActiveColour(*_colour);
}


EWXWEXPORT(void,wxAuiTabArt_SetSizingInfo)(wxAuiTabArt* self, int _wdt, int _hgt, size_t tab_count)
{
  self->SetSizingInfo(wxSize(_wdt, _hgt),tab_count);
}

/* wxAuiTabCtrll */




/* wxAuiSimpleTabArt */

EWXWEXPORT(void*,wxAuiSimpleTabArt_Create)()
{
   return (void*)new wxAuiSimpleTabArt();
}


EWXWEXPORT(void*,wxAuiSimpleTabArt_Clone)(wxAuiSimpleTabArt* self)
{
   return (void*)self->Clone();
}


EWXWEXPORT(void,wxAuiSimpleTabArt_SetFlags)(wxAuiSimpleTabArt* self, int _flags)
{
    self->SetFlags(_flags);
}


EWXWEXPORT(void,wxAuiSimpleTabArt_SetSizingInfo)(wxAuiSimpleTabArt* self, int _wdt, int _hgt, size_t tabCount)
{
  self->SetSizingInfo(wxSize(_wdt, _hgt),tabCount);
}


EWXWEXPORT(void,wxAuiSimpleTabArt_SetNormalFont)(wxAuiSimpleTabArt* self, wxFont* font)
{
    self->SetNormalFont(*font);
}


EWXWEXPORT(void,wxAuiSimpleTabArt_SetSelectedFont)(wxAuiSimpleTabArt* self, wxFont* font)
{
    self->SetSelectedFont(*font);
}


EWXWEXPORT(void,wxAuiSimpleTabArt_SetMeasuringFont)(wxAuiSimpleTabArt* self, wxFont* font)
{
    self->SetMeasuringFont(*font);
}


EWXWEXPORT(void,wxAuiSimpleTabArt_SetColour)(wxAuiSimpleTabArt* self, wxColour* _colour)
{
    self->SetColour(*_colour);
}


EWXWEXPORT(void,wxAuiSimpleTabArt_SetActiveColour)(wxAuiSimpleTabArt* self, wxColour* _colour)
{
    self->SetActiveColour(*_colour);
}


EWXWEXPORT(void,wxAuiSimpleTabArt_DrawBackground)(wxAuiSimpleTabArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _rect)
{
  return self->DrawBackground(*_dc,_wnd,*_rect);
}


EWXWEXPORT(void,wxAuiSimpleTabArt_DrawTab)(wxAuiSimpleTabArt* self, wxDC* _dc, wxWindow* _wnd, wxAuiNotebookPage* _pane, wxRect* _inRect, int closeButtonState, wxRect* _outTabRect, wxRect* _outButtonRect, int* xExtent )
{
 self->DrawTab(*_dc, _wnd, *_pane, *_inRect, closeButtonState, _outTabRect, _outButtonRect, xExtent );
}


EWXWEXPORT(void,wxAuiSimpleTabArt_DrawButton)(wxAuiSimpleTabArt* self, wxDC* _dc, wxWindow* _wnd, wxRect* _inRect, int bitmapId, int buttonState, int orientation, wxRect* _outRect)
{
 self->DrawButton(*_dc, _wnd, *_inRect, bitmapId, buttonState, orientation, _outRect);
}


EWXWEXPORT(int,wxAuiSimpleTabArt_GetIndentSize)(wxAuiSimpleTabArt* self)
{
   return self->GetIndentSize();
}

EWXWEXPORT(wxSize*,wxAuiSimpleTabArt_GetTabSize)(wxAuiSimpleTabArt* self, wxDC* _dc, wxWindow* _wnd, wxString* _caption, wxBitmap* _bitmap, bool active, int close_button_state, int *x_extent )
{
	wxSize* sz = new wxSize();
	*sz = self->GetTabSize(*_dc,_wnd,*_caption, *_bitmap,active,close_button_state,x_extent );
	return sz;
}

EWXWEXPORT(int,wxAuiSimpleTabArt_ShowDropDown)(wxAuiSimpleTabArt* self, wxWindow* _wnd, wxAuiNotebookPageArray* _items, int activeIdx)
{
  return self->ShowDropDown(_wnd,*_items,activeIdx);
}


EWXWEXPORT(int,wxAuiSimpleTabArt_GetBestTabCtrlSize)(wxAuiSimpleTabArt* self, wxWindow* _wnd, wxAuiNotebookPageArray* _pages, int _wdt, int _hgt)
{
  return self->GetBestTabCtrlSize(_wnd,*_pages,wxSize(_wdt, _hgt));
}



/* wxAuiManager - public member */

EWXWEXPORT(void*,wxAuiManager_Create)(wxWindow* _managed_wnd, int _flags)
{
 return (void*) new wxAuiManager(_managed_wnd, _flags);
}

EWXWEXPORT(bool,wxAuiManager_DetachPane)(wxAuiManager* self, wxWindow* _window)
{
  return self->DetachPane(_window);
}


EWXWEXPORT(wxAuiPaneInfoArray,wxAuiManager_GetAllPanes)(wxAuiManager* self)
{
   return self->GetAllPanes();
}


EWXWEXPORT(void*,wxAuiManager_GetArtProvider)(wxAuiManager* self)
{
   return (void*)self->GetArtProvider();
}

EWXWEXPORT(void,wxAuiManager_GetDockSizeConstraint)(wxAuiManager* self, double* widthpct, double* heightpct)
{
    self->GetDockSizeConstraint(widthpct, heightpct);
}

EWXWEXPORT(int,wxAuiManager_GetFlags)(wxAuiManager* self)
{
 return self->GetFlags();
}


EWXWEXPORT(void*,wxAuiManager_GetManagedWindow)(wxAuiManager* self)
{
   return (void*)self->GetManagedWindow();
}


EWXWEXPORT(void,wxAuiManager_HideHint)(wxAuiManager* self)
{
    self->HideHint();
}


EWXWEXPORT(bool,wxAuiManager_InsertPane)(wxAuiManager* self, wxWindow* _window, wxAuiPaneInfo* _insert_location, int _insert_level)
{
  return self->InsertPane(_window,*_insert_location,_insert_level);
}


EWXWEXPORT(void,wxAuiManager_LoadPaneInfo)(wxAuiManager* self, wxString* _pane_part, wxAuiPaneInfo* _pane)
{
  self->LoadPaneInfo(*_pane_part,*_pane);
}


EWXWEXPORT(bool,wxAuiManager_LoadPerspective)(wxAuiManager* self, wxString* _perspective, bool update)
{
  return self->LoadPerspective(*_perspective,update);
}

EWXWEXPORT(wxString*,wxAuiManager_SavePaneInfo)(wxAuiManager* self, wxAuiPaneInfo* _pane )
{
	wxString *result = new wxString();
	*result = self->SavePaneInfo(*_pane);
	return result;
}

EWXWEXPORT(wxString*,wxAuiManager_SavePerspective)(wxAuiManager* self)
{
	wxString *result = new wxString();
	*result = self->SavePerspective();
	return result;
}

EWXWEXPORT(void,wxAuiManager_SetArtProvider)(wxAuiManager* self, wxAuiDockArt* _art_provider)
{
   self->SetArtProvider(_art_provider);
}


EWXWEXPORT(void,wxAuiManager_SetDockSizeConstraint)(wxAuiManager* self, double widthpct, double heightpct)
{
    self->SetDockSizeConstraint(widthpct, heightpct);
}


EWXWEXPORT(void,wxAuiManager_SetFlags)(wxAuiManager* self, int flags)
{
    self->SetFlags(flags);
}


EWXWEXPORT(void,wxAuiManager_SetManagedWindow)(wxAuiManager* self, wxWindow* _managed_wnd)
{
   self->SetManagedWindow(_managed_wnd);
}


EWXWEXPORT(void,wxAuiManager_ShowHint)(wxAuiManager* self, wxRect* _rect)
{
  self->ShowHint(*_rect);
}


EWXWEXPORT(void,wxAuiManager_UnInit)(wxAuiManager* self)
{
    self->UnInit();
}


EWXWEXPORT(void,wxAuiManager_Update)(wxAuiManager* self)
{
    self->Update();
}



EWXWEXPORT(bool,wxAuiManager_AddPane)(wxAuiManager* self, wxWindow* _window, int _direction, wxString* _caption)
{
  return self->AddPane(_window,_direction,*_caption);
}


EWXWEXPORT(bool,wxAuiManager_AddPaneByPaneInfo)(wxAuiManager* self, wxWindow* _window, wxAuiPaneInfo* _pane_info)
{
  return self->AddPane(_window,*_pane_info);
}


EWXWEXPORT(bool,wxAuiManager_AddPaneByPaneInfoAndDropPosition)(wxAuiManager* self, wxWindow* _window, wxAuiPaneInfo* _pane_info, int _lft,int _top)
{
  return self->AddPane(_window,*_pane_info,wxPoint(_lft, _top));
}

EWXWEXPORT(wxAuiPaneInfo*,wxAuiManager_GetPaneByWindow)(wxAuiManager* self, wxWindow* _window)
{
  wxAuiPaneInfo* info = new wxAuiPaneInfo();
  *info = self->GetPane(_window);
  return info;
}

EWXWEXPORT(wxAuiPaneInfo*,wxAuiManager_GetPaneByName)(wxAuiManager* self, wxString* id)
{
   wxAuiPaneInfo* info = new wxAuiPaneInfo();
  *info = self->GetPane(*id);
  return info;
}

EWXWEXPORT(void*,wxAuiManager_GetManager)(wxWindow* _window)
{
 return wxAuiManager::GetManager(_window);
}
 

/* wxAuiDockArt */

EWXWEXPORT(void,wxAuiDockArt_DrawBackground)(wxAuiDockArt* self, wxDC* _dc, wxWindow* _window, int orientation, wxRect* _rect)
{
  self->DrawBackground(*_dc,_window,orientation,*_rect);
}


EWXWEXPORT(void,wxAuiDockArt_DrawBorder)(wxAuiDockArt* self, wxDC* _dc, wxWindow* _window, wxRect* _rect, wxAuiPaneInfo* _pane)
{
  self->DrawBorder(*_dc,_window,*_rect,*_pane);
}


EWXWEXPORT(void,wxAuiDockArt_DrawCaption)(wxAuiDockArt* self, wxDC* _dc, wxWindow* _window, wxString* _text, wxRect* _rect, wxAuiPaneInfo* _pane)
{
 self->DrawCaption(*_dc, _window, *_text, *_rect, *_pane);
}


EWXWEXPORT(void,wxAuiDockArt_DrawGripper)(wxAuiDockArt* self, wxDC* _dc, wxWindow* _window, wxRect* _rect, wxAuiPaneInfo* _pane)
{
  self->DrawGripper(*_dc,_window,*_rect,*_pane);
}


EWXWEXPORT(void,wxAuiDockArt_DrawPaneButton)(wxAuiDockArt* self, wxDC* _dc, wxWindow* _window, int button, int button_state, wxRect* _rect, wxAuiPaneInfo* _pane)
{
 self->DrawPaneButton(*_dc, _window, button, button_state, *_rect, *_pane);
}


EWXWEXPORT(void,wxAuiDockArt_DrawSash)(wxAuiDockArt* self, wxDC* _dc, wxWindow* _window, int orientation, wxRect* _rect)
{
  self->DrawSash(*_dc,_window,orientation,*_rect);
}

EWXWEXPORT(wxColour*,wxAuiDockArt_GetColour)(wxAuiDockArt* self, int id)
{
	wxColour* colr = new wxColour();
	*colr = self->GetColour(id);
	return colr;
}

EWXWEXPORT(wxFont*,wxAuiDockArt_GetFont)(wxAuiDockArt* self, int id)
{
	wxFont* font = new wxFont();
	*font = self->GetFont(id);
	return font;
}

EWXWEXPORT(int,wxAuiDockArt_GetMetric)(wxAuiDockArt* self, int id)
{
   return self->GetMetric(id);
}


EWXWEXPORT(void,wxAuiDockArt_SetColour)(wxAuiDockArt* self, int id, wxColour* _colour)
{
  self->SetColour(id,*_colour);
}


EWXWEXPORT(void,wxAuiDockArt_SetFont)(wxAuiDockArt* self, int id, wxFont* _font)
{
  self->SetFont(id,*_font);
}


EWXWEXPORT(void,wxAuiDockArt_SetMetric)(wxAuiDockArt* self, int id, int new_val)
{
    self->SetMetric(id, new_val);
}


/* wxAuiPaneInfo */

EWXWEXPORT(void*,wxAuiPaneInfo_CreateDefault)()
{
   return (void*)new wxAuiPaneInfo();
}


EWXWEXPORT(void*,wxAuiPaneInfo_Create)(wxAuiPaneInfo* _c)
{
 return (void*) new wxAuiPaneInfo(*_c);
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Bottom)(wxAuiPaneInfo* self)
{
    wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Bottom();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_BottomDockable)(wxAuiPaneInfo* self, bool b)
{
  wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->BottomDockable(b);
  return out;
}

EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Caption)(wxAuiPaneInfo* self, wxString* _c)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Caption(*_c);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_CaptionVisible)(wxAuiPaneInfo* self, bool visible)
{
  wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->CaptionVisible(visible);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_CloseButton)(wxAuiPaneInfo* self, bool visible)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->CloseButton(visible);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_DefaultPane)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
*out = self->DefaultPane();
return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_DestroyOnClose)(wxAuiPaneInfo* self, bool b)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
*out = self->DestroyOnClose(b);
return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Direction)(wxAuiPaneInfo* self, int direction)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Direction(direction);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Dock)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Dock();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_DockFixed)(wxAuiPaneInfo* self, bool b)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->DockFixed(b);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Dockable)(wxAuiPaneInfo* self, bool b)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Dockable(b);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Fixed)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Fixed();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Float)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Float();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Floatable)(wxAuiPaneInfo* self, bool b)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Floatable(b);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Gripper)(wxAuiPaneInfo* self, bool visible)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Gripper(visible);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_GripperTop)(wxAuiPaneInfo* self, bool attop)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->GripperTop(attop);
  return out;
}


EWXWEXPORT(bool,wxAuiPaneInfo_HasBorder)(wxAuiPaneInfo* self)
{
   return self->HasBorder();
}


EWXWEXPORT(bool,wxAuiPaneInfo_HasCaption)(wxAuiPaneInfo* self)
{
   return self->HasCaption();
}


EWXWEXPORT(bool,wxAuiPaneInfo_HasCloseButton)(wxAuiPaneInfo* self)
{
   return self->HasCloseButton();
}


EWXWEXPORT(bool,wxAuiPaneInfo_HasFlag)(wxAuiPaneInfo* self, int flag)
{
   return self->HasFlag(flag);
}


EWXWEXPORT(bool,wxAuiPaneInfo_HasGripper)(wxAuiPaneInfo* self)
{
   return self->HasGripper();
}


EWXWEXPORT(bool,wxAuiPaneInfo_HasGripperTop)(wxAuiPaneInfo* self)
{
   return self->HasGripperTop();
}


EWXWEXPORT(bool,wxAuiPaneInfo_HasMaximizeButton)(wxAuiPaneInfo* self)
{
   return self->HasMaximizeButton();
}


EWXWEXPORT(bool,wxAuiPaneInfo_HasMinimizeButton)(wxAuiPaneInfo* self)
{
   return self->HasMinimizeButton();
}


EWXWEXPORT(bool,wxAuiPaneInfo_HasPinButton)(wxAuiPaneInfo* self)
{
   return self->HasPinButton();
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Hide)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Hide();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Icon)(wxAuiPaneInfo* self, wxBitmap* _b)
{
  wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Icon(*_b);
  return out;
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsBottomDockable)(wxAuiPaneInfo* self)
{
   return self->IsBottomDockable();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsDockable)(wxAuiPaneInfo* self)
{
   return self->IsDockable();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsDocked)(wxAuiPaneInfo* self)
{
   return self->IsDocked();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsFixed)(wxAuiPaneInfo* self)
{
   return self->IsFixed();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsFloatable)(wxAuiPaneInfo* self)
{
   return self->IsFloatable();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsFloating)(wxAuiPaneInfo* self)
{
   return self->IsFloating();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsLeftDockable)(wxAuiPaneInfo* self)
{
   return self->IsLeftDockable();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsMovable)(wxAuiPaneInfo* self)
{
   return self->IsMovable();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsOk)(wxAuiPaneInfo* self)
{
   return self->IsOk();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsResizable)(wxAuiPaneInfo* self)
{
   return self->IsResizable();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsRightDockable)(wxAuiPaneInfo* self)
{
   return self->IsRightDockable();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsShown)(wxAuiPaneInfo* self)
{
   return self->IsShown();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsToolbar)(wxAuiPaneInfo* self)
{
   return self->IsToolbar();
}


EWXWEXPORT(bool,wxAuiPaneInfo_IsTopDockable)(wxAuiPaneInfo* self)
{
   return self->IsTopDockable();
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Layer)(wxAuiPaneInfo* self, int layer)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Layer(layer);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Left)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Left();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_LeftDockable)(wxAuiPaneInfo* self, bool b)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->LeftDockable(b);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_MaximizeButton)(wxAuiPaneInfo* self, bool visible)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->MaximizeButton(visible);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_MinimizeButton)(wxAuiPaneInfo* self, bool visible)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->MinimizeButton(visible);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Movable)(wxAuiPaneInfo* self, bool b)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Movable(b);
  return out;
}

EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Name)(wxAuiPaneInfo* self, wxString* _n)
{
 wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Name(*_n);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_PaneBorder)(wxAuiPaneInfo* self, bool visible)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->PaneBorder(visible);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_PinButton)(wxAuiPaneInfo* self, bool visible)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->PinButton(visible);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Position)(wxAuiPaneInfo* self, int pos)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Position(pos);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Resizable)(wxAuiPaneInfo* self, bool resizable)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Resizable(resizable);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Right)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Right();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_RightDockable)(wxAuiPaneInfo* self, bool b)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->RightDockable(b);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Row)(wxAuiPaneInfo* self, int row)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Row(row);
  return out;
}


EWXWEXPORT(void,wxAuiPaneInfo_SafeSet)(wxAuiPaneInfo* self, wxAuiPaneInfo* source)
{
   self->SafeSet(*source);
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_SetFlag)(wxAuiPaneInfo* self, int flag, bool option_state)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->SetFlag(flag, option_state);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Show)(wxAuiPaneInfo* self, bool show)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Show(show);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_ToolbarPane)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->ToolbarPane();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Top)(wxAuiPaneInfo* self)
{
   
  wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Top();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_TopDockable)(wxAuiPaneInfo* self, bool b)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->TopDockable(b);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Window)(wxAuiPaneInfo* self, wxWindow* _w)
{
  wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Window(_w);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Copy)(wxAuiPaneInfo* self, wxAuiPaneInfo* _c)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->operator=(*_c);
  return out;
}

EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_BestSize)(wxAuiPaneInfo* self, int _wdt, int _hgt)
{
  wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->BestSize(wxSize(_wdt, _hgt));
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_BestSizeXY)(wxAuiPaneInfo* self, int x, int y)
{
  wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->BestSize(x, y);
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Centre)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Centre();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_Center)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->Center();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_CentrePane)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->CentrePane();
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_CenterPane)(wxAuiPaneInfo* self)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->CenterPane();
  return out;
}

EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_FloatingPosition)(wxAuiPaneInfo* self, int _lft,int _top)
{
 wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->FloatingPosition(wxPoint(_lft, _top));
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_FloatingPositionXY)(wxAuiPaneInfo* self, int x, int y)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->FloatingPosition(x, y);
  return out;
}

EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_FloatingSize)(wxAuiPaneInfo* self, int _wdt, int _hgt)
{
 wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->FloatingSize(wxSize(_wdt, _hgt));
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_FloatingSizeXY)(wxAuiPaneInfo* self, int x, int y)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->FloatingSize(x, y);
  return out;
}

EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_MaxSize)(wxAuiPaneInfo* self, int _wdt, int _hgt)
{
 wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->MaxSize(wxSize(_wdt, _hgt));
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_MaxSizeXY)(wxAuiPaneInfo* self, int x, int y)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->MaxSize(x, y);
  return out;
}

EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_MinSize)(wxAuiPaneInfo* self, int _wdt, int _hgt)
{
 wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->MinSize(wxSize(_wdt, _hgt));
  return out;
}


EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfo_MinSizeXY)(wxAuiPaneInfo* self, int x, int y)
{
   wxAuiPaneInfo* out = new wxAuiPaneInfo();
  *out = self->MinSize(x, y);
  return out;
}


/* wxAuiManagerEvent */

EWXWEXPORT(void*,wxAuiManagerEvent_Create)(int type)
{
   return (void*)new wxAuiManagerEvent(type);
}

EWXWEXPORT(bool,wxAuiManagerEvent_CanVeto)(wxAuiManagerEvent* self)
{
   return self->CanVeto();
}


EWXWEXPORT(int,wxAuiManagerEvent_GetButton)(wxAuiManagerEvent* self)
{
   return self->GetButton();
}

EWXWEXPORT(wxDC*,wxAuiManagerEvent_GetDC)(wxAuiManagerEvent* self)
{
	return self->GetDC();
}

EWXWEXPORT(bool,wxAuiManagerEvent_GetVeto)(wxAuiManagerEvent* self)
{
   return self->GetVeto();
}


EWXWEXPORT(void*,wxAuiManagerEvent_GetManager)(wxAuiManagerEvent* self)
{
   return (void*)self->GetManager();
}


EWXWEXPORT(void*,wxAuiManagerEvent_GetPane)(wxAuiManagerEvent* self)
{
   return (void*)self->GetPane();
}


EWXWEXPORT(void,wxAuiManagerEvent_SetButton)(wxAuiManagerEvent* self, int button)
{
    self->SetButton(button);
}


EWXWEXPORT(void,wxAuiManagerEvent_SetCanVeto)(wxAuiManagerEvent* self, bool can_veto)
{
    self->SetCanVeto(can_veto);
}


EWXWEXPORT(void,wxAuiManagerEvent_SetDC)(wxAuiManagerEvent* self, wxDC* pdc)
{
    self->SetDC(pdc);
}
 

EWXWEXPORT(void,wxAuiManagerEvent_SetManager)(wxAuiManagerEvent* self, wxAuiManager* _manager)
{
   self->SetManager(_manager);
}


EWXWEXPORT(void,wxAuiManagerEvent_SetPane)(wxAuiManagerEvent* self, wxAuiPaneInfo* _pane)
{
   self->SetPane(_pane);
}


EWXWEXPORT(void,wxAuiManagerEvent_Veto)(wxAuiManagerEvent* self, bool veto)
{
    self->Veto(veto);
}


/* wxBookCtrlBase */

EWXWEXPORT(bool,wxBookCtrlBase_CreateFromDefault)(wxAuiNotebook* self, wxWindow* _parent, int winid, int _lft,int _top, int _wdt, int _hgt, long style )
{
 return self->Create(_parent, winid, wxPoint(_lft, _top), wxSize(_wdt, _hgt), style  );
}


EWXWEXPORT(void,wxBookCtrlBase_SetPageSize)(wxBookCtrlBase* self, int _wdt, int _hgt)
{
  self->SetPageSize(wxSize(_wdt, _hgt));
}

EWXWEXPORT(int,wxBookCtrlBase_HitTest)(wxBookCtrlBase* self, int _lft,int _top, long* flags )
{
 return self->HitTest(wxPoint(_lft, _top),flags);
}


EWXWEXPORT(int,wxBookCtrlBase_GetPageImage)(wxBookCtrlBase* self, size_t nPage)
{
  return self->GetPageImage(nPage);
}


EWXWEXPORT(bool,wxBookCtrlBase_SetPageImage)(wxBookCtrlBase* self, size_t page, int image)
{
 return self->SetPageImage(page, image);
}

EWXWEXPORT(wxString*,wxBookCtrlBase_GetPageText)(wxBookCtrlBase* self, size_t nPage )
{
	wxString *result = new wxString();
	*result = self->GetPageText(nPage);
	return result;
}

EWXWEXPORT(bool,wxBookCtrlBase_SetPageText)(wxBookCtrlBase* self, size_t page, wxString* _text)
{
  return self->SetPageText(page, *_text);
}


EWXWEXPORT(int,wxBookCtrlBase_GetSelection)(wxBookCtrlBase* self)
{
   return self->GetSelection();
}


EWXWEXPORT(void*,wxBookCtrlBase_GetCurrentPage)(wxBookCtrlBase* self)
{
   return (void*)self->GetCurrentPage();
}


EWXWEXPORT(int,wxBookCtrlBase_SetSelection)(wxBookCtrlBase* self, size_t page)
{
  return self->SetSelection(page);
}


EWXWEXPORT(void,wxBookCtrlBase_AdvanceSelection)(wxBookCtrlBase* self, bool forward)
{
    self->AdvanceSelection(forward);
}


EWXWEXPORT(int,wxBookCtrlBase_ChangeSelection)(wxBookCtrlBase* self, size_t page)
{
  return self->ChangeSelection(page);
}


EWXWEXPORT(int,wxBookCtrlBase_FindPage)(wxBookCtrlBase* self, wxWindow* _page)
{
  return self->FindPage(_page);
}

EWXWEXPORT(bool,wxBookCtrlBase_AddPage)(wxBookCtrlBase* self, wxWindow* _page, wxString* _text, bool select, int imageId )
{
 return self->AddPage(_page, *_text, select, imageId );
}

EWXWEXPORT(bool,wxBookCtrlBase_DeleteAllPages)(wxBookCtrlBase* self)
{
   return self->DeleteAllPages();
}


EWXWEXPORT(bool,wxBookCtrlBase_DeletePage)(wxBookCtrlBase* self, size_t page)
{
  return self->DeletePage(page);
}


EWXWEXPORT(bool,wxBookCtrlBase_InsertPage)(wxBookCtrlBase* self, size_t index, wxWindow* _page, wxString* _text, bool select, int imageId )
{
 return self->InsertPage(index, _page, *_text, select, imageId );
}


EWXWEXPORT(bool,wxBookCtrlBase_RemovePage)(wxBookCtrlBase* self, size_t page)
{
  return self->RemovePage(page);
}


EWXWEXPORT(size_t,wxBookCtrlBase_GetPageCount)(wxBookCtrlBase* self)
{
  return self->GetPageCount();
}

EWXWEXPORT(void*,wxBookCtrlBase_GetPage)(wxBookCtrlBase* self, size_t page)
{
 return (void*)self->GetPage(page);
}


EWXWEXPORT(void,wxBookCtrlBase_AssignImageList)(wxBookCtrlBase* self, wxImageList* imageList)
{
   self->AssignImageList(imageList);
}


EWXWEXPORT(void,wxBookCtrlBase_SetImageList)(wxBookCtrlBase* self, wxImageList* imageList)
{
   self->SetImageList(imageList);
}


EWXWEXPORT(void*,wxBookCtrlBase_GetImageList)(wxBookCtrlBase* self)
{
   return (void*)self->GetImageList();
}

/** wxAuiNotebookPage **/
EWXWEXPORT(wxWindow*,wxAuiNotebookPage_Window)(wxAuiNotebookPage* self)
{
  return self->window;
}

EWXWEXPORT(wxString*,wxAuiNotebookPage_Caption)(wxAuiNotebookPage* self)
{
  wxString *result = new wxString();
  *result = self->caption;
  return result;
}

EWXWEXPORT(wxString*,wxAuiNotebookPage_Tooltip)(wxAuiNotebookPage* self)
{
  wxString *result = new wxString();
  *result = self->tooltip;
  return result;
}

EWXWEXPORT(wxBitmap*,wxAuiNotebookPage_Bitmap)(wxAuiNotebookPage* self)
{
  wxBitmap *result = new wxBitmap();
  *result = self->bitmap;
  return result;
}

EWXWEXPORT(wxRect*,wxAuiNotebookPage_Rect)(wxAuiNotebookPage* self)
{
  wxRect* rect = new wxRect();
  *rect = self->rect;
  return rect;
}

EWXWEXPORT(bool,wxAuiNotebookPage_Active)(wxAuiNotebookPage* self)
{
  return self->active;
}

/** wxAuiNotebookPageArray **/
/** see wxWidgets dynarray.h for additional array functions **/

EWXWEXPORT(wxAuiNotebookPageArray*,wxAuiNotebookPageArray_Create)()
{
  return new wxAuiNotebookPageArray();
}

EWXWEXPORT(void,wxAuiNotebookPageArray_Delete)(wxAuiNotebookPageArray* self)
{
  delete self;
}

EWXWEXPORT(int,wxAuiNotebookPageArray_GetCount)(wxAuiNotebookPageArray* self)
{
  return self->GetCount();
}

EWXWEXPORT(wxAuiNotebookPage*,wxAuiNotebookPageArray_Item)(wxAuiNotebookPageArray* self,int _idx)
{
  wxAuiNotebookPage* page = new wxAuiNotebookPage();
  *page = self->Item(_idx);
  return page;
}

/** wxAuiToolBarItemArray **/
/** see wxWidgets dynarray.h for additional array functions **/
EWXWEXPORT(wxAuiToolBarItemArray*,wxAuiToolBarItemArray_Create)()
{
  return new wxAuiToolBarItemArray();
}

EWXWEXPORT(void,wxAuiToolBarItemArray_Delete)(wxAuiToolBarItemArray* self)
{
  delete self;
}

EWXWEXPORT(int,wxAuiToolBarItemArray_GetCount)(wxAuiToolBarItemArray* self)
{
  return self->GetCount();
}

EWXWEXPORT(wxAuiToolBarItem*,wxAuiToolBarItemArray_Item)(wxAuiToolBarItemArray* self,int _idx)
{
  wxAuiToolBarItem* item = new wxAuiToolBarItem();
  *item = self->Item(_idx);
  return item;

}
/** wxAuiPaneInfoArray **/
/** see wxWidgets dynarray.h for additional array functions **/
EWXWEXPORT(wxAuiPaneInfoArray*,wxAuiPaneInfoArray_Create)()
{
  return new wxAuiPaneInfoArray();
}

EWXWEXPORT(void,wxAuiPaneInfoArray_Delete)(wxAuiPaneInfoArray* self)
{
  delete self;
}

EWXWEXPORT(int,wxAuiPaneInfoArray_GetCount)(wxAuiPaneInfoArray* self)
{
  return self->GetCount();
}

EWXWEXPORT(wxAuiPaneInfo*,wxAuiPaneInfoArray_Item)(wxAuiPaneInfoArray* self,int _idx)
{
  wxAuiPaneInfo* info = new wxAuiPaneInfo();
  *info = self->Item(_idx);
  return info;
}
}


