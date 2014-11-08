#include "wrapper.h"
#include "wx/tooltip.h"

extern "C"
{

EWXWEXPORT(void*,wxWindow_Create)(wxWindow* _prt,int _id,int _x,int _y,int _w,int _h,int _stl)
{
	return (void*)new wxWindow(_prt, (wxWindowID)_id, wxPoint(_x, _y), wxSize(_w, _h), (long)_stl);
}
	
EWXWEXPORT(bool,wxWindow_Close)(wxWindow* self,bool _force)
{
	return self->Close(_force);
}
	
EWXWEXPORT(bool,wxWindow_Destroy)(wxWindow* self)
{
	return self->Destroy();
}
	
EWXWEXPORT(void,wxWindow_ClearBackground)(wxWindow* self)
{
	self->ClearBackground();
}
	
EWXWEXPORT(void,wxWindow_Fit)(wxWindow* self)
{
	self->Fit();
}
	
EWXWEXPORT(void,wxWindow_DestroyChildren)(wxWindow* self)
{
	self->DestroyChildren();
}
	
EWXWEXPORT(bool,wxWindow_IsBeingDeleted)(wxWindow* self)
{
	return self->IsBeingDeleted();
}
	
EWXWEXPORT(void,wxWindow_SetLabel)(wxWindow* self,wxString* _title)
{
	self->SetLabel(*_title);
}
	
EWXWEXPORT(wxString*,wxWindow_GetLabel)(wxWindow* self)
{
	wxString *result = new wxString();
	*result = self->GetLabel();
	return result;
}
	
EWXWEXPORT(bool,wxWindow_GetLabelEmpty)(wxWindow* self)
{
	return self->GetLabel().IsEmpty();
}
	
EWXWEXPORT(void,wxWindow_SetName)(wxWindow* self,wxString* _name)
{
	self->SetName(*_name);
}
	
EWXWEXPORT(wxString*,wxWindow_GetName)(wxWindow* self)
{
	wxString *result = new wxString();
	*result = self->GetName();
	return result;
}
	
EWXWEXPORT(void,wxWindow_SetId)(wxWindow* self,int _id)
{
	self->SetId(_id);
}
	
EWXWEXPORT(int,wxWindow_GetId)(wxWindow* self)
{
	return self->GetId();
}
	
EWXWEXPORT(void,wxWindow_SetSize)(wxWindow* self,int x,int y,int width,int height,int sizeFlags)
{
	self->SetSize(x, y, width, height, sizeFlags);
}
	
EWXWEXPORT(void,wxWindow_Move)(wxWindow* self,int x,int y)
{
	self->Move( x, y );
}
	
EWXWEXPORT(void,wxWindow_Raise)(wxWindow* self)
{
	self->Raise();
}
	
EWXWEXPORT(void,wxWindow_Lower)(wxWindow* self)
{
	self->Lower();
}
	
EWXWEXPORT(void,wxWindow_SetClientSize)(wxWindow* self,int width,int height)
{
	self->SetClientSize( width, height );
}
	
EWXWEXPORT(wxPoint*,wxWindow_GetPosition)(wxWindow* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetPosition();
	return pt;
}
	
EWXWEXPORT(wxSize*,wxWindow_GetSize)(wxWindow* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetSize();
	return sz;
}
	
EWXWEXPORT(wxRect*,wxWindow_GetRect)(wxWindow* self)
{
	wxRect* rct = new wxRect();
	*rct = self->GetRect();
	return rct;
}
	
EWXWEXPORT(wxSize*,wxWindow_GetClientSize)(wxWindow* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetClientSize();
	return sz;
}
	
EWXWEXPORT(wxSize*,wxWindow_GetBestSize)(wxWindow* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetBestSize();
	return sz;
}
	
EWXWEXPORT(void,wxWindow_Center)(wxWindow* self,int direction)
{
	self->Center( direction );
}
	
EWXWEXPORT(void,wxWindow_CenterOnParent)(wxWindow* self,int dir)
{
	self->CenterOnParent(dir);
}
	
EWXWEXPORT(void,wxWindow_SetSizeHints)(wxWindow* self,int minW,int minH,int maxW,int maxH,int incW,int incH)
{
	self->SetSizeHints( minW, minH, maxW, maxH, incW, incH );
}
	
EWXWEXPORT(int,wxWindow_GetMinWidth)(wxWindow* self)
{
	return self->GetMinWidth();
}
	
EWXWEXPORT(int,wxWindow_GetMinHeight)(wxWindow* self)
{
	return self->GetMinHeight();
}
	
EWXWEXPORT(int,wxWindow_GetMaxWidth)(wxWindow* self)
{
	return self->GetMaxWidth();
}

EWXWEXPORT(int,wxWindow_GetMaxHeight)(wxWindow* self)
{
	return self->GetMaxHeight();
}
	
EWXWEXPORT(bool,wxWindow_Show)(wxWindow* self)
{
	return self->Show();
}
	
EWXWEXPORT(bool,wxWindow_Hide)(wxWindow* self)
{
	return self->Hide();
}
	
EWXWEXPORT(bool,wxWindow_Enable)(wxWindow* self)
{
	return self->Enable();
}

EWXWEXPORT(bool,wxWindow_Disable)(wxWindow* self)
{
	return self->Disable();
}
	
EWXWEXPORT(bool,wxWindow_IsShown)(wxWindow* self)
{
	return self->IsShown();
}
	
EWXWEXPORT(bool,wxWindow_IsEnabled)(wxWindow* self)
{
	return self->IsEnabled();
}
	
EWXWEXPORT(void,wxWindow_SetWindowStyleFlag)(wxWindow* self,long style)
{
	self->SetWindowStyleFlag( style );
}
	
EWXWEXPORT(int,wxWindow_GetWindowStyleFlag)(wxWindow* self)
{
	return (int)self->GetWindowStyleFlag();
}
	
EWXWEXPORT(bool,wxWindow_HasFlag)(wxWindow* self,int flag)
{
	return self->HasFlag(flag);
}
	
EWXWEXPORT(void,wxWindow_SetExtraStyle)(wxWindow* self,long exStyle)
{
	self->SetExtraStyle(exStyle);
}
	
// Obsolete
// EWXWEXPORT(void,wxWindow_MakeModal)(wxWindow* self,bool modal)

EWXWEXPORT(bool,wxWindow_HasFocus)(wxWindow* self)
{
	return self->HasFocus();
}

EWXWEXPORT(void,wxWindow_SetFocus)(wxWindow* self)
{
	self->SetFocus();
}
	
EWXWEXPORT(void*,wxWindow_FindFocus)(wxWindow* self)
{
	return (void*)self->FindFocus();
}
	
EWXWEXPORT(int,wxWindow_GetChildren)(wxWindow* self,void* _res,int _cnt)
{
	if (_res && (unsigned int)_cnt == self->GetChildren().GetCount())
	{
		unsigned int i = 0;
		wxWindowList::compatibility_iterator node = self->GetChildren().GetFirst();
		while (node)
		{
			((void**)_res)[i++] = (void*)(node->GetData());
			node = node->GetNext();
		}

		return i;
	}
	else
		return self->GetChildren().GetCount();
}
	
EWXWEXPORT(void*,wxWindow_GetParent)(wxWindow* self)
{
	return (void*)self->GetParent();
}
	
EWXWEXPORT(bool,wxWindow_IsTopLevel)(wxWindow* self)
{
	return self->IsTopLevel();
}
	
EWXWEXPORT(void*,wxWindow_FindWindow)(wxWindow* self,wxString* name)
{
	return (void*)self->FindWindow( *name );
}
	
EWXWEXPORT(void,wxWindow_AddChild)(wxWindow* self,wxWindowBase* child)
{
	self->AddChild(  child );
}
	
EWXWEXPORT(void,wxWindow_RemoveChild)(wxWindow* self,wxWindowBase* child)
{
	self->RemoveChild(  child );
}
	
EWXWEXPORT(void*,wxWindow_GetEventHandler)(wxWindow* self)
{
	return (void*)self->GetEventHandler();
}
	
EWXWEXPORT(void,wxWindow_PushEventHandler)(wxWindow* self,wxEvtHandler* handler)
{
	self->PushEventHandler( handler );
}
	
EWXWEXPORT(void*,wxWindow_PopEventHandler)(wxWindow* self,bool deleteHandler)
{
	return (void*)self->PopEventHandler(deleteHandler);
}
	
EWXWEXPORT(void,wxWindow_SetValidator)(wxWindow* self,void* validator)
{
	self->SetValidator(*((wxValidator*)validator));
}
	
EWXWEXPORT(void*,wxWindow_GetValidator)(wxWindow* self)
{
	return (void*)self->GetValidator();
}
	
EWXWEXPORT(void,wxWindow_SetClientData)(wxWindow* self,void* data)
{
	self->SetClientData( data );
}
	
EWXWEXPORT(void*,wxWindow_GetClientData)(wxWindow* self)
{
	return (void*)self->GetClientData();
}
	
EWXWEXPORT(bool,wxWindow_Validate)(wxWindow* self)
{
	return self->Validate();
}
	
EWXWEXPORT(bool,wxWindow_TransferDataToWindow)(wxWindow* self)
{
	return self->TransferDataToWindow();
}
	
EWXWEXPORT(bool,wxWindow_TransferDataFromWindow)(wxWindow* self)
{
	return self->TransferDataFromWindow();
}
	
EWXWEXPORT(void,wxWindow_InitDialog)(wxWindow* self)
{
	self->InitDialog();
}
	
EWXWEXPORT(void,wxWindow_SetAcceleratorTable)(wxWindow* self,void* accel)
{
	self->SetAcceleratorTable(*((wxAcceleratorTable*)accel));
}
	
EWXWEXPORT(wxPoint*,wxWindow_ConvertPixelsToDialog)(wxWindow* self,int x,int y)
{
	const wxPoint pos(x, y);
	wxPoint* pt = new wxPoint();
	*pt = self->ConvertPixelsToDialog(pos);
	return pt;
}
	
EWXWEXPORT(wxPoint*,wxWindow_ConvertDialogToPixels)(wxWindow* self,int x,int y)
{
	const wxPoint pos(x, y);
	wxPoint* pt = new wxPoint();
	*pt = self->ConvertDialogToPixels(pos);
	return pt;
}
	
EWXWEXPORT(void,wxWindow_WarpPointer)(wxWindow* self,int x,int y)
{
	self->WarpPointer(x, y);
}
	
EWXWEXPORT(void,wxWindow_CaptureMouse)(wxWindow* self)
{
	self->CaptureMouse();
}
	
EWXWEXPORT(void,wxWindow_ReleaseMouse)(wxWindow* self)
{
	self->ReleaseMouse();
}
	
EWXWEXPORT(void,wxWindow_Refresh)(wxWindow* self,bool eraseBackground)
{
	self->Refresh(eraseBackground, (const wxRect*)NULL);
}
	
EWXWEXPORT(void,wxWindow_RefreshRect)(wxWindow* self,bool eraseBackground,int x,int y,int w,int h)
{
	const wxRect rect(x, y, w, h);
	self->Refresh(eraseBackground, &rect);
}
	
EWXWEXPORT(void,wxWindow_PrepareDC)(wxWindow* self,wxDC* dc)
{
	self->PrepareDC(*dc);
}
	
EWXWEXPORT(void*,wxWindow_GetUpdateRegion)(wxWindow* self)
{
	return (void*)(&self->GetUpdateRegion());
}
	
EWXWEXPORT(bool,wxWindow_IsExposed)(wxWindow* self,int x,int y,int w,int h)
{
	return self->IsExposed( x, y, w, h );
}
	
EWXWEXPORT(bool,wxWindow_SetBackgroundColour)(wxWindow* self,wxColour* colour)
{
	return self->SetBackgroundColour(*colour);
}
	
EWXWEXPORT(void,wxWindow_SetForegroundColour)(wxWindow* self,wxColour* colour)
{
	self->SetForegroundColour(*colour);
}
	
EWXWEXPORT(void,wxWindow_GetBackgroundColour)(wxWindow* self,wxColour* colour)
{
	*colour = self->GetBackgroundColour();
}
	
EWXWEXPORT(void,wxWindow_GetForegroundColour)(wxWindow* self,wxColour* colour)
{
	*colour = self->GetForegroundColour();
}
	
EWXWEXPORT(void,wxWindow_SetCursor)(wxWindow* self,wxCursor* cursor)
{
	self->SetCursor(*cursor);
}
	
EWXWEXPORT(wxCursor*,wxWindow_GetCursor)(wxWindow* self)
{
	wxCursor* cur = new wxCursor();
	*cur = self->GetCursor();
	return cur;
}
	
EWXWEXPORT(void,wxWindow_SetFont)(wxWindow* self,wxFont* font)
{
	self->SetFont(*font);
}
	
EWXWEXPORT(void,wxWindow_GetFont)(wxWindow* self,wxFont* _font)
{
	*_font = self->GetFont();
}
	
EWXWEXPORT(void,wxWindow_SetCaret)(wxWindow* self,wxCaret* caret)
{
	self->SetCaret(caret);
}
	
EWXWEXPORT(wxCaret*,wxWindow_GetCaret)(wxWindow* self)
{
	return self->GetCaret();
}
	
EWXWEXPORT(int,wxWindow_GetCharHeight)(wxWindow* self)
{
	return self->GetCharHeight();
}
	
EWXWEXPORT(int,wxWindow_GetCharWidth)(wxWindow* self)
{
	return self->GetCharWidth();
}
	
EWXWEXPORT(void,wxWindow_GetTextExtent)(wxWindow* self,wxString* string,int* x,int* y,int* descent,int* externalLeading,wxFont* theFont)
{
	self->GetTextExtent(*string, x,  y, descent, externalLeading, theFont );
}
	
EWXWEXPORT(wxPoint*,wxWindow_ScreenToClient)(wxWindow* self,int x,int y)
{
	const wxPoint pos(x, y);
	wxPoint* pt = new wxPoint();
	*pt = self->ScreenToClient(pos);
	return pt;
}
	
EWXWEXPORT(void,wxWindow_UpdateWindowUI)(wxWindow* self)
{
	self->UpdateWindowUI();
}
	
EWXWEXPORT(bool,wxWindow_PopupMenu)(wxWindow* self,wxMenu* menu,int x,int y)
{
	return self->PopupMenu(menu, x, y );
}
	
EWXWEXPORT(void,wxWindow_SetScrollPos)(wxWindow* self,int orient,int pos,bool refresh)
{
	self->SetScrollPos( orient, pos, refresh);
}
	
EWXWEXPORT(int,wxWindow_GetScrollPos)(wxWindow* self,int orient)
{
	return self->GetScrollPos( orient );
}
	
EWXWEXPORT(int,wxWindow_GetScrollThumb)(wxWindow* self,int orient)
{
	return self->GetScrollThumb( orient );
}
	
EWXWEXPORT(int,wxWindow_GetScrollRange)(wxWindow* self,int orient)
{
	return self->GetScrollRange( orient );
}
	
EWXWEXPORT(void,wxWindow_ScrollWindow)(wxWindow* self,int dx,int dy)
{
	self->ScrollWindow(dx, dy, (const wxRect*)NULL);
}
	
EWXWEXPORT(void,wxWindow_ScrollWindowRect)(wxWindow* self,int dx,int dy,int x,int y,int w,int h)
{
	const wxRect rect(x, y, w, h);
	self->ScrollWindow(dx, dy, &rect);
}
	
EWXWEXPORT(void,wxWindow_SetToolTip)(wxWindow* self,wxString* tip)
{
	self->SetToolTip( *tip );
}
	
EWXWEXPORT(wxString*,wxWindow_GetToolTip)(wxWindow* self)
{
	wxToolTip* tip = self->GetToolTip();

	if (tip)
	{
		wxString *result = new wxString();
		*result = tip->GetTip();
		return result;
	}
	return NULL;
}
	
EWXWEXPORT(void,wxWindow_SetDropTarget)(wxWindow* self,void* dropTarget)
{
	self->SetDropTarget((wxDropTarget*)dropTarget);
}
	
EWXWEXPORT(void*,wxWindow_GetDropTarget)(wxWindow* self)
{
	return (void*)self->GetDropTarget();
}
	
EWXWEXPORT(void,wxWindow_SetConstraints)(wxWindow* self,void* constraints)
{
	self->SetConstraints((wxLayoutConstraints*)constraints);
}
	
EWXWEXPORT(void*,wxWindow_GetConstraints)(wxWindow* self)
{
	return (void*)self->GetConstraints();
}
	
EWXWEXPORT(void,wxWindow_SetAutoLayout)(wxWindow* self,bool autoLayout)
{
	self->SetAutoLayout( autoLayout );
}
	
EWXWEXPORT(int,wxWindow_GetAutoLayout)(wxWindow* self)
{
	return (int)self->GetAutoLayout();
}
	
EWXWEXPORT(void,wxWindow_Layout)(wxWindow* self)
{
	self->Layout();
}
	
EWXWEXPORT(void,wxWindow_UnsetConstraints)(wxWindow* self,void* c)
{
	self->UnsetConstraints((wxLayoutConstraints*)c);
}
	
EWXWEXPORT(void*,wxWindow_GetConstraintsInvolvedIn)(wxWindow* self)
{
	return (void*)self->GetConstraintsInvolvedIn();
}
	
EWXWEXPORT(void,wxWindow_AddConstraintReference)(wxWindow* self,wxWindowBase* otherWin)
{
	self->AddConstraintReference( otherWin);
}
	
EWXWEXPORT(void,wxWindow_RemoveConstraintReference)(wxWindow* self,wxWindowBase* otherWin)
{
	self->RemoveConstraintReference( otherWin);
}
	
EWXWEXPORT(void,wxWindow_DeleteRelatedConstraints)(wxWindow* self)
{
	self->DeleteRelatedConstraints();
}
	
EWXWEXPORT(void,wxWindow_ResetConstraints)(wxWindow* self)
{
	self->ResetConstraints();
}
	
EWXWEXPORT(void,wxWindow_SetConstraintSizes)(wxWindow* self,bool recurse)
{
	self->SetConstraintSizes(recurse);
}
	
EWXWEXPORT(int,wxWindow_LayoutPhase1)(wxWindow* self,int* noChanges)
{
	return (int)self->LayoutPhase1(noChanges);
}
	
EWXWEXPORT(int,wxWindow_LayoutPhase2)(wxWindow* self,int* noChanges)
{
	return (int)self->LayoutPhase2(noChanges);
}
	
EWXWEXPORT(int,wxWindow_DoPhase)(wxWindow* self,int phase)
{
	return (int)self->DoPhase(phase);
}
	
EWXWEXPORT(void,wxWindow_SetSizeConstraint)(wxWindow* self,int x,int y,int w,int h)
{
	self->SetSizeConstraint(x, y, w, h);
}
	
EWXWEXPORT(void,wxWindow_MoveConstraint)(wxWindow* self,int x,int y)
{
	self->MoveConstraint(x, y);
}
	
EWXWEXPORT(void,wxWindow_GetSizeConstraint)(wxWindow* self,int* w,int* h)
{
	self->GetSizeConstraint(w, h);
}
	
EWXWEXPORT(void,wxWindow_GetClientSizeConstraint)(wxWindow* self,int* w,int* h)
{
	self->GetClientSizeConstraint(w, h);
}
	
EWXWEXPORT(void,wxWindow_GetPositionConstraint)(wxWindow* self,int* x,int* y)
{
	self->GetPositionConstraint(x, y);
}
	
EWXWEXPORT(void,wxWindow_SetSizer)(wxWindow* self,wxSizer* sizer)
{
	self->SetSizer(sizer);
}
	
EWXWEXPORT(void*,wxWindow_GetSizer)(wxWindow* self)
{
	return (void*)self->GetSizer();
}
	
EWXWEXPORT(void*,wxWindow_GetHandle)(wxWindow* self)
{
	return (void*)self->GetHandle();
}
	
EWXWEXPORT(void,wxWindow_SetScrollbar)(wxWindow* self,int orient,int pos,int thumbVisible,int range,bool refresh)
{
	self->SetScrollbar(orient, pos, thumbVisible, range, refresh);
}

EWXWEXPORT(bool,wxWindow_Reparent)(wxWindow* self,wxWindow* _par)
{
	return self->Reparent(_par);
}

#if (wxVERSION_NUMBER < 2800)
EWXWEXPORT(wxSize*,wxWindow_GetAdjustedBestSize)(wxWindow* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetAdjustedBestSize();
	return sz;
}
#else
EWXWEXPORT(wxSize*,wxWindow_GetEffectiveMinSize)(wxWindow* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetEffectiveMinSize();
	return sz;
}
#endif

EWXWEXPORT(void,wxWindow_Freeze)(wxWindow* self)
{
	self->Freeze();
}

EWXWEXPORT(void,wxWindow_Thaw)(wxWindow* self)
{
	self->Thaw();
}

#if (wxVERSION_NUMBER >= 2800)
EWXWEXPORT(wxPoint*,wxWindow_ClientToScreen)(wxWindow* self,int x,int y)
{
	const wxPoint pos(x, y);
	wxPoint* pt = new wxPoint();
	*pt = self->ClientToScreen(pos);
	return pt;
}

EWXWEXPORT(void,wxWindow_FitInside)(wxWindow* self)
{
	self->FitInside();
}

EWXWEXPORT(void,wxWindow_SetVirtualSize)(wxWindow* self,int w,int h)
{
	self->SetVirtualSize( w, h );
}

EWXWEXPORT(wxSize*,wxWindow_GetVirtualSize)(wxWindow* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetVirtualSize();
	return sz;
}

#endif
}
