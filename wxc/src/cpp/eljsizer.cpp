#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxSizerItem_Create)(int width,int height,int option,int flag,int border,void* userData)
{
	return (void*)new wxSizerItem(width, height, option, flag, border, new ELJDataObject(userData));
}
	
EWXWEXPORT(void*,wxSizerItem_CreateInWindow)(wxWindow* window,int option,int flag,int border,void* userData)
{
	return (void*)new wxSizerItem(window, option, flag, border, new ELJDataObject(userData));
}
	
EWXWEXPORT(void*,wxSizerItem_CreateInSizer)(wxSizer* sizer,int option,int flag,int border,void* userData)
{
	return (void*)new wxSizerItem(sizer, option, flag, border, new ELJDataObject(userData));
}
	
EWXWEXPORT(wxSize*,wxSizerItem_GetSize)(wxSizerItem* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetSize();
	return sz;
}
	
EWXWEXPORT(wxSize*,wxSizerItem_CalcMin)(wxSizerItem* self)
{
	wxSize* sz = new wxSize();
	*sz = self->CalcMin();
	return sz;
}
	
EWXWEXPORT(void,wxSizerItem_SetDimension)(wxSizerItem* self,int _x,int _y,int _w,int _h)
{
	self->SetDimension(wxPoint(_x, _y), wxSize(_w, _h));
}
	
EWXWEXPORT(wxSize*,wxSizerItem_GetMinSize)(wxSizerItem* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetMinSize();
	return sz;
}
	
EWXWEXPORT(void,wxSizerItem_SetRatio)(wxSizerItem* self,int width,int height)
{
	self->SetRatio(width, height);
}
	
EWXWEXPORT(void,wxSizerItem_SetFloatRatio)(wxSizerItem* self,float ratio)
{
	self->SetRatio(ratio);
}
	
EWXWEXPORT(float,wxSizerItem_GetRatio)(wxSizerItem* self)
{
	return self->GetRatio();
}
	
EWXWEXPORT(bool,wxSizerItem_IsWindow)(wxSizerItem* self)
{
	return self->IsWindow();
}
	
EWXWEXPORT(bool,wxSizerItem_IsSizer)(wxSizerItem* self)
{
	return self->IsSizer();
}
	
EWXWEXPORT(bool,wxSizerItem_IsSpacer)(wxSizerItem* self)
{
	return self->IsSpacer();
}
	
EWXWEXPORT(void,wxSizerItem_SetInitSize)(wxSizerItem* self,int x,int y)
{
	self->SetInitSize(x, y);
}
	
#if (wxVERSION_NUMBER < 2800)	
EWXWEXPORT(void,wxSizerItem_SetOption)(wxSizerItem* self,int option)
{
	self->SetOption(option);
}
#endif

EWXWEXPORT(void,wxSizerItem_SetFlag)(wxSizerItem* self,int flag)
{
	self->SetFlag(flag);
}
	
EWXWEXPORT(void,wxSizerItem_SetBorder)(wxSizerItem* self,int border)
{
	self->SetBorder(border);
}
	
EWXWEXPORT(wxWindow*,wxSizerItem_GetWindow)(wxSizerItem* self)
{
	return self->GetWindow();
}
	
// EWXWEXPORT(void,wxSizerItem_SetWindow)(wxSizerItem* self,wxWindow* window)
EWXWEXPORT(void, wxSizerItem_AssignWindow)(wxSizerItem* self, wxWindow* window)
{
	self->AssignWindow(window);
}
	
EWXWEXPORT(void*,wxSizerItem_GetSizer)(wxSizerItem* self)
{
	return (void*)self->GetSizer();
}
	
// EWXWEXPORT(void,wxSizerItem_SetSizer)(wxSizerItem* self,wxSizer* sizer)
EWXWEXPORT(void, wxSizerItem_AssignSizer)(wxSizerItem* self, wxSizer* sizer)
{
	self->AssignSizer(sizer);
}
	
#if (wxVERSION_NUMBER < 2800)
EWXWEXPORT(int,wxSizerItem_GetOption)(wxSizerItem* self)
{
	return self->GetOption();
}
#endif

EWXWEXPORT(int,wxSizerItem_GetFlag)(wxSizerItem* self)
{
	return self->GetFlag();
}
	
EWXWEXPORT(int,wxSizerItem_GetBorder)(wxSizerItem* self)
{
	return self->GetBorder();
}
	
EWXWEXPORT(void*,wxSizerItem_GetUserData)(wxSizerItem* self)
{
	return ((ELJDataObject*)self->GetUserData())->data;
}
	
EWXWEXPORT(wxPoint*,wxSizerItem_GetPosition)(wxSizerItem* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetPosition();
	return pt;
}
	
#if (wxVERSION_NUMBER >= 2800)
EWXWEXPORT(void,wxSizerItem_Delete)(wxSizerItem* self)
{
	delete self;
}

EWXWEXPORT(void,wxSizerItem_DeleteWindows)(wxSizerItem* self)
{
	self->DeleteWindows();
}

EWXWEXPORT(void,wxSizerItem_DetachSizer)(wxSizerItem* self)
{
	self->DetachSizer();
}

EWXWEXPORT(int,wxSizerItem_GetProportion)(wxSizerItem* self)
{
	return self->GetProportion();
}

EWXWEXPORT(wxRect*,wxSizerItem_GetRect)(wxSizerItem* self)
{
	wxRect* rct = new wxRect();
	*rct = self->GetRect();
	return rct;
}

EWXWEXPORT(wxSize*,wxSizerItem_GetSpacer)(wxSizerItem* self,void* _w,void* _h)
{
	wxSize* sz = new wxSize(0,0);

	if (self->IsSpacer())
	{
		*sz = self->GetSpacer();
	}
	return sz;
}

EWXWEXPORT(bool,wxSizerItem_IsShown)(wxSizerItem* self)
{
	return  self->IsShown();
}

EWXWEXPORT(void,wxSizerItem_SetProportion)(wxSizerItem* self,int proportion)
{
	self->SetProportion(proportion);
}

// EWXWEXPORT(void,wxSizerItem_SetSpacer)(wxSizerItem* self,int width,int height)
EWXWEXPORT(void, wxSizerItem_AssignSpacer)(wxSizerItem* self, int width, int height)
{
	self->AssignSpacer(wxSize(width, height));
}

EWXWEXPORT(void,wxSizerItem_Show)(wxSizerItem* self,int show)
{
  	self->Show(show);
}
#endif

EWXWEXPORT(void,wxSizer_AddWindow)(wxSizer* self,wxWindow* window,int option,int flag,int border,void* userData)
{
	self->Add(window, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_AddSizer)(wxSizer* self,wxSizer* sizer,int option,int flag,int border,void* userData)
{
	self->Add(sizer, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_Add)(wxSizer* self,int width,int height,int option,int flag,int border,void* userData)
{
	self->Add(width, height, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_InsertWindow)(wxSizer* self,int before,wxWindow* window,int option,int flag,int border,void* userData)
{
	self->Insert(before, window, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_InsertSizer)(wxSizer* self,int before,wxSizer* sizer,int option,int flag,int border,void* userData)
{
	self->Insert(before, sizer, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_Insert)(wxSizer* self,int before,int width,int height,int option,int flag,int border,void* userData)
{
	self->Insert(before, width, height, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_PrependWindow)(wxSizer* self,wxWindow* window,int option,int flag,int border,void* userData)
{
	self->Prepend(window, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_PrependSizer)(wxSizer* self,wxSizer* sizer,int option,int flag,int border,void* userData)
{
	self->Prepend(sizer, option, flag, border, new ELJDataObject (userData));
}
	
EWXWEXPORT(void,wxSizer_Prepend)(wxSizer* self,int width,int height,int option,int flag,int border,void* userData)
{
	self->Prepend(width, height, option, flag, border, new ELJDataObject (userData));
}
	
#if (wxVERSION_NUMBER < 2800)	
EWXWEXPORT(bool,wxSizer_RemoveWindow)(wxSizer* self,wxWindow* window)
{
	return self->Remove(window);
}
	
EWXWEXPORT(bool,wxSizer_RemoveSizer)(wxSizer* self,wxSizer* sizer)
{
	return self->Remove(sizer);
}
	
EWXWEXPORT(bool,wxSizer_Remove)(wxSizer* self,int pos)
{
	return self->Remove(pos);
}
#endif
	
EWXWEXPORT(void,wxSizer_SetMinSize)(wxSizer* self,int width,int height)
{
	self->SetMinSize(width, height);
}
	
EWXWEXPORT(void,wxSizer_SetItemMinSizeWindow)(wxSizer* self,wxWindow* window,int width,int height)
{
	self->SetItemMinSize( window, width, height);
}
	
EWXWEXPORT(void,wxSizer_SetItemMinSizeSizer)(wxSizer* self,wxSizer* sizer,int width,int height)
{
	self->SetItemMinSize(sizer, width, height);
}
	
EWXWEXPORT(void,wxSizer_SetItemMinSize)(wxSizer* self,int pos,int width,int height)
{
	self->SetItemMinSize(pos, width, height);
}
	
EWXWEXPORT(wxSize*,wxSizer_GetSize)(wxSizer* self)
{
	wxSize* s = new wxSize();
	*s = self->GetSize();
	return s;
}
	
EWXWEXPORT(wxPoint*,wxSizer_GetPosition)(wxSizer* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetPosition();
	return pt;
}
	
EWXWEXPORT(wxSize*,wxSizer_GetMinSize)(wxSizer* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetMinSize();
	return sz;
}
	
EWXWEXPORT(void,wxSizer_RecalcSizes)(wxSizer* self)
{
	self->RecalcSizes();
}
	
EWXWEXPORT(wxSize*,wxSizer_CalcMin)(wxSizer* self)
{
	wxSize* sz = new wxSize();
	*sz = self->CalcMin();
	return sz;
}
	
EWXWEXPORT(void,wxSizer_Layout)(wxSizer* self)
{
	self->Layout();
}
	
EWXWEXPORT(void,wxSizer_Fit)(wxSizer* self,wxWindow* window)
{
	self->Fit(window);
}
	
EWXWEXPORT(void,wxSizer_SetSizeHints)(wxSizer* self,wxWindow* window)
{
	self->SetSizeHints(window);
}
	
EWXWEXPORT(int, wxSizer_GetChildren)(wxSizer* self, void* _res, int _cnt)
{
	if (_res && (unsigned int)_cnt == self->GetChildren().GetCount())
	{
		int i = 0;
		wxSizerItemList::compatibility_iterator node = self->GetChildren().GetFirst();
		while (node)
		{
			((void**)_res)[i] = node->GetData();
			node = node->GetNext();
			++i;
		}
		return i;
	}
	else
		return self->GetChildren().GetCount();
}

#if (wxVERSION_NUMBER >= 2800)
EWXWEXPORT(void,wxSizer_AddSpacer)(wxSizer* self,int size)
{
	self->AddSpacer(size);
}

EWXWEXPORT(void,wxSizer_AddStretchSpacer)(wxSizer* self,int size)
{
	self->AddStretchSpacer(size);
}

EWXWEXPORT(void,wxSizer_Clear)(wxSizer* self,bool delete_windows)
{
	self->Clear(delete_windows);
}

EWXWEXPORT(bool,wxSizer_DetachWindow)(wxSizer* self,wxWindow* window)
{
	return self->Detach(window);
}

EWXWEXPORT(bool,wxSizer_DetachSizer)(wxSizer* self,wxSizer* sizer)
{
	return self->Detach(sizer);
}

EWXWEXPORT(bool,wxSizer_Detach)(wxSizer* self,int index)
{
	return self->Detach((size_t) index);
}

EWXWEXPORT(void,wxSizer_FitInside)(wxSizer* self,wxWindow* window)
{
	self->FitInside( window);
}

EWXWEXPORT(void*,wxSizer_GetContainingWindow)(wxSizer* self)
{
	return (void*)self->GetContainingWindow();
}

EWXWEXPORT(void*,wxSizer_GetItemWindow)(wxSizer* self,wxWindow* window,bool recursive)
{
	return (void*)self->GetItem( window, recursive);
}

EWXWEXPORT(void*,wxSizer_GetItemSizer)(wxSizer* self,wxSizer* sizer,bool recursive)
{
	return (void*)self->GetItem(sizer, recursive);
}

EWXWEXPORT(void*,wxSizer_GetItem)(wxSizer* self,int index)
{
	return (void*)self->GetItem((size_t) index);
}

EWXWEXPORT(bool,wxSizer_HideWindow)(wxSizer* self,wxWindow* window)
{
	return self->Hide(window);
}

EWXWEXPORT(bool,wxSizer_HideSizer)(wxSizer* self,wxSizer* sizer)
{
	return self->Hide(sizer);
}

EWXWEXPORT(bool,wxSizer_Hide)(wxSizer* self,int index)
{
	return self->Hide((size_t) index);
}

EWXWEXPORT(void*,wxSizer_InsertSpacer)(wxSizer* self,int index,int size)
{
	return (void*)self->InsertSpacer((size_t) index, size);
}

EWXWEXPORT(void*,wxSizer_InsertStretchSpacer)(wxSizer* self,int index,int prop)
{
	return (void*)self->InsertStretchSpacer((size_t) index, prop);
}

EWXWEXPORT(bool,wxSizer_IsShownWindow)(wxSizer* self,wxWindow* window)
{
	return self->IsShown( window);
}

EWXWEXPORT(bool,wxSizer_IsShownSizer)(wxSizer* self,wxSizer* sizer)
{
	return self->IsShown(sizer);
}

EWXWEXPORT(bool,wxSizer_IsShown)(wxSizer* self,size_t index)
{
	return self->IsShown( index);
}

EWXWEXPORT(void*,wxSizer_PrependSpacer)(wxSizer* self,int size)
{
	return (void*)self->PrependSpacer(size);
}

EWXWEXPORT(void*,wxSizer_PrependStretchSpacer)(wxSizer* self,int prop)
{
	return (void*)self->PrependStretchSpacer(prop);
}

EWXWEXPORT(bool,wxSizer_ReplaceWindow)(wxSizer* self,wxWindow* oldwin,wxWindow* newwin,bool recursive)
{
	return self->Replace(oldwin, newwin, recursive);
}

EWXWEXPORT(bool,wxSizer_ReplaceSizer)(wxSizer* self,wxSizer* oldsz,wxSizer* newsz,bool recursive)
{
	return self->Replace( oldsz, newsz,recursive);
}

EWXWEXPORT(bool,wxSizer_Replace)(wxSizer* self,int oldindex,wxSizerItem* newsz)
{
	return self->Replace((size_t) oldindex,newsz);
}

/* Deprecated; replace with wxSizer_FitInside
// EWXWEXPORT(void,wxSizer_SetVirtualSizeHints)(wxSizer* self,wxWindow* window)
*/

EWXWEXPORT(bool,wxSizer_ShowWindow)(wxSizer* self,wxWindow* window,bool show,bool recursive)
{
	return self->Show(window, show, recursive);
}

EWXWEXPORT(bool,wxSizer_ShowSizer)(wxSizer* self,wxSizer* sizer,bool show,bool recursive)
{
	return self->Show(sizer, show, recursive);
}

EWXWEXPORT(bool,wxSizer_Show)(wxSizer* self,int index,bool show)
{
	return self->Show((size_t) index, show);
}
#endif
	
EWXWEXPORT(void,wxSizer_SetDimension)(wxSizer* self,int x,int y,int width,int height)
{
	self->SetDimension(x, y, width, height);
}
	
EWXWEXPORT(void*,wxGridSizer_Create)(int rows,int cols,int vgap,int hgap)
{
	return (void*)new wxGridSizer(rows, cols, vgap, hgap);
}
	
EWXWEXPORT(void,wxGridSizer_RecalcSizes)(wxGridSizer* self)
{
	self->RecalcSizes();
}
	
EWXWEXPORT(wxSize*,wxGridSizer_CalcMin)(wxGridSizer* self)
{
	wxSize* sz = new wxSize();
	*sz = self->CalcMin();
	return sz;
}
	
EWXWEXPORT(void,wxGridSizer_SetCols)(wxGridSizer* self,int cols)
{
	self->SetCols(cols);
}
	
EWXWEXPORT(void,wxGridSizer_SetRows)(wxGridSizer* self,int rows)
{
	self->SetRows(rows);
}
	
EWXWEXPORT(void,wxGridSizer_SetVGap)(wxGridSizer* self,int gap)
{
	self->SetVGap(gap);
}
	
EWXWEXPORT(void,wxGridSizer_SetHGap)(wxGridSizer* self,int gap)
{
	self->SetHGap(gap);
}
	
EWXWEXPORT(int,wxGridSizer_GetCols)(wxGridSizer* self)
{
	return self->GetCols();
}
	
EWXWEXPORT(int,wxGridSizer_GetRows)(wxGridSizer* self)
{
	return self->GetRows();
}
	
EWXWEXPORT(int,wxGridSizer_GetVGap)(wxGridSizer* self)
{
	return self->GetVGap();
}
	
EWXWEXPORT(int,wxGridSizer_GetHGap)(wxGridSizer* self)
{
	return self->GetHGap();
}
	
EWXWEXPORT(void*,wxFlexGridSizer_Create)(int rows,int cols,int vgap,int hgap)
{
	return new wxFlexGridSizer(rows, cols, vgap, hgap);
}
	
EWXWEXPORT(void,wxFlexGridSizer_RecalcSizes)(wxFlexGridSizer* self)
{
	self->RecalcSizes();
}
	
EWXWEXPORT(wxSize*,wxFlexGridSizer_CalcMin)(wxFlexGridSizer* self)
{
	wxSize* sz = new wxSize();
	*sz = self->CalcMin();
	return sz;
}
	
EWXWEXPORT(void,wxFlexGridSizer_AddGrowableRow)(wxFlexGridSizer* self,size_t idx)
{
	self->AddGrowableRow(idx);
}
	
EWXWEXPORT(void,wxFlexGridSizer_RemoveGrowableRow)(wxFlexGridSizer* self,size_t idx)
{
	self->RemoveGrowableRow(idx);
}
	
EWXWEXPORT(void,wxFlexGridSizer_AddGrowableCol)(wxFlexGridSizer* self,size_t idx)
{
	self->AddGrowableCol(idx);
}
	
EWXWEXPORT(void,wxFlexGridSizer_RemoveGrowableCol)(wxFlexGridSizer* self,size_t idx)
{
	self->RemoveGrowableCol(idx);
}
	
EWXWEXPORT(void*,wxBoxSizer_Create)(int orient)
{
	return (void*)new wxBoxSizer(orient);
}
	
EWXWEXPORT(void,wxBoxSizer_RecalcSizes)(wxBoxSizer* self)
{
	self->RecalcSizes();
}
	
EWXWEXPORT(wxSize*,wxBoxSizer_CalcMin)(wxBoxSizer* self)
{
	wxSize* sz = new wxSize();
	*sz = self->CalcMin();
	return sz;
}
	
EWXWEXPORT(int,wxBoxSizer_GetOrientation)(wxBoxSizer* self)
{
	return self->GetOrientation();
}
	
EWXWEXPORT(void*,wxStaticBoxSizer_Create)(wxStaticBox* box,int orient)
{
	return (void*)new wxStaticBoxSizer(box, orient );
}
	
EWXWEXPORT(void,wxStaticBoxSizer_RecalcSizes)(wxStaticBoxSizer* self)
{
	self->RecalcSizes();
}
	
EWXWEXPORT(wxSize*,wxStaticBoxSizer_CalcMin)(wxStaticBoxSizer* self)
{
	wxSize* sz = new wxSize();
	*sz = self->CalcMin();
	return sz;
}
	
EWXWEXPORT(void*,wxStaticBoxSizer_GetStaticBox)(wxStaticBoxSizer* self)
{
	return (void*)self->GetStaticBox();
}
	
#if (wxVERSION_NUMBER < 2800)
EWXWEXPORT(void*,wxNotebookSizer_Create)(wxNotebook* nb)
{
	return (void*)new wxNotebookSizer(nb);
}
	
EWXWEXPORT(void,wxNotebookSizer_RecalcSizes)(wxNotebookSizer* self)
{
	self->RecalcSizes();
}
	
EWXWEXPORT(wxSize*,wxNotebookSizer_CalcMin)(wxNotebookSizer* self)
{
	wxSize* sz = new wxSize();
	*sz = self->CalcMin();
	return sz;
}
	
EWXWEXPORT(void*,wxNotebookSizer_GetNotebook)(wxNotebookSizer* self)
{
	return (void*)self->GetNotebook();
}
#endif

}
