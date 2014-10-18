#include "wrapper.h"

extern "C"
{

typedef int _cdecl (*EiffelSortFunc) (void* obj, int data1, int data2);

typedef struct _EiffelSort
{
	void*          obj;
	EiffelSortFunc fnc;
}EiffelSort;

int wxCALLBACK ListCmp (wxIntPtr item1, wxIntPtr item2, wxIntPtr sortData)
{
	return ((EiffelSort*)sortData)->fnc (((EiffelSort*)sortData)->obj, (int)item1, (int)item2);
}

EWXWEXPORT(wxListItem*,wxListItem_Create)()
{
	return new wxListItem();
}

EWXWEXPORT(void,wxListItem_Delete)(wxListItem* self)
{
	delete self;
}

EWXWEXPORT(void,wxListItem_Clear)(wxListItem* self)
{
	self->Clear();
}
	
EWXWEXPORT(void,wxListItem_ClearAttributes)(wxListItem* self)
{
	self->ClearAttributes();
}
	
EWXWEXPORT(void,wxListItem_SetMask)(wxListItem* self,int mask)
{
	self->SetMask((long)mask);
}
	
EWXWEXPORT(void,wxListItem_SetId)(wxListItem* self,int id)
{
	self->SetId((long)id);
}
	
EWXWEXPORT(void,wxListItem_SetColumn)(wxListItem* self,int col)
{
	self->SetColumn(col);
}
	
EWXWEXPORT(void,wxListItem_SetState)(wxListItem* self,int state)
{
	self->SetState((long)state);
}
	
EWXWEXPORT(void,wxListItem_SetStateMask)(wxListItem* self,int stateMask)
{
	self->SetStateMask((long)stateMask);
}
	
EWXWEXPORT(void,wxListItem_SetText)(wxListItem* self,wxString* text)
{
	self->SetText(*text);
}
	
EWXWEXPORT(void,wxListItem_SetImage)(wxListItem* self,int image)
{
	self->SetImage(image);
}
	
EWXWEXPORT(void,wxListItem_SetData)(wxListItem* self,int data)
{
	self->SetData((long)data);
}
	
EWXWEXPORT(void,wxListItem_SetDataPointer)(wxListItem* self,void* data)
{
	self->SetData(data);
}
	
EWXWEXPORT(void,wxListItem_SetWidth)(wxListItem* self,int width)
{
	self->SetWidth(width);
}
	
EWXWEXPORT(void,wxListItem_SetAlign)(wxListItem* self,int align)
{
	self->SetAlign((wxListColumnFormat)align);
}
	
EWXWEXPORT(void,wxListItem_SetTextColour)(wxListItem* self,wxColour* colText)
{
	self->SetTextColour(*colText);
}
	
EWXWEXPORT(void,wxListItem_SetBackgroundColour)(wxListItem* self,wxColour* colBack)
{
	self->SetBackgroundColour(*colBack);
}
	
EWXWEXPORT(void,wxListItem_SetFont)(wxListItem* self,wxFont* font)
{
	self->SetFont(*font);
}
	
EWXWEXPORT(long,wxListItem_GetMask)(wxListItem* self)
{
	return self->GetMask();
}
	
EWXWEXPORT(long,wxListItem_GetId)(wxListItem* self)
{
	return self->GetId();
}
	
EWXWEXPORT(int,wxListItem_GetColumn)(wxListItem* self)
{
	return self->GetColumn();
}
	
EWXWEXPORT(long,wxListItem_GetState)(wxListItem* self)
{
	return self->GetState();
}
	
EWXWEXPORT(wxString*,wxListItem_GetText)(wxListItem* self)
{
	wxString *result = new wxString();
	*result = self->GetText();
	return result;
}
	
EWXWEXPORT(int,wxListItem_GetImage)(wxListItem* self)
{
	return self->GetImage();
}
	
EWXWEXPORT(long,wxListItem_GetData)(wxListItem* self)
{
	return self->GetData();
}
	
EWXWEXPORT(int,wxListItem_GetWidth)(wxListItem* self)
{
	return self->GetWidth();
}
	
EWXWEXPORT(int,wxListItem_GetAlign)(wxListItem* self)
{
	return (int)self->GetAlign();
}
	
EWXWEXPORT(void*,wxListItem_GetAttributes)(wxListItem* self)
{
	return (void*)self->GetAttributes();
}
	
EWXWEXPORT(int,wxListItem_HasAttributes)(wxListItem* self)
{
	return (int)self->HasAttributes();
}
	
EWXWEXPORT(void,wxListItem_GetTextColour)(wxListItem* self,wxColour* _ref)
{
	*_ref = self->GetTextColour();
}
	
EWXWEXPORT(void,wxListItem_GetBackgroundColour)(wxListItem* self,wxColour* _ref)
{
	*_ref = self->GetBackgroundColour();
}
	
EWXWEXPORT(void,wxListItem_GetFont)(wxListItem* self,wxFont* _ref)
{
	*_ref = self->GetFont();
}
	
EWXWEXPORT(void*,wxListCtrl_Create)(wxWindow* _prt,int _id, int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxListCtrl (_prt, _id,wxPoint(_lft, _top),wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int,wxListCtrl_SetForegroundColour)(wxListCtrl* self,wxColour* col)
{
	return (int)self->SetForegroundColour(*col);
}
	
EWXWEXPORT(int,wxListCtrl_SetBackgroundColour)(wxListCtrl* self,wxColour* col)
{
	return (int)self->SetBackgroundColour(*col);
}
	
EWXWEXPORT(int,wxListCtrl_GetColumn)(wxListCtrl* self,int col,wxListItem* item)
{
	return (int)self->GetColumn(col,*item);
}
	
EWXWEXPORT(int,wxListCtrl_SetColumn)(wxListCtrl* self,int col,wxListItem* item)
{
	return (int)self->SetColumn(col,*item);
}
	
EWXWEXPORT(int,wxListCtrl_GetColumnWidth)(void* self,int col)
{
	return ((wxListCtrl*)self)->GetColumnWidth(col);
}
	
EWXWEXPORT(bool,wxListCtrl_SetColumnWidth)(wxListCtrl* self,int col,int width)
{
	return self->SetColumnWidth(col, width);
}
	
EWXWEXPORT(int,wxListCtrl_GetCountPerPage)(wxListCtrl* self)
{
	return self->GetCountPerPage();
}
	
EWXWEXPORT(void*,wxListCtrl_GetEditControl)(wxListCtrl* self)
{
#ifdef __WIN32__
	return (void*)self->GetEditControl();
#else
	return NULL;
#endif
}
	
EWXWEXPORT(int,wxListCtrl_GetItem)(wxListCtrl* self,wxListItem* info)
{
	return (int)self->GetItem(*info);
}
	
EWXWEXPORT(bool,wxListCtrl_SetItemFromInfo)(wxListCtrl* self,wxListItem* info)
{
	return self->SetItem(*info);
}
	
EWXWEXPORT(bool,wxListCtrl_SetItem)(wxListCtrl* self,int index,int col,wxString* label,int imageId)
{
	return self->SetItem((long)index, col,*label, imageId);
}
	
EWXWEXPORT(int,wxListCtrl_GetItemState)(void* self,int item,int stateMask)
{
	return ((wxListCtrl*)self)->GetItemState((long)item, (long)stateMask);
}
	
EWXWEXPORT(bool,wxListCtrl_SetItemState)(wxListCtrl* self,int item,int state,int stateMask)
{
	return self->SetItemState((long)item, (long)state, (long)stateMask);
}
	
EWXWEXPORT(bool,wxListCtrl_SetItemImage)(wxListCtrl* self,int item,int image,int selImage)
{
	return self->SetItemImage((long)item, image, selImage);
}
	
EWXWEXPORT(wxString*,wxListCtrl_GetItemText)(wxListCtrl* self,int item)
{
	wxString *result = new wxString();
	*result = self->GetItemText((long)item);
	return result;
}
	
EWXWEXPORT(void,wxListCtrl_SetItemText)(wxListCtrl* self,int item,wxString* str)
{
	self->SetItemText((long)item,*str);
}
	
EWXWEXPORT(int,wxListCtrl_GetItemData)(wxListCtrl* self,int item)
{
	return (int)self->GetItemData((long)item);
}
	
EWXWEXPORT(bool,wxListCtrl_SetItemData)(wxListCtrl* self,int item,int data)
{
	return self->SetItemData((long)item, (long)data);
}
	
EWXWEXPORT(wxRect*,wxListCtrl_GetItemRect)(wxListCtrl* self,int item,int code)
{
	wxRect* rct = new wxRect();
	bool result = self->GetItemRect((long)item, *rct, code);
	if (result)
	{
		return rct;
	}
	return NULL;
}
	
EWXWEXPORT(wxPoint*,wxListCtrl_GetItemPosition)(wxListCtrl* self,int item)
{
	wxPoint* pos = new wxPoint();
	bool result = self->GetItemPosition((long)item, *pos);
	if (result)
	{
		return pos;
	}
	return NULL;
}
	
EWXWEXPORT(int,wxListCtrl_SetItemPosition)(wxListCtrl* self,int item,int x,int y)
{
	return self->SetItemPosition((long)item, wxPoint(x,y));
}
	
EWXWEXPORT(int,wxListCtrl_GetItemCount)(wxListCtrl* self)
{
	return self->GetItemCount();
}
	
EWXWEXPORT(int,wxListCtrl_GetColumnCount)(wxListCtrl* self)
{
	return self->GetColumnCount();
}
	
EWXWEXPORT(wxSize*,wxListCtrl_GetItemSpacing)(wxListCtrl* self,bool isSmall)
{
	wxSize* sz = new wxSize();
	*sz = self->GetItemSpacing();
	return sz;
}
	
EWXWEXPORT(int,wxListCtrl_GetSelectedItemCount)(wxListCtrl* self)
{
	return self->GetSelectedItemCount();
}
	
EWXWEXPORT(void,wxListCtrl_GetTextColour)(wxListCtrl* self,wxColour* colour)
{
	*colour = self->GetTextColour();
}
	
EWXWEXPORT(void,wxListCtrl_SetTextColour)(wxListCtrl* self,wxColour* col)
{
	self->SetTextColour(*col);
}
	
EWXWEXPORT(int,wxListCtrl_GetTopItem)(wxListCtrl* self)
{
	return (int)self->GetTopItem();
}
	
EWXWEXPORT(void,wxListCtrl_SetSingleStyle)(wxListCtrl* self,int style,bool add)
{
	self->SetSingleStyle((long)style, add);
}
	
EWXWEXPORT(void,wxListCtrl_SetWindowStyleFlag)(wxListCtrl* self,int style)
{
	self->SetWindowStyleFlag((long)style);
}
	
EWXWEXPORT(int,wxListCtrl_GetNextItem)(wxListCtrl* self,int item,int geometry,int state)
{
	return self->GetNextItem((long)item, geometry, state);
}
	
EWXWEXPORT(void*,wxListCtrl_GetImageList)(wxListCtrl* self,int which)
{
	return (void*)self->GetImageList(which);
}
	
EWXWEXPORT(void,wxListCtrl_SetImageList)(wxListCtrl* self,void* imageList,int which)
{
	self->SetImageList((wxImageList*)imageList, which);
}
	
EWXWEXPORT(bool,wxListCtrl_Arrange)(wxListCtrl* self,int flag)
{
	return self->Arrange(flag);
}
	
EWXWEXPORT(bool,wxListCtrl_DeleteItem)(wxListCtrl* self,int item)
{
	return self->DeleteItem((long)item);
}
	
EWXWEXPORT(bool,wxListCtrl_DeleteAllItems)(wxListCtrl* self)
{
	return self->DeleteAllItems();
}
	
EWXWEXPORT(bool,wxListCtrl_DeleteColumn)(wxListCtrl* self,int col)
{
	return self->DeleteColumn(col);
}
	
EWXWEXPORT(int,wxListCtrl_DeleteAllColumns)(wxListCtrl* self)
{
	return (int)self->DeleteAllColumns();
}
	
EWXWEXPORT(void,wxListCtrl_ClearAll)(wxListCtrl* self)
{
	self->ClearAll();
}
	
EWXWEXPORT(void,wxListCtrl_EditLabel)(wxListCtrl* self,int item)
{
	self->EditLabel((long)item);
}
	
EWXWEXPORT(bool,wxListCtrl_EndEditLabel)(wxListCtrl* self,bool cancel)
{
#ifdef __WIN32__
	return self->EndEditLabel(cancel);
#else
	return false;
#endif
}
	
EWXWEXPORT(bool,wxListCtrl_EnsureVisible)(wxListCtrl* self,int item)
{
	return self->EnsureVisible((long)item);
}
	
EWXWEXPORT(int,wxListCtrl_FindItem)(wxListCtrl* self,int start,wxString* str,bool partial)
{
	return (long)self->FindItem((long)start,* str, partial);
}
	
EWXWEXPORT(int,wxListCtrl_FindItemByData)(wxListCtrl* self,int start,int data)
{
	return (int)self->FindItem((long)start, (long)data);
}
	
EWXWEXPORT(int,wxListCtrl_FindItemByPosition)(wxListCtrl* self,int start,int x,int y,int direction)
{
	return (int)self->FindItem((long)start, wxPoint(x, y), direction);
}
	
EWXWEXPORT(int,wxListCtrl_HitTest)(wxListCtrl* self,int x,int y,void* flags)
{
	return self->HitTest(wxPoint(x, y),*((int*)flags));
}
	
EWXWEXPORT(int,wxListCtrl_InsertItem)(wxListCtrl* self,wxListItem* info)
{
	return (int)self->InsertItem(*info);
}
	
EWXWEXPORT(int,wxListCtrl_InsertItemWithData)(wxListCtrl* self,int index,wxString* label)
{
	return (int)self->InsertItem((long)index,*label);
}
	
EWXWEXPORT(int,wxListCtrl_InsertItemWithImage)(wxListCtrl* self,int index,int imageIndex)
{
	return (int)self->InsertItem((long)index, imageIndex);
}
	
EWXWEXPORT(int,wxListCtrl_InsertItemWithLabel)(wxListCtrl* self,int index,wxString* label,int imageIndex)
{
	return (int)self->InsertItem((long)index,*label, imageIndex);
}
	
EWXWEXPORT(int,wxListCtrl_InsertColumnFromInfo)(wxListCtrl* self,int col,wxListItem* info)
{
	return (int)self->InsertColumn((long)col,*info);
}
	
EWXWEXPORT(int,wxListCtrl_InsertColumn)(wxListCtrl* self,int col,wxString* heading,int format,int width)
{
	return (int)self->InsertColumn((long)col,* heading, format, width);
}
	
EWXWEXPORT(bool,wxListCtrl_ScrollList)(wxListCtrl* self,int dx,int dy)
{
	return self->ScrollList(dx, dy);
}
	
EWXWEXPORT(bool,wxListCtrl_SortItems)(wxListCtrl* self,void* fnc,void* obj)
{
	EiffelSort srt = {obj, (EiffelSortFunc)fnc};
	return self->SortItems(ListCmp, (wxIntPtr)&srt);
}
	
EWXWEXPORT(void,wxListCtrl_UpdateStyle)(wxListCtrl* self)
{
#ifdef __WIN32__
	self->UpdateStyle();
#endif
}

EWXWEXPORT(wxFont*,wxListCtrl_GetItemFont)(wxListCtrl* self, long item)
{
  wxFont* fnt = new wxFont;
  *fnt = self->GetItemFont(item);
  return fnt;
}

EWXWEXPORT(bool,wxListCtrl_IsVirtual)(wxListCtrl* self)
{
  return self->IsVirtual();
}

EWXWEXPORT(void,wxListCtrl_RefreshItem)(wxListCtrl* self, long item)
{
  self->RefreshItem(item);
}

EWXWEXPORT(void,wxListCtrl_RefreshItems)(wxListCtrl* self, long from, long to)
{
  self->RefreshItems(from, to);
}

}
