#include "wrapper.h"

class wxcTreeItemData : public wxTreeItemData
{
  private:
    wxClosure* m_closure;

  public:
    wxcTreeItemData( wxClosure* closure ) { 
      m_closure = closure;
    }

    ~wxcTreeItemData() {
      if (m_closure) delete m_closure;
    }

    wxClosure* GetClientClosure() { 
      return m_closure; 
    }

    void SetClientClosure( wxClosure* closure ) {
      if (m_closure) delete m_closure;
      m_closure = closure;
    }
};

extern "C"
{

EWXWEXPORT(wxcTreeItemData*,wxcTreeItemData_Create)(wxClosure* closure)
{
	return new wxcTreeItemData(closure);
}

EWXWEXPORT(wxClosure*,wxcTreeItemData_GetClientClosure)(wxcTreeItemData* self)
{
	return self->GetClientClosure();
}

EWXWEXPORT(void,wxcTreeItemData_SetClientClosure)(wxcTreeItemData* self,wxClosure* closure)
{
	self->SetClientClosure(closure);
}

EWXWEXPORT(void*,wxTreeItemId_Create)()
{
	return new wxTreeItemId();
}

EWXWEXPORT(void,wxTreeItemId_Delete)(wxTreeItemId* self)
{
	delete self;
}

EWXWEXPORT(bool,wxTreeItemId_IsOk)(wxTreeItemId* self)
{
	return self->IsOk();
}

EWXWEXPORT(wxTreeItemId*,wxTreeItemId_Clone)(wxTreeItemId* self)
{
	wxTreeItemId* clone = new wxTreeItemId();
	*clone = *self;
	return clone;
}

// FIXME: wxHaskell uses this function in Graphics.UI.WXCore.WxcTypes.withTreeItemIdPtr
// to make wxTreeItemId.
//
// But wxWidgets' document says: wxTreemItemIds are not meant to be constructed
// explicitly by the user; they are returned by the wxTreeCtrl functions instead.
//
// http://www.wxwindows.org/manuals/2.8/wx_wxtreeitemid.html#wxtreeitemid
//
// So we must remove this function and replace treeItemId implementation in the
// funture.
EWXWEXPORT(wxTreeItemId*,wxTreeItemId_CreateFromValue)(intptr_t value)
{
#if wxVERSION_NUMBER < 2800
	return new wxTreeItemId( value );
#else
	// TODO: This function should be removed. No longer any equivalent in wxWidgets
	wxTreeItemId *item = new wxTreeItemId();
	item->m_pItem = reinterpret_cast<wxTreeItemIdValue>(value);
	return item;
#endif
}

EWXWEXPORT(intptr_t,wxTreeItemId_GetValue)(wxTreeItemId* self)
{
	return (intptr_t)(self->m_pItem);
}


EWXWEXPORT(wxKeyEvent*,wxTreeEvent_GetKeyEvent)(wxTreeEvent* self)
{
	return (wxKeyEvent*)&(self->GetKeyEvent());
}

EWXWEXPORT(bool,wxTreeEvent_IsEditCancelled)(wxTreeEvent* self)
{
	return self->IsEditCancelled();
}

EWXWEXPORT(void,wxTreeEvent_Allow)(wxTreeEvent* self)
{
	self->Allow();
}


EWXWEXPORT(wxTreeCtrl*,wxTreeCtrl_Create)(void* _obj,void* _cmp,wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return new wxTreeCtrl (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(wxTreeCtrl*,wxTreeCtrl_Create2)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return new wxTreeCtrl (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, wxDefaultValidator);
}

EWXWEXPORT(int,wxTreeCtrl_GetCount)(wxTreeCtrl* self)
{
	int result =(int)self->GetCount();
#ifdef __WXGTK__
	wxTreeItemId t = self->GetRootItem();
	if (t.IsOk()) result++;
#endif
	return result;
}
	
EWXWEXPORT(int,wxTreeCtrl_GetIndent)(wxTreeCtrl* self)
{
	return self->GetIndent();
}
	
EWXWEXPORT(void,wxTreeCtrl_SetIndent)(wxTreeCtrl* self,int indent)
{
	self->SetIndent(indent);
}
	
EWXWEXPORT(int,wxTreeCtrl_GetSpacing)(wxTreeCtrl* self)
{
	return (int)self->GetSpacing();
}
	
EWXWEXPORT(void,wxTreeCtrl_SetSpacing)(wxTreeCtrl* self,int spacing)
{
	self->SetSpacing(spacing);
}
	
EWXWEXPORT(wxImageList*,wxTreeCtrl_GetImageList)(wxTreeCtrl* self)
{
	return self->GetImageList();
}
	
EWXWEXPORT(wxImageList*,wxTreeCtrl_GetStateImageList)(wxTreeCtrl* self)
{
	return self->GetStateImageList();
}

EWXWEXPORT(void,wxTreeCtrl_AssignImageList)(wxTreeCtrl* self,wxImageList* imageList)
{
	self->AssignImageList(imageList);
}

EWXWEXPORT(void,wxTreeCtrl_AssignStateImageList)(wxTreeCtrl* self,wxImageList* imageList)
{
	self->AssignStateImageList(imageList);
}

/*
EWXWEXPORT(wxImageList*,wxTreeCtrl_GetButtonsImageList)(wxTreeCtrl* self)
{
	return self->GetButtonsImageList();
}

EWXWEXPORT(void,wxTreeCtrl_SetButtonsImageList)(wxTreeCtrl* self,wxImageList* imageList)
{
	self->SetButtonsImageList(imageList);
}

EWXWEXPORT(void,wxTreeCtrl_AssignButtonsImageList)(wxTreeCtrl* self,wxImageList* imageList)
{
	self->AssignButtonsImageList(imageList);
}
*/

EWXWEXPORT(void,wxTreeCtrl_SetImageList)(wxTreeCtrl* self,wxImageList* imageList)
{
	self->SetImageList(imageList);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetStateImageList)(wxTreeCtrl* self,wxImageList* imageList)
{
	self->SetStateImageList(imageList);
}
	
EWXWEXPORT(wxString*,wxTreeCtrl_GetItemText)(wxTreeCtrl* self,void* item)
{
	wxString *result = new wxString();
	*result = self->GetItemText(*(wxTreeItemId*)item);
	return result;
}
	
EWXWEXPORT(int,wxTreeCtrl_GetItemImage)(wxTreeCtrl* self,wxTreeItemId* item,int which)
{
	return self->GetItemImage(*item, (wxTreeItemIcon) which);
}
	
EWXWEXPORT(void*,wxTreeCtrl_GetItemData)(wxTreeCtrl* self,wxTreeItemId* item)
{
	return ((wxcTreeItemData*)self->GetItemData(* item))->GetClientClosure();
}

EWXWEXPORT(void*,wxTreeCtrl_GetItemClientClosure)(wxTreeCtrl* self,wxTreeItemId* item)
{
	return ((wxcTreeItemData*)self->GetItemData(* item))->GetClientClosure();
}

	
EWXWEXPORT(void,wxTreeCtrl_SetItemText)(wxTreeCtrl* self,wxTreeItemId* item,wxString* text)
{
	self->SetItemText(* item,*text);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemImage)(wxTreeCtrl* self,wxTreeItemId* item,int image,int which)
{
	self->SetItemImage(*item, image, (wxTreeItemIcon)which);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemData)(wxTreeCtrl* self,wxTreeItemId* item,wxClosure* closure)
{
	self->SetItemData(*item, new wxcTreeItemData (closure));
}

EWXWEXPORT(void,wxTreeCtrl_SetItemClientClosure)(wxTreeCtrl* self,wxTreeItemId* item,wxClosure* closure)
{
        wxTreeItemData* oldData = self->GetItemData(* item);
        /* bit unsafe: might delete twice but it is definitely ok on MSW 2.4.1 */
        if (oldData) delete oldData;
	self->SetItemData(* item, new wxcTreeItemData (closure));
}

	
EWXWEXPORT(void,wxTreeCtrl_SetItemHasChildren)(wxTreeCtrl* self,wxTreeItemId* item,bool has)
{
	self->SetItemHasChildren(*item, has);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemBold)(wxTreeCtrl* self,wxTreeItemId* item,bool bold)
{
	self->SetItemBold(* item, bold);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemDropHighlight)(wxTreeCtrl* self,wxTreeItemId* item,bool highlight)
{
#ifdef __WIN32__
	self->SetItemDropHighlight(* item, highlight);
#endif
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemTextColour)(wxTreeCtrl* self,wxTreeItemId* item,wxColour* col)
{
	self->SetItemTextColour(*item,*col);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemBackgroundColour)(wxTreeCtrl* self,wxTreeItemId* item,wxColour* col)
{
	self->SetItemBackgroundColour(* item,* col);
}
	
EWXWEXPORT(void,wxTreeCtrl_SetItemFont)(wxTreeCtrl* self,wxTreeItemId* item,wxFont* font)
{
	self->SetItemFont(* item,*font);
}
	
EWXWEXPORT(bool,wxTreeCtrl_IsVisible)(wxTreeCtrl* self,wxTreeItemId* item)
{
	return self->IsVisible(*item);
}
	
EWXWEXPORT(int,wxTreeCtrl_ItemHasChildren)(wxTreeCtrl* self,wxTreeItemId* item)
{
	return (int)self->ItemHasChildren(* item);
}
	
EWXWEXPORT(bool,wxTreeCtrl_IsExpanded)(wxTreeCtrl* self,wxTreeItemId* item)
{
	return self->IsExpanded(*item);
}
	
EWXWEXPORT(bool,wxTreeCtrl_IsSelected)(wxTreeCtrl* self,wxTreeItemId* item)
{
	return self->IsSelected(*item);
}
	
EWXWEXPORT(bool,wxTreeCtrl_IsBold)(wxTreeCtrl* self,wxTreeItemId* item)
{
	return self->IsBold(*item);
}
	
EWXWEXPORT(int,wxTreeCtrl_GetChildrenCount)(wxTreeCtrl* self,wxTreeItemId* item,int recursively)
{
	return self->GetChildrenCount(* item, recursively);
}
	
EWXWEXPORT(void,wxTreeCtrl_GetRootItem)(wxTreeCtrl* self,wxTreeItemId* _item)
{
	*_item = self->GetRootItem();
}
	
EWXWEXPORT(void,wxTreeCtrl_GetSelection)(wxTreeCtrl* self,wxTreeItemId* _item)
{
	*_item = self->GetSelection();
}
	
EWXWEXPORT(int,wxTreeCtrl_GetSelections)(wxTreeCtrl* self,intptr_t* selections)
{
	int result = 0;
	wxArrayTreeItemIds sel;
	result = self->GetSelections(sel);
	
	if (selections)
	{
          for (int i = 0; i < result; i++) {
            /*
			*(((wxTreeItemId**)selections)[i]) = sel[i];
            */
#if (wxVERSION_NUMBER < 2800)
	    #if wxCHECK_VERSION(2,5,0)
            selections[i] = (intptr_t)(((wxTreeItemId*)sel[i])->m_pItem);
        #else
	        selections[i] = (intptr_t)(sel[i].m_pItem);
	    #endif
#else
            selections[i] = (intptr_t)(((wxTreeItemId)sel[i]).m_pItem);
#endif
	  }
	}
	return result;
}
	
EWXWEXPORT(void,wxTreeCtrl_GetParent)(wxTreeCtrl* self,wxTreeItemId* item,wxTreeItemId* _item)
{
#if wxVERSION_NUMBER < 2400
	*_item = self->GetParent(*item);
#else
	*_item = self->GetItemParent(*item);
#endif
}
	
EWXWEXPORT(void,wxTreeCtrl_GetFirstChild)(wxTreeCtrl* self,wxTreeItemId* item,void* cookie,wxTreeItemId* _item)
{
#if wxVERSION_NUMBER < 2600
	*_item = self->GetFirstChild(*item,*((long*)cookie));
#else
	*_item = self->GetFirstChild(*item, cookie);
#endif
}
	
EWXWEXPORT(void,wxTreeCtrl_GetNextChild)(wxTreeCtrl* self,wxTreeItemId* item,void* cookie,wxTreeItemId* _item)
{
#if wxVERSION_NUMBER < 2600
	*_item = self->GetNextChild(*item,*((long*)cookie));
#else
	*_item = self->GetNextChild(*item, cookie);
#endif
}
	
EWXWEXPORT(void,wxTreeCtrl_GetLastChild)(wxTreeCtrl* self,wxTreeItemId* item,wxTreeItemId* _item)
{
	*_item = self->GetLastChild(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_GetNextSibling)(wxTreeCtrl* self,wxTreeItemId* item,wxTreeItemId* _item)
{
	*_item = self->GetNextSibling(* item);
}
	
EWXWEXPORT(void,wxTreeCtrl_GetPrevSibling)(wxTreeCtrl* self,wxTreeItemId* item,wxTreeItemId* _item)
{
	*_item = self->GetPrevSibling(* item);
}
	
EWXWEXPORT(void,wxTreeCtrl_GetFirstVisibleItem)(wxTreeCtrl* self,wxTreeItemId* _item)
{
	*_item = self->GetFirstVisibleItem();
}

EWXWEXPORT(void,wxTreeCtrl_GetNextVisible)(wxTreeCtrl* self,wxTreeItemId* item,wxTreeItemId* _item)
{
	*_item = self->GetNextVisible(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_GetPrevVisible)(wxTreeCtrl* self,wxTreeItemId* item,wxTreeItemId* _item)
{
	*_item = self->GetPrevVisible(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_AddRoot)(wxTreeCtrl* self,wxString* text,int image,int selectedImage,wxClosure* data,wxTreeItemId* _item)
{
	*_item = self->AddRoot(*text, image, selectedImage, new wxcTreeItemData(data));
}
	
EWXWEXPORT(void,wxTreeCtrl_PrependItem)(wxTreeCtrl* self,wxTreeItemId* parent,wxString* text,int image,int selectedImage,wxClosure* data,wxTreeItemId* _item)
{
	*_item = self->PrependItem(*parent,*text, image, selectedImage, new wxcTreeItemData(data));
}
	
EWXWEXPORT(void,wxTreeCtrl_InsertItem)(wxTreeCtrl* self,wxTreeItemId* parent,wxTreeItemId* idPrevious,wxString* text,int image,int selectedImage,wxClosure* data,wxTreeItemId* _item)
{
	*_item = self->InsertItem(*parent,*idPrevious,*text, image, selectedImage, new wxcTreeItemData(data));
}

EWXWEXPORT(void,wxTreeCtrl_InsertItem2)(wxTreeCtrl* self,wxTreeItemId* parent,wxTreeItemId* idPrevious,wxString* text,int image,int selectedImage,wxClosure* closure,wxTreeItemId* _item)
{
	*_item = self->InsertItem(*parent,*idPrevious,*text, image, selectedImage, new wxcTreeItemData(closure));
}

	
EWXWEXPORT(void,wxTreeCtrl_InsertItemByIndex)(wxTreeCtrl* self,wxTreeItemId* parent,int index,wxString* text,int image,int selectedImage,wxClosure* data,wxTreeItemId* _item)
{
	*_item = self->InsertItem(*parent, index,*text, image, selectedImage, new wxcTreeItemData(data));
}

EWXWEXPORT(void,wxTreeCtrl_InsertItemByIndex2)(wxTreeCtrl* self,wxTreeItemId* parent,int index,wxString* text,int image,int selectedImage,wxClosure* data,wxTreeItemId* _item)
{
	*_item = self->InsertItem(*parent, index,*text, image, selectedImage, new wxcTreeItemData(data));
}

	
EWXWEXPORT(void,wxTreeCtrl_AppendItem)(wxTreeCtrl* self,wxTreeItemId* parent,wxString* text,int image,int selectedImage,wxClosure* data,wxTreeItemId* _item)
{
	*_item = self->AppendItem(* parent,*text, image, selectedImage, new wxcTreeItemData(data));
}
	
EWXWEXPORT(void,wxTreeCtrl_Delete)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->Delete(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_DeleteChildren)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->DeleteChildren(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_DeleteAllItems)(wxTreeCtrl* self)
{
	self->DeleteAllItems();
}
	
EWXWEXPORT(void,wxTreeCtrl_Expand)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->Expand(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_Collapse)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->Collapse(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_CollapseAndReset)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->CollapseAndReset(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_Toggle)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->Toggle(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_Unselect)(wxTreeCtrl* self)
{
	self->Unselect();
}
	
EWXWEXPORT(void,wxTreeCtrl_UnselectAll)(wxTreeCtrl* self)
{
	self->UnselectAll();
}
	
EWXWEXPORT(void,wxTreeCtrl_SelectItem)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->SelectItem(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_EnsureVisible)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->EnsureVisible(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_ScrollTo)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->ScrollTo(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_EditLabel)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->EditLabel(*item);
}
	
EWXWEXPORT(void*,wxTreeCtrl_GetEditControl)(wxTreeCtrl* self)
{
#ifdef __WIN32__
	return (void*)self->GetEditControl();
#else
	return NULL;
#endif
}
	
EWXWEXPORT(void,wxTreeCtrl_EndEditLabel)(wxTreeCtrl* self,wxTreeItemId* item,bool discardChanges)
{
#ifdef __WIN32__
	self->EndEditLabel(*item, discardChanges);
#endif
}
	
EWXWEXPORT(int,wxTreeCtrl_OnCompareItems)(wxTreeCtrl* self,wxTreeItemId* item1,wxTreeItemId* item2)
{
	return self->OnCompareItems(*item1,*item2);
}
	
EWXWEXPORT(void,wxTreeCtrl_SortChildren)(wxTreeCtrl* self,wxTreeItemId* item)
{
	self->SortChildren(*item);
}
	
EWXWEXPORT(void,wxTreeCtrl_HitTest)(wxTreeCtrl* self,int _x,int _y,void* flags,wxTreeItemId* _item)
{
	*_item = self->HitTest(wxPoint(_x, _y),*((int*)flags));
}
	
EWXWEXPORT(wxRect*,wxTreeCtrl_GetBoundingRect)(wxTreeCtrl* self,wxTreeItemId* item,bool textOnly)
{
	wxRect * const rct = new wxRect(-1,-1,-1,-1);
	self->GetBoundingRect(*item, *rct, textOnly);
	return rct;
}
	
}
