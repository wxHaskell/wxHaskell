#include "wrapper.h"

extern "C"
{

EWXWEXPORT(wxMenu*,wxMenu_Create)(wxString* title,long style)
{
	return new wxMenu(*title, style);
}
	
EWXWEXPORT(void,wxMenu_DeletePointer)(wxMenu* self)
{
	delete self;
}
	
EWXWEXPORT(void,wxMenu_AppendSeparator)(wxMenu* self)
{
	self->AppendSeparator();
}
	
EWXWEXPORT(void,wxMenu_Append)(wxMenu* self,int id,wxString* text,wxString* help,bool isCheckable)
{
	self->Append(id,*text,*help, isCheckable);
}
	
EWXWEXPORT(void,wxMenu_AppendSub)(wxMenu* self,int id,wxString* text,wxMenu* submenu,wxString* help)
{
	self->Append(id,*text,submenu,*help);
}
	
EWXWEXPORT(void,wxMenu_AppendItem)(wxMenu* self,wxMenuItem* _itm)
{
	self->Append(_itm);
}
	
EWXWEXPORT(void,wxMenu_Break)(wxMenu* self)
{
	self->Break();
}
	
EWXWEXPORT(void,wxMenu_Insert)(wxMenu* self,size_t pos,int id,wxString* text,wxString* help,bool isCheckable)
{
	self->Insert(pos, id,*text,*help, isCheckable);
}
	
EWXWEXPORT(void,wxMenu_InsertSub)(wxMenu* self,size_t pos,int id,wxString* text,wxMenu* submenu,wxString* help)
{
	self->Insert(pos, id,*text, submenu,*help);
}
	
EWXWEXPORT(void,wxMenu_InsertItem)(wxMenu* self,int pos,wxMenuItem* _itm)
{
	self->Insert((size_t)pos, _itm);
}
	
EWXWEXPORT(void,wxMenu_Prepend)(wxMenu* self,int id,wxString* text,wxString* help,bool isCheckable)
{
	self->Prepend(id,*text,*help, isCheckable);
}
	
EWXWEXPORT(void,wxMenu_PrependSub)(wxMenu* self,int id,wxString* text,wxMenu* submenu,wxString* help)
{
	self->Prepend(id,*text, submenu,*help);
}
	
EWXWEXPORT(void,wxMenu_PrependItem)(wxMenu* self,wxMenuItem* _itm)
{
	self->Prepend(_itm);
}
	
EWXWEXPORT(void,wxMenu_RemoveByItem)(wxMenu* self,wxMenuItem* item)
{
	self->Remove(item);
}
	
EWXWEXPORT(void,wxMenu_RemoveById)(wxMenu* self,int id,void* _itm)
{
	*((void**)_itm) = (void*)self->Remove(id);
}
	
EWXWEXPORT(void,wxMenu_DeleteById)(wxMenu* self,int id)
{
	self->Delete(id);
}
	
EWXWEXPORT(void,wxMenu_DeleteByItem)(wxMenu* self,wxMenuItem* _itm)
{
	self->Delete(_itm);
}
	
EWXWEXPORT(void,wxMenu_DestroyById)(wxMenu* self,int id)
{
	self->Destroy(id);
}
	
EWXWEXPORT(void,wxMenu_DestroyByItem)(wxMenu* self,wxMenuItem* _itm)
{
	self->Destroy(_itm);
}
	
EWXWEXPORT(size_t,wxMenu_GetMenuItemCount)(wxMenu* self)
{
	return self->GetMenuItemCount();
}
	
EWXWEXPORT(int,wxMenu_GetMenuItems)(wxMenu* self,void* _lst)
{
	if (_lst)
	{
		for (unsigned int i = 0; i < self->GetMenuItems().GetCount(); i++)
			((void**)_lst)[i] = self->GetMenuItems().Item(i)->GetData();
	}
	return self->GetMenuItems().GetCount();
}
	
EWXWEXPORT(int,wxMenu_FindItemByLabel)(wxMenu* self,wxString* itemString)
{
	return self->FindItem(*itemString);
}
	
EWXWEXPORT(wxMenuItem*,wxMenu_FindItem)(wxMenu* self,int id)
{
	return self->FindItem(id);
}
	
EWXWEXPORT(void,wxMenu_Enable)(wxMenu* self,int id,bool enable)
{
	self->Enable(id, enable);
}
	
EWXWEXPORT(bool,wxMenu_IsEnabled)(wxMenu* self,int id)
{
	return self->IsEnabled(id);
}
	
EWXWEXPORT(void,wxMenu_Check)(wxMenu* self,int id,bool check)
{
	self->Check(id, check);
}
	
EWXWEXPORT(bool,wxMenu_IsChecked)(wxMenu* self,int id)
{
	return self->IsChecked(id);
}
	
EWXWEXPORT(void,wxMenu_SetLabel)(wxMenu* self,int id,wxString* label)
{
	self->SetLabel(id,*label);
}
	
EWXWEXPORT(wxString*, wxMenu_GetLabelText)(wxMenu* self, int id)
{
	wxString *result = new wxString();
	*result = self->GetLabelText(id);
	return result;
}
	
EWXWEXPORT(void,wxMenu_SetHelpString)(wxMenu* self,int id,wxString* helpString)
{
	self->SetHelpString(id,*helpString);
}
	
EWXWEXPORT(wxString*,wxMenu_GetHelpString)(wxMenu* self,int id)
{
	wxString *result = new wxString();
	*result = self->GetHelpString(id);
	return result;
}
	
EWXWEXPORT(void,wxMenu_SetTitle)(void* _obj,wxString* title)
{
	((wxMenu*)_obj)->SetTitle(*title);
}
	
EWXWEXPORT(wxString*,wxMenu_GetTitle)(wxMenu* self)
{
	wxString *result = new wxString();
	*result = self->GetTitle();
	return result;
}
	
EWXWEXPORT(void,wxMenu_SetClientData)(wxMenu* self,void* clientData)
{
	self->SetClientData(clientData);
}
	
EWXWEXPORT(void*,wxMenu_GetClientData)(wxMenu* self)
{
	return (void*)self->GetClientData();
}
	
EWXWEXPORT(void,wxMenu_SetEventHandler)(wxMenu* self,wxEvtHandler* handler)
{
	self->SetEventHandler(handler);
}
	
EWXWEXPORT(void,wxMenu_SetInvokingWindow)(wxMenu* self,wxWindow* win)
{
	self->SetInvokingWindow(win);
}
	
EWXWEXPORT(void*,wxMenu_GetInvokingWindow)(wxMenu* self)
{
	return (void*)self->GetInvokingWindow();
}
	
EWXWEXPORT(int,wxMenu_GetStyle)(wxMenu* self)
{
	return self->GetStyle();
}
	
EWXWEXPORT(void,wxMenu_UpdateUI)(wxMenu* self,wxEvtHandler* source)
{
	self->UpdateUI(source);
}
	
EWXWEXPORT(bool,wxMenu_IsAttached)(wxMenu* self)
{
	return self->IsAttached();
}
	
EWXWEXPORT(void,wxMenu_SetParent)(wxMenu* self,wxMenu* parent)
{
	self->SetParent(parent);
}
	
EWXWEXPORT(void*,wxMenu_GetParent)(wxMenu* self)
{
	return (void*)self->GetParent();
}
	

EWXWEXPORT(wxMenuItem*,wxMenuItem_Create)()
{
	return new wxMenuItem();
}
	
EWXWEXPORT(void,wxMenuItem_Delete)(wxMenuItem* self)
{
	delete self;
}

EWXWEXPORT(wxMenu*,wxMenuItem_GetMenu)(wxMenuItem* self)
{
	return self->GetMenu();
}
	
EWXWEXPORT(void,wxMenuItem_SetId)(wxMenuItem* self,int id)
{
	self->SetId(id);
}
	
EWXWEXPORT(int,wxMenuItem_GetId)(wxMenuItem* self)
{
	return self->GetId();
}
	
EWXWEXPORT(bool,wxMenuItem_IsSeparator)(wxMenuItem* self)
{
	return self->IsSeparator();
}
	
EWXWEXPORT(void, wxMenuItem_SetItemLabel)(wxMenuItem* self, wxString* str)
{
	self->SetItemLabel(*str);
}
	
EWXWEXPORT(wxString*, wxMenuItem_GetItemLabelText)(wxMenuItem* self)
{
	wxString *result = new wxString();
	*result = self->GetItemLabelText();
	return result;
}
	
EWXWEXPORT(wxString*, wxMenuItem_GetItemLabel)(wxMenuItem* self)
{
	wxString *result = new wxString();
	*result = self->GetItemLabel();
	return result;
}

EWXWEXPORT(wxString*, wxMenuItem_GetLabelText)(wxString* text)
{
	wxString *result = new wxString();
	*result = wxMenuItem::GetLabelText(* text);
	return result;
}

EWXWEXPORT(void,wxMenuItem_SetCheckable)(wxMenuItem* self,bool checkable)
{
	self->SetCheckable(checkable);
}
	
EWXWEXPORT(bool,wxMenuItem_IsCheckable)(wxMenuItem* self)
{
	return self->IsCheckable();
}
	
EWXWEXPORT(bool,wxMenuItem_IsSubMenu)(wxMenuItem* self)
{
	return self->IsSubMenu();
}
	
EWXWEXPORT(void,wxMenuItem_SetSubMenu)(wxMenuItem* self,wxMenu* menu)
{
	self->SetSubMenu(menu);
}
	
EWXWEXPORT(wxMenu*,wxMenuItem_GetSubMenu)(wxMenuItem* self)
{
	return self->GetSubMenu();
}
	
EWXWEXPORT(void,wxMenuItem_Enable)(wxMenuItem* self,bool enable)
{
	self->Enable(enable);
}
	
EWXWEXPORT(bool,wxMenuItem_IsEnabled)(wxMenuItem* self)
{
	return self->IsEnabled();
}
	
EWXWEXPORT(void,wxMenuItem_Check)(wxMenuItem* self,bool check)
{
	self->Check(check);
}
	
EWXWEXPORT(bool,wxMenuItem_IsChecked)(wxMenuItem* self)
{
	return self->IsChecked();
}
	
EWXWEXPORT(void,wxMenuItem_SetHelp)(wxMenuItem* self,wxString* str)
{
	self->SetHelp(*str);
}
	
EWXWEXPORT(wxString*,wxMenuItem_GetHelp)(wxMenuItem* self)
{
	wxString *result = new wxString();
	*result = self->GetHelp();
	return result;
}

}
