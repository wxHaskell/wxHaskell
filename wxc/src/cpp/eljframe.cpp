#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(bool,wxTopLevelWindow_EnableCloseButton)(wxTopLevelWindow* self,bool enable)
{
  return self->EnableCloseButton(enable);
}

EWXWEXPORT(void*,wxTopLevelWindow_GetDefaultButton)(wxTopLevelWindow* self)
{
  return (void*)self->GetDefaultItem();
}

EWXWEXPORT(void*,wxTopLevelWindow_GetDefaultItem)(wxTopLevelWindow* self)
{
  return (void*)self->GetDefaultItem();
}

EWXWEXPORT(void*,wxTopLevelWindow_GetIcon)(wxTopLevelWindow* self)
{
  static wxIcon tmp = self->GetIcon();
  return (void*)&tmp;
}

EWXWEXPORT(wxString*,wxTopLevelWindow_GetTitle)(wxTopLevelWindow* self)
{
  wxString *result = new wxString();
  *result = self->GetTitle();
  return result;
}

EWXWEXPORT(void,wxTopLevelWindow_Iconize)(wxTopLevelWindow* self,bool iconize)
{
  ((wxTopLevelWindow*)self)->Iconize(iconize);
}

EWXWEXPORT(bool,wxTopLevelWindow_IsActive)(wxTopLevelWindow* self)
{
  return self->IsActive();
}

EWXWEXPORT(bool,wxTopLevelWindow_IsIconized)(wxTopLevelWindow* self)
{
  return self->IsIconized();
}

EWXWEXPORT(bool,wxTopLevelWindow_IsMaximized)(wxTopLevelWindow* self)
{
  return self->IsMaximized();
}

EWXWEXPORT(void,wxTopLevelWindow_Maximize)(void* self,bool iconize)
{
  ((wxTopLevelWindow*)self)->Maximize(iconize);
}

EWXWEXPORT(void,wxTopLevelWindow_RequestUserAttention)(void* self,int flags)
{
  ((wxTopLevelWindow*)self)->RequestUserAttention(flags);
}

EWXWEXPORT(void,wxTopLevelWindow_SetDefaultButton)(void* self,void* _item)
{
  ((wxTopLevelWindow*)self)->SetDefaultItem((wxButton*)_item);
}

EWXWEXPORT(void,wxTopLevelWindow_SetDefaultItem)(void* self,wxWindow* _item)
{
  ((wxTopLevelWindow*)self)->SetDefaultItem( _item);
}

EWXWEXPORT(void,wxTopLevelWindow_SetIcon)(void* self,wxIcon* _icons)
{
  ((wxTopLevelWindow*)self)->SetIcon(*_icons);
}

EWXWEXPORT(void,wxTopLevelWindow_SetIcons)(void* self,void* _icons)
{
  ((wxTopLevelWindow*)self)->SetIcons(*((wxIconBundle*)_icons));
}

EWXWEXPORT(void,wxTopLevelWindow_SetMaxSize)(void* self,int _w,int _h)
{
  ((wxTopLevelWindow*)self)->SetMaxSize(wxSize(_w, _h));
}
           
EWXWEXPORT(void,wxTopLevelWindow_SetMinSize)(void* self,int _w,int _h)
{
  ((wxTopLevelWindow*)self)->SetMinSize(wxSize(_w, _h));
}

EWXWEXPORT(void,wxTopLevelWindow_SetTitle)(void* self,wxString* _str)
{
  ((wxTopLevelWindow*)self)->SetTitle(*_str);
}

EWXWEXPORT(wxFrame*,wxFrame_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return new wxFrame (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(wxStatusBar*,wxFrame_CreateStatusBar)(wxFrame* self,int number,int style)
{
	return self->CreateStatusBar(number, style, 1);
}
	
#if wxVERSION_NUMBER >= 2800
EWXWEXPORT(void,wxFrame_Maximize)(wxFrame* self)
{
	self->Maximize();
}
#endif
	
EWXWEXPORT(void,wxFrame_Restore)(wxFrame* self)
{
	self->Restore();
}
	
#if wxVERSION_NUMBER >= 2800
EWXWEXPORT(void,wxFrame_Iconize)(wxFrame* self)
{
	self->Iconize();
}
	
EWXWEXPORT(bool,wxFrame_IsMaximized)(wxFrame* self)
{
	return self->IsMaximized();
}
	
EWXWEXPORT(bool,wxFrame_IsIconized)(wxFrame* self)
{
	return self->IsIconized();
}
	
EWXWEXPORT(void*,wxFrame_GetIcon)(wxFrame* self)
{
    static wxIcon icon;
    icon = self->GetIcon();
    return (void*) &icon;
}
	
EWXWEXPORT(void,wxFrame_SetIcon)(wxFrame* self,wxIcon* _icon)
{
	self->SetIcon(*_icon);
}
#endif
	
EWXWEXPORT(int,wxFrame_GetClientAreaOrigin_left)(wxFrame* self)
{
	return self->GetClientAreaOrigin().x;
}
	
EWXWEXPORT(int,wxFrame_GetClientAreaOrigin_top)(wxFrame* self)
{
	return self->GetClientAreaOrigin().y;
}
	
EWXWEXPORT(void,wxFrame_SetMenuBar)(wxFrame* self,wxMenuBar* menubar)
{
	self->SetMenuBar(menubar);
}
	
EWXWEXPORT(wxMenuBar*,wxFrame_GetMenuBar)(wxFrame* self)
{
	return self->GetMenuBar();
}
	
EWXWEXPORT(wxStatusBar*,wxFrame_GetStatusBar)(wxFrame* self)
{
	return self->GetStatusBar();
}
	
EWXWEXPORT(void,wxFrame_SetStatusBar)(wxFrame* self,wxStatusBar* statBar)
{
	self->SetStatusBar(statBar);
}
	
EWXWEXPORT(void,wxFrame_SetStatusText)(wxFrame* self,wxString* _txt,int _number)
{
	self->SetStatusText(*_txt, _number);
}
	
EWXWEXPORT(void,wxFrame_SetStatusWidths)(wxFrame* self,int _n,void* _widths_field)
{
	self->SetStatusWidths(_n, (int*)_widths_field);
}
	
EWXWEXPORT(void*,wxFrame_CreateToolBar)(wxFrame* self,long style)
{
	return (void*)self->CreateToolBar(style, 1);
}
	
EWXWEXPORT(void*,wxFrame_GetToolBar)(wxFrame* self)
{
	return (void*)self->GetToolBar();
}
	
EWXWEXPORT(void,wxFrame_SetToolBar)(wxFrame* self,wxToolBar* _toolbar)
{
	self->SetToolBar(_toolbar);
}

#if wxVERSION_NUMBER >= 2400 && wxVERSION_NUMBER < 2800
EWXWEXPORT(void,wxFrame_SetIcons)(wxFrame* self,void* _icons)
{
	self->SetIcons(*((wxIconBundle*)_icons));
}
#endif

}
