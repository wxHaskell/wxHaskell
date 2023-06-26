#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxBitmap_Create)(void* _data,int _width,int _height,int _depth)
{
	return (void*) new wxBitmap((const char*)_data, _width, _height, _depth);
}

EWXWEXPORT(void*,wxBitmap_CreateFromXPM)(void* _data)
{
	return (void*) new wxBitmap((const char**)_data);
}

EWXWEXPORT(void*,wxBitmap_CreateEmpty)(int _width,int _height,int _depth)
{
	return (void*) new wxBitmap(_width, _height, _depth);
}

EWXWEXPORT(void*,wxBitmap_CreateLoad)(wxString* name,int type)
{
#if wxVERSION_NUMBER >= 2400
	return (void*) new wxBitmap(*name, (wxBitmapType)type);
#else
	return (void*) new wxBitmap(*name, (long)type);
#endif
}

EWXWEXPORT(void*,wxBitmap_CreateDefault)()
{
	return (void*) new wxBitmap();
}

EWXWEXPORT(void,wxBitmap_Delete)(wxBitmap* self)
{
	delete self;
}

EWXWEXPORT(void,wxBitmap_GetSubBitmap)(wxBitmap* self,int x,int y,int w,int h,wxBitmap* bitmap)
{
	*bitmap = self->GetSubBitmap(wxRect(x, y, w, h));
}

EWXWEXPORT(bool,wxBitmap_LoadFile)(wxBitmap* self,wxString* name,int type)
{
#if wxVERSION_NUMBER >= 2400
	return self->LoadFile(*name, (wxBitmapType)type);
#else
	return self->LoadFile(*name, (long)type);
#endif
}

EWXWEXPORT(bool,wxBitmap_SaveFile)(wxBitmap* self,wxString* name,int type,wxPalette* cmap)
{
#if wxVERSION_NUMBER >= 2400
	return self->SaveFile(*name, (wxBitmapType)type,  cmap);
#else
	return self->SaveFile(*name, type,  cmap);
#endif
}

EWXWEXPORT(wxMask*,wxBitmap_GetMask)(wxBitmap* self)
{
	return self->GetMask();
}

EWXWEXPORT(void,wxBitmap_SetMask)(wxBitmap* self,wxMask* mask)
{
	self->SetMask(mask);
}

/**/
EWXWEXPORT(void,wxBitmap_AddHandler)(void* handler)
{
#ifdef __WIN32__
	wxBitmap::AddHandler((wxGDIImageHandler*) handler);
#endif
}

EWXWEXPORT(void,wxBitmap_InsertHandler)(void* handler)
{
#ifdef __WIN32__
	wxBitmap::InsertHandler((wxGDIImageHandler*) handler);
#endif
}

EWXWEXPORT(bool,wxBitmap_RemoveHandler)(wxString* name)
{
#ifdef __WIN32__
	return wxBitmap::RemoveHandler(*name);
#else
	return false;
#endif
}

EWXWEXPORT(void*,wxBitmap_FindHandlerByName)(wxString* name)
{
#ifdef __WIN32__
	return (void*)wxBitmap::FindHandler(*name);
#else
	return NULL;
#endif
}

EWXWEXPORT(void*,wxBitmap_FindHandlerByExtension)(wxString* extension,int type)
{
#ifdef __WIN32__
	return (void*)wxBitmap::FindHandler(*extension, (long)type);
#else
	return NULL;
#endif
}

EWXWEXPORT(void*,wxBitmap_FindHandlerByType)(int type)
{
#ifdef __WIN32__
	return (void*)wxBitmap::FindHandler((long)type);
#else
	return NULL;
#endif
}

EWXWEXPORT(void,wxBitmap_InitStandardHandlers)()
{
#ifdef __WIN32__
	wxBitmap::InitStandardHandlers();
#endif
}

EWXWEXPORT(void,wxBitmap_CleanUpHandlers)()
{
#ifdef __WIN32__
	wxBitmap::CleanUpHandlers();
#endif
}

/**/
EWXWEXPORT(bool,wxBitmap_IsOk)(wxBitmap* self)
{
	return self->IsOk();
}

EWXWEXPORT(int,wxBitmap_GetWidth)(wxBitmap* self)
{
	return self->GetWidth();
}

EWXWEXPORT(int,wxBitmap_GetHeight)(wxBitmap* self)
{
	return self->GetHeight();
}

EWXWEXPORT(int,wxBitmap_GetDepth)(wxBitmap* self)
{
	return self->GetDepth();
}

EWXWEXPORT(void,wxBitmap_SetWidth)(wxBitmap* self,int w)
{
	self->SetWidth(w);
}

EWXWEXPORT(void,wxBitmap_SetHeight)(wxBitmap* self,int h)
{
	self->SetHeight(h);
}

EWXWEXPORT(void,wxBitmap_SetDepth)(wxBitmap* self,int d)
{
	self->SetDepth(d);
}

EWXWEXPORT(void*,wxStaticBitmap_Create)(wxWindow* _prt,int _id,wxBitmap* bitmap, int _lft, int _top, int _wdt, int _hgt, int _stl)
{
	return (void*) new wxStaticBitmap (_prt, _id, *bitmap, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void,wxStaticBitmap_SetIcon)(wxStaticBitmap* self,wxIcon* icon)
{
	self->SetIcon(*icon);
}

EWXWEXPORT(void,wxStaticBitmap_SetBitmap)(wxStaticBitmap* self,wxBitmap* bitmap)
{
	self->SetBitmap(*bitmap);
}

EWXWEXPORT(void,wxStaticBitmap_GetIcon)(wxStaticBitmap* self,wxIcon* _ref)
{
	*_ref = self->GetIcon();
}

EWXWEXPORT(void,wxStaticBitmap_GetBitmap)(wxStaticBitmap* self,wxBitmap* _ref)
{
	*_ref = self->GetBitmap();
}

EWXWEXPORT(void,wxStaticBitmap_Delete)(wxStaticBitmap* self)
{
	delete self;
}

}
