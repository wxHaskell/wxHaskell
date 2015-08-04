#include "wrapper.h"

extern "C"
{

EWXWEXPORT(wxSplashScreen*,wxSplashScreen_Create)(wxBitmap _bmp,long _sstl,int _ms,wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,long _stl)
{
	return new wxSplashScreen (_bmp,_sstl,_ms,_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}
	
EWXWEXPORT(long,wxSplashScreen_GetSplashStyle)(wxSplashScreen* self)
{
	return self->GetSplashStyle();
}

EWXWEXPORT(int,wxSplashScreen_GetTimeout)(wxSplashScreen* self)
{
	return self->GetTimeout();
}
	
}
