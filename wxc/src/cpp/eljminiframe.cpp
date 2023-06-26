#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxMiniFrame_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxMiniFrame (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

}
