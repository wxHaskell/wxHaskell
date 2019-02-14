#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxStaticText_Create)(wxWindow* _prt,int _id,wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxStaticText (_prt, _id, *_txt, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(void,wxStaticText_Wrap)(void* _obj, int _width)
{
        return ((wxStaticText*)_obj)->Wrap(_width);
}

}
