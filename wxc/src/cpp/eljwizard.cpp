#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxWizard_Create)(wxWindow* _prt,int _id,wxString* _txt,wxBitmap* _bmp,int _lft,int _top,int _wdt,int _hgt)
{
	wxBitmap bmp = wxNullBitmap;
	if (_bmp) bmp = *_bmp;
#if wxVERSION_NUMBER >= 2400
	return (void*)new wxWizard (_prt, _id,*_txt, bmp, wxPoint(_lft, _top));
#else
	return (void*)wxWizard::Create (_prt, _id,*_txt, bmp, wxPoint(_lft, _top), wxSize(_wdt, _hgt));
#endif
}

EWXWEXPORT(bool,wxWizard_RunWizard)(wxWizard* self,wxWizardPage* firstPage)
{
	return self->RunWizard(firstPage);
}
	
EWXWEXPORT(void*,wxWizard_GetCurrentPage)(wxWizard* self)
{
	return (void*)self->GetCurrentPage();
}
	
EWXWEXPORT(void,wxWizard_Chain)(void* f,void* s)
{
	wxWizardPageSimple::Chain((wxWizardPageSimple*)f, (wxWizardPageSimple*)s);
}
	
EWXWEXPORT(void,wxWizard_SetPageSize)(wxWizard* self,int w,int h)
{
	self->SetPageSize(wxSize(w, h));
}
	
EWXWEXPORT(wxSize*,wxWizard_GetPageSize)(wxWizard* self)
{
	wxSize* sz = new wxSize();
	*sz = self->GetPageSize();
	return sz;
}
	
EWXWEXPORT(void*,wxWizardPageSimple_Create)(wxWizard* _prt)
{
	return (void*)new wxWizardPageSimple(_prt);
}

EWXWEXPORT(void*,wxWizardPageSimple_GetPrev)(void* self)
{
	return (void*)((wxWizardPageSimple*)self)->GetPrev();
}
	
EWXWEXPORT(void*,wxWizardPageSimple_GetNext)(void* self)
{
	return (void*)((wxWizardPageSimple*)self)->GetNext();
}
	
EWXWEXPORT(void,wxWizardPageSimple_GetBitmap)(void* self,wxBitmap* _ref)
{
	*_ref = ((wxWizardPageSimple*)self)->GetBitmap();
}
	
EWXWEXPORT(void,wxWizardPageSimple_SetPrev)(void* self,void* prev)
{
	((wxWizardPageSimple*)self)->SetPrev((wxWizardPage*)prev);
}
	
EWXWEXPORT(void,wxWizardPageSimple_SetNext)(void* self,void* next)
{
	((wxWizardPageSimple*)self)->SetNext((wxWizardPage*)next);
}

EWXWEXPORT(bool,wxWizardEvent_GetDirection)(wxWizardEvent* self)
{
	return self->GetDirection();
}
	
}
