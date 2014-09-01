#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxValidator_Create)()
{
	return (void*)new wxValidator();
}

EWXWEXPORT(void,wxValidator_Delete)(void* self)
{
	delete (wxValidator*)self;
}

EWXWEXPORT(bool,wxValidator_Validate)(wxValidator* self,wxWindow* parent)
{
	return self->Validate(parent);
}
	
EWXWEXPORT(bool,wxValidator_TransferToWindow)(wxValidator* self)
{
	return self->TransferToWindow();
}
	
EWXWEXPORT(bool,wxValidator_TransferFromWindow)(wxValidator* self)
{
	return self->TransferFromWindow();
}
	
EWXWEXPORT(void*,wxValidator_GetWindow)(void* self)
{
	return (void*)((wxValidator*)self)->GetWindow();
}
	
EWXWEXPORT(void,wxValidator_SetWindow)(void* self,wxWindowBase* win)
{
	((wxValidator*)self)->SetWindow(win);
}
	
#if (wxVERSION_NUMBER < 2800)	
EWXWEXPORT(bool,wxValidator_IsSilent)()
{
	return wxValidator::IsSilent();
}
#endif
	
// EWXWEXPORT(void,wxValidator_SetBellOnError)(bool doIt)
EWXWEXPORT(void,wxValidator_SuppressBellOnError)(bool doIt)
{
//	wxValidator::SetBellOnError(doIt);
	wxValidator::SuppressBellOnError(doIt);
}
	
EWXWEXPORT(void*,wxTextValidator_Create)(int style,void* val)
{
	return (void*)new wxTextValidator((long)style, new wxString);
}

EWXWEXPORT(int,wxTextValidator_GetStyle)(wxTextValidator* self)
{
	return (int)self->GetStyle();
}
	
EWXWEXPORT(void,wxTextValidator_SetStyle)(void* self,int style)
{
	((wxTextValidator*)self)->SetStyle((long) style);
}
	
#if (wxVERSION_NUMBER < 2800)	
EWXWEXPORT(void,wxTextValidator_SetIncludeList)(void* self,void* list,int count)
{
#if (wxVERSION_NUMBER <= 2600)
	wxStringList str;
	
	for (int i = 0; i < count; i++)
		str.Add(((wxChar**)list)[i]);
		
	((wxTextValidator*)self)->SetIncludeList(str);
#else
	((wxTextValidator*)self)->SetIncludes((const wxArrayString&)list);
#endif
}
	
EWXWEXPORT(int,wxTextValidator_GetIncludeList)(void* self,void* _ref)
{
#if (wxVERSION_NUMBER <= 2600)
	if (_ref)
	{
		for (unsigned int i = 0; i < ((wxTextValidator*)self)->GetIncludeList().GetCount(); i++)
			((const wxChar**)_ref)[i] = wxStrdup(((wxTextValidator*)self)->GetIncludeList().Item(i)->GetData());
	}
	return ((wxTextValidator*)self)->GetIncludeList().GetCount();
#else
	wxArrayString arr = ((wxTextValidator*)self)->GetIncludes();
	if (_ref)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((const wxChar**)_ref)[i] = wxStrdup (arr.Item(i).c_str());
	}
	return arr.GetCount();
#endif
}
	
EWXWEXPORT(void,wxTextValidator_SetExcludeList)(void* self,void* list,int count)
{
#if (wxVERSION_NUMBER <= 2600)
	wxStringList str;
	
	for (int i = 0; i < count; i++)
		str.Add(((wxChar**)list)[i]);
		
	((wxTextValidator*)self)->SetExcludeList(str);
#else
	((wxTextValidator*)self)->SetExcludes((const wxArrayString&)list);
#endif
}
	
EWXWEXPORT(int,wxTextValidator_GetExcludeList)(void* self,void* _ref)
{
#if (wxVERSION_NUMBER <= 2600)
	if (_ref)
	{
		for (unsigned int i = 0; i < ((wxTextValidator*)self)->GetExcludeList().GetCount(); i++)
			((const wxChar**)_ref)[i] = ((wxTextValidator*)self)->GetExcludeList().Item(i)->GetData();
	}
	return ((wxTextValidator*)self)->GetExcludeList().GetCount();
#else
	wxArrayString arr = ((wxTextValidator*)self)->GetExcludes();
	if (_ref)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((const wxChar**)_ref)[i] = wxStrdup (arr.Item(i).c_str());
	}
	return arr.GetCount();
#endif
}
#else
EWXWEXPORT(void,wxTextValidator_SetIncludes)(void* self,void* list,int count)
{
  wxArrayString str;
  
  for (int i = 0; i < count; i++)
    str.Add(((wxChar**)list)[i]);
  
  ((wxTextValidator*)self)->SetIncludes(str);
}

EWXWEXPORT(void*,wxTextValidator_GetIncludes)(void* self,int* _nitems)
{
  void* retval = NULL;

  if (_nitems != NULL)
  {
    wxArrayString items = ((wxTextValidator*)self)->GetIncludes();
    wxChar **items_copy = (wxChar**) malloc(sizeof(wxChar*) * items.GetCount());

    for (unsigned int i = 0; i < items.GetCount(); i++)
    {
      items_copy[i] = wxStrdup(items.Item(i).wchar_str());
    }
    retval = (void*)items_copy;
    *_nitems = items.GetCount();
  }
  return retval;
}
	
EWXWEXPORT(void,wxTextValidator_SetExcludes)(void* self,void* list,int count)
{
	wxArrayString str;
	
	for (int i = 0; i < count; i++)
		str.Add(((wxChar**)list)[i]);
		
	((wxTextValidator*)self)->SetExcludes(str);
}
	
EWXWEXPORT(void*,wxTextValidator_GetExcludes)(void* self,int* _nitems)
{
  void* retval = NULL;

  if (_nitems != NULL)
  {
    wxArrayString items = ((wxTextValidator*)self)->GetExcludes();
    wxChar **items_copy = (wxChar **)malloc(sizeof(wxChar *)* items.GetCount());

    for (unsigned int i = 0; i < items.GetCount(); i++)
    {
      items_copy[i] = wxStrdup(items.Item(i).wchar_str());
    }
    retval = (void*)items_copy;
    *_nitems = items.GetCount();
  }
  return retval;
}

EWXWEXPORT(void*,wxTextValidator_Clone)(void* self)
{
  return (void*)((wxTextValidator*)self)->Clone();
}

EWXWEXPORT(bool,wxTextValidator_TransferToWindow)(wxTextValidator* self)
{
	return self->TransferToWindow();
}
	
EWXWEXPORT(bool,wxTextValidator_TransferFromWindow)(wxTextValidator* self)
{
	return self->TransferFromWindow();
}
	
#endif

EWXWEXPORT(void,wxTextValidator_OnChar)(void* self,wxKeyEvent* event)
{
	((wxTextValidator*)self)->OnChar(*event);
}

EWXWEXPORT(void*,ELJTextValidator_Create)(void* self,void* _fnc,void* _txt,long _stl)
{
	return new ELJTextValidator(self, _fnc, _txt, _stl);
}

}

bool ELJTextValidator::Validate(wxWindow* _prt)
{
	if (obj && fnc)
		return fnc(obj) != 0;
	else
		return wxTextValidator::Validate(_prt);
}
