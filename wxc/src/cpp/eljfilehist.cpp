#include "wxc/wrapper.h"
#include "wx/docview.h"

extern "C"
{

EWXWEXPORT(void*,wxFileHistory_Create)(int maxFiles)
{
	return (void*)new wxFileHistory(maxFiles);
}
	
EWXWEXPORT(void,wxFileHistory_Delete)(void* _obj)
{
	delete (wxFileHistory*)_obj;
}
	
EWXWEXPORT(void,wxFileHistory_AddFileToHistory)(void* _obj,wxString* file)
{
	((wxFileHistory*)_obj)->AddFileToHistory(*file);
}
	
EWXWEXPORT(void,wxFileHistory_RemoveFileFromHistory)(void* _obj,int i)
{
	((wxFileHistory*)_obj)->RemoveFileFromHistory(i);
}
	
EWXWEXPORT(int,wxFileHistory_GetMaxFiles)(void* _obj)
{
	return ((wxFileHistory*)_obj)->GetMaxFiles();
}
	
EWXWEXPORT(void,wxFileHistory_UseMenu)(void* _obj,void* menu)
{
	((wxFileHistory*)_obj)->UseMenu((wxMenu*)menu);
}
	
EWXWEXPORT(void,wxFileHistory_RemoveMenu)(void* _obj,void* menu)
{
	((wxFileHistory*)_obj)->RemoveMenu((wxMenu*)menu);
}
	
EWXWEXPORT(void,wxFileHistory_Load)(void* _obj,void* config)
{
	((wxFileHistory*)_obj)->Load(*((wxConfigBase*)config));
}
	
EWXWEXPORT(void,wxFileHistory_Save)(void* _obj,void* config)
{
	((wxFileHistory*)_obj)->Save(*((wxConfigBase*)config));
}
	
EWXWEXPORT(void,wxFileHistory_AddFilesToMenu)(void* _obj,void* menu)
{
	if (menu)
		((wxFileHistory*)_obj)->AddFilesToMenu((wxMenu*)menu);
	else
		((wxFileHistory*)_obj)->AddFilesToMenu();
}
	
EWXWEXPORT(wxString*,wxFileHistory_GetHistoryFile)(void* _obj,int i)
{
  wxString *result = new wxString();
  *result = ((wxFileHistory*)_obj)->GetHistoryFile(i);
  return result;
}
	
EWXWEXPORT(int,wxFileHistory_GetCount)(void* _obj)
{
#if (wxVERSION_NUMBER <= 2600)
	return ((wxFileHistory*)_obj)->GetNoHistoryFiles();
#else
	return ((wxFileHistory*)_obj)->GetCount();
#endif
}
	
EWXWEXPORT(int,wxFileHistory_GetMenus)(void* _obj, void* _ref)
{
	wxList lst = ((wxFileHistory*)_obj)->GetMenus();
	if (_ref)
	{
		int i = 0;
		wxList::compatibility_iterator node = lst.GetFirst();
		while (node)
		{
			((void**)_ref)[i] = node->GetData();
			node = node->GetNext();
			++i;
		}
	}
	
	return lst.GetCount();
}
	
}
