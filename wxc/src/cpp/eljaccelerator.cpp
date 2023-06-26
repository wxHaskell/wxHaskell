#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxAcceleratorEntry_Create)(int flags, int keyCode, int cmd)
{
	return (void*) new wxAcceleratorEntry(flags, keyCode, cmd);
}

EWXWEXPORT(void, wxAcceleratorEntry_Delete)(void* _obj)
{
	delete (wxAcceleratorEntry*)_obj;
}

EWXWEXPORT(void, wxAcceleratorEntry_Set)(void* _obj, int flags, int keyCode, int cmd)
{
	((wxAcceleratorEntry*)_obj)->Set(flags, keyCode, cmd);
}
	
EWXWEXPORT(int, wxAcceleratorEntry_GetFlags)(void* _obj)
{
	return ((wxAcceleratorEntry*)_obj)->GetFlags();
}
	
EWXWEXPORT(int, wxAcceleratorEntry_GetKeyCode)(void* _obj)
{
	return ((wxAcceleratorEntry*)_obj)->GetKeyCode();
}
	
EWXWEXPORT(int, wxAcceleratorEntry_GetCommand)(void* _obj)
{
	return ((wxAcceleratorEntry*)_obj)->GetCommand();
}
	
EWXWEXPORT(void*, wxAcceleratorTable_Create)(int n, void* entries)
{
	wxAcceleratorEntry* list = new wxAcceleratorEntry[n];
	
	for (int i = 0; i< n; i++)
		list[i] = *(((wxAcceleratorEntry**)entries)[i]);
	
	wxAcceleratorTable* result = new wxAcceleratorTable(n, list);
	
	delete [] list;
	
	return (void*) result;
}

EWXWEXPORT(void, wxAcceleratorTable_Delete)(void* _obj)
{
	delete (wxAcceleratorEntry*)_obj;
}

}
