#include "wxc/wrapper.h"
#include "wx/docview.h"
#include "wx/cmdproc.h"
extern "C"
{

typedef int _cdecl (*TGetResp) (void* _obj, int _und);

}

class ELJCommand : public wxCommand
{
	private:
		TGetResp func;
		void*    EiffelObject;
	
	public:
		ELJCommand (bool _und, const wxString& _nme, void* _obj, void* _clb) : wxCommand(_und, _nme)
		{
			func = (TGetResp)_clb;
			EiffelObject = _obj;
		}
		
		bool Do()
		{return func (EiffelObject, 0) != 0;}

		bool Undo()
		{return func (EiffelObject, 1) != 0;}
};

extern "C"
{

EWXWEXPORT(void*,ELJCommand_Create)(bool _und,wxString* _nme,void* _obj,void* _clb)
{
	return (void*)new ELJCommand(_und,*_nme, _obj, _clb);
}

EWXWEXPORT(void,ELJCommand_Delete)(ELJCommand* self)
{
	delete self;
}

EWXWEXPORT(wxString*,ELJCommand_GetName)(void* _obj)
{
	return new wxString(((ELJCommand*)_obj)->GetName());
}

EWXWEXPORT(bool,ELJCommand_CanUndo)(ELJCommand* self)
{
	return self->CanUndo();
}
	

EWXWEXPORT(void*,wxCommandProcessor_wxCommandProcessor)(int maxCommands)
{
	return (void*)new wxCommandProcessor(maxCommands);
}
	
EWXWEXPORT(void,wxCommandProcessor_Delete)(wxCommandProcessor* self)
{
	delete self;
}

EWXWEXPORT(bool,wxCommandProcessor_Submit)(wxCommandProcessor* self,wxCommand* command,bool storeIt)
{
	return self->Submit(command, storeIt);
}
	
EWXWEXPORT(bool,wxCommandProcessor_Undo)(wxCommandProcessor* self)
{
	return self->Undo();
}
	
EWXWEXPORT(bool,wxCommandProcessor_Redo)(wxCommandProcessor* self)
{
	return self->Redo();
}
	
EWXWEXPORT(bool,wxCommandProcessor_CanUndo)(wxCommandProcessor* self)
{
	return self->CanUndo();
}
	
EWXWEXPORT(bool,wxCommandProcessor_CanRedo)(wxCommandProcessor* self)
{
	return self->CanRedo();
}
	
EWXWEXPORT(void,wxCommandProcessor_SetEditMenu)(wxCommandProcessor* self,wxMenu* menu)
{
	self->SetEditMenu(menu);
}
	
EWXWEXPORT(void*,wxCommandProcessor_GetEditMenu)(wxCommandProcessor* self)
{
	return (void*)self->GetEditMenu();
}
	
EWXWEXPORT(void,wxCommandProcessor_SetMenuStrings)(wxCommandProcessor* self)
{
	self->SetMenuStrings();
}
	
EWXWEXPORT(void,wxCommandProcessor_Initialize)(wxCommandProcessor* self)
{
	self->Initialize();
}
	
EWXWEXPORT(int,wxCommandProcessor_GetCommands)(wxCommandProcessor* self,void* _ref)
{
	wxList lst = self->GetCommands();
	if (_ref)
	{
		for (unsigned int i = 0; i < lst.GetCount(); i++)
			((void**)_ref)[i] = (void*)lst.Item(i);
	}
	
	return lst.GetCount();
}
	
EWXWEXPORT(int,wxCommandProcessor_GetMaxCommands)(wxCommandProcessor* self)
{
	return self->GetMaxCommands();
}
	
EWXWEXPORT(void,wxCommandProcessor_ClearCommands)(wxCommandProcessor* self)
{
	self->ClearCommands();
}
	
}
