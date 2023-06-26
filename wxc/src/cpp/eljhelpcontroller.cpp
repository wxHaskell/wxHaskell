#include "wxc/wrapper.h"
#include "wx/fs_zip.h"

extern "C"
{

EWXWEXPORT(void*,wxHtmlHelpController_Create)(int _style)
{
	wxGetApp().InitZipFileSystem();
	wxGetApp().InitImageHandlers();
	return (void*)new wxHtmlHelpController(_style);
}

EWXWEXPORT(void,wxHtmlHelpController_Delete)(void* self)
{
	delete (wxHtmlHelpController*)self;
}

EWXWEXPORT(void,wxHtmlHelpController_SetTitleFormat)(void* self,wxString* format)
{
	((wxHtmlHelpController*)self)->SetTitleFormat(*format);
}
	
EWXWEXPORT(void,wxHtmlHelpController_SetTempDir)(void* self,wxString* path)
{
	((wxHtmlHelpController*)self)->SetTempDir(*path);
}
	
EWXWEXPORT(bool,wxHtmlHelpController_AddBook)(wxHtmlHelpController* self,wxString* book,bool show_wait_msg)
{
	return self->AddBook(*book, show_wait_msg);
}
	
EWXWEXPORT(void,wxHtmlHelpController_Display)(wxHtmlHelpController* self,wxString* x)
{
	self->Display(*x);
}
	
EWXWEXPORT(void,wxHtmlHelpController_DisplayNumber)(wxHtmlHelpController* self,int id)
{
	self->Display(id);
}
	
EWXWEXPORT(void,wxHtmlHelpController_DisplayContents)(wxHtmlHelpController* self)
{
	self->DisplayContents();
}
	
EWXWEXPORT(void,wxHtmlHelpController_DisplayIndex)(wxHtmlHelpController* self)
{
	self->DisplayIndex();
}
	
EWXWEXPORT(bool,wxHtmlHelpController_KeywordSearch)(wxHtmlHelpController* self,wxString* keyword)
{
	return self->KeywordSearch(*keyword);
}
	
EWXWEXPORT(void*,wxHtmlHelpController_GetFrame)(void* self)
{
	return (void*)((wxHtmlHelpController*)self)->GetFrame();
}
	
EWXWEXPORT(void,wxHtmlHelpController_UseConfig)(void* self,wxConfigBase* config,wxString* rootpath)
{
	((wxHtmlHelpController*)self)->UseConfig(config,*rootpath);
}
	
EWXWEXPORT(void,wxHtmlHelpController_ReadCustomization)(void* self,wxConfigBase* cfg,wxString* path)
{
	((wxHtmlHelpController*)self)->ReadCustomization(cfg,*path);
}
	
EWXWEXPORT(void,wxHtmlHelpController_WriteCustomization)(void* self,wxConfigBase* cfg,wxString* path)
{
	((wxHtmlHelpController*)self)->WriteCustomization(cfg,*path);
}
	
EWXWEXPORT(bool,wxHtmlHelpController_Initialize)(wxHtmlHelpController* self,wxString* file)
{
	return self->Initialize(*file);
}
	
EWXWEXPORT(void,wxHtmlHelpController_SetViewer)(void* self,wxString* viewer,int flags)
{
	((wxHtmlHelpController*)self)->SetViewer(*viewer, (long)flags);
}
	
EWXWEXPORT(bool,wxHtmlHelpController_LoadFile)(wxHtmlHelpController* self,wxString* file)
{
	return self->LoadFile(*file);
}
	
EWXWEXPORT(bool,wxHtmlHelpController_DisplaySectionNumber)(wxHtmlHelpController* self,int sectionNo)
{
	return self->DisplaySection(sectionNo);
}
	
EWXWEXPORT(bool,wxHtmlHelpController_DisplaySection)(wxHtmlHelpController* self,wxString* section)
{
	return self->DisplaySection(*section);
}
	
EWXWEXPORT(bool,wxHtmlHelpController_DisplayBlock)(wxHtmlHelpController* self,int blockNo)
{
	return self->DisplayBlock((long)blockNo);
}
	
EWXWEXPORT(void,wxHtmlHelpController_SetFrameParameters)(void* self,wxString* title,int width,int height,int pos_x,int pos_y,bool newFrameEachTime)
{
	((wxHtmlHelpController*)self)->SetFrameParameters(*title, wxSize(width, height), wxPoint(pos_x, pos_y), newFrameEachTime);
}
	
EWXWEXPORT(void*,wxHtmlHelpController_GetFrameParameters)(void* self,void* title,int* width,int* height,int* pos_x,int* pos_y,int* newFrameEachTime)
{
	void* result;
	wxPoint pos;
	wxSize size;

	result = (void*)((wxHtmlHelpController*)self)->GetFrameParameters(&size, &pos, (bool*)newFrameEachTime);
	
	*height = size.y;
	*width  = size.x;
	*pos_x  = pos.x;
	*pos_y  = pos.y;
	
	return result;
}
	
EWXWEXPORT(bool,wxHtmlHelpController_Quit)(wxHtmlHelpController* self)
{
	return self->Quit();
}
	
}
