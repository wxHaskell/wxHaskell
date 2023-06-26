#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(int,expNB_TOP)()
{
    return (int)wxNB_TOP;
}

EWXWEXPORT(int,expNB_BOTTOM)()
{
    return (int)wxNB_BOTTOM;
}

EWXWEXPORT(int,expNB_LEFT)()
{
    return (int)wxNB_LEFT;
}

EWXWEXPORT(int,expNB_RIGHT)()
{
    return (int)wxNB_RIGHT;
}

EWXWEXPORT(void*, wxNotebook_Create)(wxWindow* _prt,int _id,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
	return (void*) new wxNotebook (_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl);
}

EWXWEXPORT(int,wxNotebook_GetPageCount)(wxNotebook* self)
{
	return self->GetPageCount();
}
	
EWXWEXPORT(int,wxNotebook_SetSelection)(wxNotebook* self,int nPage)
{
	return self->SetSelection(nPage);
}
	
EWXWEXPORT(void,wxNotebook_AdvanceSelection)(wxNotebook* self,bool bForward)
{
	self->AdvanceSelection(bForward);
}
	
EWXWEXPORT(int,wxNotebook_GetSelection)(wxNotebook* self)
{
	return self->GetSelection();
}
	
EWXWEXPORT(bool,wxNotebook_SetPageText)(wxNotebook* self,int nPage,wxString* strText)
{
	return self->SetPageText(nPage,*strText);
}
	
EWXWEXPORT(wxString*,wxNotebook_GetPageText)(wxNotebook* self,int nPage)
{
	wxString *result = new wxString();
	*result = self->GetPageText(nPage);
	return result;
}
	
EWXWEXPORT(void,wxNotebook_SetImageList)(wxNotebook* self,void* imageList)
{
	self->SetImageList((wxImageList*)imageList);
}
	
EWXWEXPORT(void*,wxNotebook_GetImageList)(void* self)
{
	return (void*)((wxNotebook*)self)->GetImageList();
}
	
EWXWEXPORT(int,wxNotebook_GetPageImage)(wxNotebook* self,int nPage)
{
	return self->GetPageImage(nPage);
}
	
EWXWEXPORT(bool,wxNotebook_SetPageImage)(wxNotebook* self,int nPage,int nImage)
{
	return self->SetPageImage(nPage, nImage);
}
	
EWXWEXPORT(int,wxNotebook_GetRowCount)(wxNotebook* self)
{
	return self->GetRowCount();
}
	
EWXWEXPORT(void,wxNotebook_SetPageSize)(wxNotebook* self,int _w,int _h)
{
	self->SetPageSize(wxSize(_w, _h));
}
	
EWXWEXPORT(void,wxNotebook_SetPadding)(wxNotebook* self,int _w,int _h)
{
	self->SetPadding(wxSize(_w, _h));
}

EWXWEXPORT(int, wxNotebook_HitTest)(wxNotebook* _obj, int x, int y, long *flags)
{
	return _obj->HitTest(wxPoint(x, y), flags);
}

EWXWEXPORT(int,expBK_HITTEST_NOWHERE)()
{
    return (int)wxBK_HITTEST_NOWHERE;
}

EWXWEXPORT(int,expBK_HITTEST_ONICON)()
{
    return (int)wxBK_HITTEST_ONICON;
}

EWXWEXPORT(int,expBK_HITTEST_ONLABEL)()
{
    return (int)wxBK_HITTEST_ONLABEL;
}

EWXWEXPORT(int,expBK_HITTEST_ONITEM)()
{
    return (int)wxBK_HITTEST_ONITEM;
}

EWXWEXPORT(int,expBK_HITTEST_ONPAGE)()
{
    return (int)wxBK_HITTEST_ONPAGE;
}

EWXWEXPORT(bool,wxNotebook_DeletePage)(wxNotebook* self,int nPage)
{
	return self->DeletePage(nPage);
}
	
EWXWEXPORT(bool,wxNotebook_RemovePage)(wxNotebook* self,int nPage)
{
	return self->RemovePage(nPage);
}
	
EWXWEXPORT(bool,wxNotebook_DeleteAllPages)(wxNotebook* self)
{
	return self->DeleteAllPages();
}
	
EWXWEXPORT(bool,wxNotebook_AddPage)(wxNotebook* self,wxNotebookPage* pPage,wxString* strText,bool bSelect,int imageId)
{
	return self->AddPage( pPage,* strText, bSelect, imageId);
}
	
EWXWEXPORT(bool,wxNotebook_InsertPage)(wxNotebook* self,int nPage,wxNotebookPage* pPage,wxString* strText,bool bSelect,int imageId)
{
	return self->InsertPage(nPage,  pPage,* strText, bSelect, imageId);
}
	
EWXWEXPORT(void*,wxNotebook_GetPage)(wxNotebook* self,int nPage)
{
	return (void*)self->GetPage(nPage);
}
	
}
