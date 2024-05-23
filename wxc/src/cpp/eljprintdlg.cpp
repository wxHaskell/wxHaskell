#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxPrintDialog_Create)(wxWindow* parent,wxPrintDialogData* data)
{
	return (void*)new wxPrintDialog(parent, data);
}

EWXWEXPORT(void,wxPrintDialog_GetPrintData)(wxPrintDialog* self,wxPrintData* _ref)
{
	*_ref = self->GetPrintData();
}


EWXWEXPORT(void*,wxPrintDialog_GetPrintDC)(wxPrintDialog* self)
{
	return (void*)self->GetPrintDC();
}

EWXWEXPORT(void*,wxPageSetupDialog_Create)(wxWindow* parent,wxPageSetupData* data)
{
	return (void*)new wxPageSetupDialog(parent, data);
}

EWXWEXPORT(void,wxPageSetupDialog_GetPageSetupData)(wxPageSetupDialog* self,wxPageSetupData* _ref)
{
	*_ref = self->GetPageSetupData();
}

EWXWEXPORT(int,wxPageSetupDialog_ShowModal)(wxPageSetupDialog* self)
{
        return (int)self->ShowModal();
}

EWXWEXPORT(void*,wxPageSetupDialogData_Create)()
{
	return (void*)new wxPageSetupDialogData();
}

EWXWEXPORT(void*,wxPageSetupDialogData_CreateFromData)(wxPrintData* printData)
{
	return (void*)new wxPageSetupDialogData(*printData);
}

EWXWEXPORT(void,wxPageSetupDialogData_Delete)(wxPageSetupDialogData* self)
{
	delete self;
}

EWXWEXPORT(wxSize*,wxPageSetupDialogData_GetPaperSize)(wxPageSetupDialogData* self)
{
	wxSize* s = new wxSize();
	*s = self->GetPaperSize();
	return s;
}

EWXWEXPORT(int,wxPageSetupDialogData_GetPaperId)(wxPageSetupDialogData* self)
{
	return (int)self->GetPaperId();
}

EWXWEXPORT(wxPoint*,wxPageSetupDialogData_GetMinMarginTopLeft)(wxPageSetupDialogData* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetMinMarginTopLeft();
	return pt;
}

EWXWEXPORT(wxPoint*,wxPageSetupDialogData_GetMinMarginBottomRight)(wxPageSetupDialogData* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetMinMarginBottomRight();
	return pt;
}

EWXWEXPORT(wxPoint*,wxPageSetupDialogData_GetMarginTopLeft)(wxPageSetupDialogData* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetMarginTopLeft();
	return pt;
}

EWXWEXPORT(wxPoint*,wxPageSetupDialogData_GetMarginBottomRight)(wxPageSetupDialogData* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetMarginBottomRight();
	return pt;
}

EWXWEXPORT(int,wxPageSetupDialogData_GetDefaultMinMargins)(wxPageSetupDialogData* self)
{
	return (int)self->GetDefaultMinMargins();
}

EWXWEXPORT(int,wxPageSetupDialogData_GetEnableMargins)(wxPageSetupDialogData* self)
{
	return (int)self->GetEnableMargins();
}

EWXWEXPORT(int,wxPageSetupDialogData_GetEnableOrientation)(wxPageSetupDialogData* self)
{
	return (int)self->GetEnableOrientation();
}

EWXWEXPORT(int,wxPageSetupDialogData_GetEnablePaper)(wxPageSetupDialogData* self)
{
	return (int)self->GetEnablePaper();
}

EWXWEXPORT(int,wxPageSetupDialogData_GetEnablePrinter)(wxPageSetupDialogData* self)
{
	return (int)self->GetEnablePrinter();
}

EWXWEXPORT(int,wxPageSetupDialogData_GetDefaultInfo)(wxPageSetupDialogData* self)
{
	return (int)self->GetDefaultInfo();
}

EWXWEXPORT(int,wxPageSetupDialogData_GetEnableHelp)(wxPageSetupDialogData* self)
{
	return (int)self->GetEnableHelp();
}

EWXWEXPORT(void,wxPageSetupDialogData_SetPaperSize)(wxPageSetupDialogData* self,int w,int h)
{
	self->SetPaperSize(wxSize(w, h));
}

EWXWEXPORT(void,wxPageSetupDialogData_SetPaperId)(wxPageSetupDialogData* self,void* id)
{
	self->SetPaperId(*((wxPaperSize*)id));
}

EWXWEXPORT(void,wxPageSetupDialogData_SetPaperSizeId)(wxPageSetupDialogData* self,int id)
{
	self->SetPaperSize((wxPaperSize)id);
}

EWXWEXPORT(void,wxPageSetupDialogData_SetMinMarginTopLeft)(wxPageSetupDialogData* self,int x,int y)
{
	self->SetMinMarginTopLeft(wxPoint(x, y));
}

EWXWEXPORT(void,wxPageSetupDialogData_SetMinMarginBottomRight)(wxPageSetupDialogData* self,int x,int y)
{
	self->SetMinMarginBottomRight(wxPoint(x, y));
}

EWXWEXPORT(void,wxPageSetupDialogData_SetMarginTopLeft)(wxPageSetupDialogData* self,int x,int y)
{
	self->SetMarginTopLeft(wxPoint(x, y));
}

EWXWEXPORT(void,wxPageSetupDialogData_SetMarginBottomRight)(wxPageSetupDialogData* self,int x,int y)
{
	self->SetMarginBottomRight(wxPoint(x, y));
}

EWXWEXPORT(void,wxPageSetupDialogData_SetDefaultMinMargins)(wxPageSetupDialogData* self,bool flag)
{
	self->SetDefaultMinMargins(flag);
}

EWXWEXPORT(void,wxPageSetupDialogData_SetDefaultInfo)(wxPageSetupDialogData* self,bool flag)
{
	self->SetDefaultInfo(flag);
}

EWXWEXPORT(void,wxPageSetupDialogData_EnableMargins)(wxPageSetupDialogData* self,bool flag)
{
	self->EnableMargins(flag);
}

EWXWEXPORT(void,wxPageSetupDialogData_EnableOrientation)(wxPageSetupDialogData* self,bool flag)
{
	self->EnableOrientation(flag);
}

EWXWEXPORT(void,wxPageSetupDialogData_EnablePaper)(wxPageSetupDialogData* self,bool flag)
{
	self->EnablePaper(flag);
}

EWXWEXPORT(void,wxPageSetupDialogData_EnablePrinter)(wxPageSetupDialogData* self,bool flag)
{
	self->EnablePrinter(flag);
}

EWXWEXPORT(void,wxPageSetupDialogData_EnableHelp)(wxPageSetupDialogData* self,bool flag)
{
	self->EnableHelp(flag);
}

EWXWEXPORT(void,wxPageSetupDialogData_CalculateIdFromPaperSize)(wxPageSetupDialogData* self)
{
	self->CalculateIdFromPaperSize();
}

EWXWEXPORT(void,wxPageSetupDialogData_CalculatePaperSizeFromId)(wxPageSetupDialogData* self)
{
	self->CalculatePaperSizeFromId();
}

EWXWEXPORT(void,wxPageSetupDialogData_Assign)(wxPageSetupDialogData* self,wxPageSetupDialogData* data)
{
	*self = *data;
}

EWXWEXPORT(void,wxPageSetupDialogData_AssignData)(wxPageSetupDialogData* self,wxPrintData* data)
{
	*self = *data;
}

EWXWEXPORT(void,wxPageSetupDialogData_GetPrintData)(wxPageSetupDialogData* self,wxPrintData* _ref)
{
	*_ref = self->GetPrintData();
}

EWXWEXPORT(void,wxPageSetupDialogData_SetPrintData)(wxPageSetupDialogData* self,wxPrintData* printData)
{
	self->SetPrintData(*printData);
}

}
