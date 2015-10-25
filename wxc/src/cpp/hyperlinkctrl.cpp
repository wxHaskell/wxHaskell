#include "wrapper.h"
#include <wx/hyperlink.h>

extern "C"
{

EWXWEXPORT(wxHyperlinkCtrl*,wxHyperlinkCtrl_Create)(wxWindow* parent,int id,const wxString& label,const wxString &url, int x, int y, int w, int h, int style)
{
	return new wxHyperlinkCtrl(parent, id, label, url, wxPoint(x, y), wxSize(w, h), style);
}

EWXWEXPORT(wxColour,wxHyperlinkCtrl_GetHoverColour)(wxHyperlinkCtrl* self)
{
	return self->GetHoverColour();
}

EWXWEXPORT(wxColour,wxHyperlinkCtrl_GetNormalColour)(wxHyperlinkCtrl* self)
{
	return self->GetNormalColour();
}

EWXWEXPORT(wxString,wxHyperlinkCtrl_GetURL)(wxHyperlinkCtrl* self)
{
	return self->GetURL();
}

EWXWEXPORT(bool,wxHyperlinkCtrl_GetVisited)(wxHyperlinkCtrl* self)
{
	return self->GetVisited();
}

EWXWEXPORT(wxColour,wxHyperlinkCtrl_GetVisitedColour)(wxHyperlinkCtrl* self)
{
	return self->GetVisitedColour();
}

EWXWEXPORT(void,wxHyperlinkCtrl_SetHoverColour)(wxHyperlinkCtrl* self,const wxColour &colour)
{
	self->SetHoverColour(colour);
}

EWXWEXPORT(void,wxHyperlinkCtrl_SetNormalColour)(wxHyperlinkCtrl* self,const wxColour &colour)
{
	self->SetNormalColour(colour);
}

EWXWEXPORT(void,wxHyperlinkCtrl_SetURL)(wxHyperlinkCtrl* self,const wxString &url)
{
	self->SetURL(url);
}

EWXWEXPORT(void,wxHyperlinkCtrl_SetVisited)(wxHyperlinkCtrl* self,bool visited)
{
	self->SetVisited(visited);
}

EWXWEXPORT(void,wxHyperlinkCtrl_SetVisitedColour)(wxHyperlinkCtrl* self,const wxColour &colour)
{
	self->SetVisitedColour(colour);
}

} 


