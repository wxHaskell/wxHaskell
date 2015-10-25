/*-----------------------------------------------------------------------------
  HyperlinkCtrl
-----------------------------------------------------------------------------*/

TClassDefExtend(wxHyperlinkCtrl,wxControl);


TClass(wxHyperlinkCtrl) wxHyperlinkCtrl_Create( TClass(wxWindow) parent, int id, TClass(wxString) label,TClass(wxString) url, TRect(x,y,w,h), int style);
TClass(wxColour)        wxHyperlinkCtrl_GetHoverColour( TSelf(wxHyperlinkCtrl) self);
TClass(wxColour)        wxHyperlinkCtrl_GetNormalColour( TSelf(wxHyperlinkCtrl) self);
TClass(wxString)        wxHyperlinkCtrl_GetURL( TSelf(wxHyperlinkCtrl) self);
TBool                   wxHyperlinkCtrl_GetVisited( TSelf(wxHyperlinkCtrl) self);
TClass(wxColour)        wxHyperlinkCtrl_GetVisitedColour( TSelf(wxHyperlinkCtrl) self);
void                    wxHyperlinkCtrl_SetHoverColour( TSelf(wxHyperlinkCtrl) self, TClass(wxColour) colour);
void                    wxHyperlinkCtrl_SetNormalColour( TSelf(wxHyperlinkCtrl) self, TClass(wxColour) colour);
void                    wxHyperlinkCtrl_SetURL( TSelf(wxHyperlinkCtrl) self, TClass(wxString) url);
void                    wxHyperlinkCtrl_SetVisited( TSelf(wxHyperlinkCtrl) self, TBool visited);
void                    wxHyperlinkCtrl_SetVisitedColour( TSelf(wxHyperlinkCtrl) self, TClass(wxColour) colour);




