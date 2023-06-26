
#include "stc_gen.h" 

/* wxStyledTextCtrl */
TClassDefExtend(wxStyledTextCtrl,wxControl)
TClass(wxStyledTextCtrl) wxStyledTextCtrl_Create( TClass(wxWindow) _prt, int _id, TClass(wxString) _txt, TRect(_lft,_top,_wdt,_hgt), int style );


/* tricky handwritten functions */
TClassDef(wxSTCDoc)
TClassDef(wxMemoryBuffer)
TClass(wxColour) wxStyledTextCtrl_IndicatorGetForeground( TSelf(wxStyledTextCtrl) _obj, int indic);
TClass(wxColour) wxStyledTextCtrl_GetCaretLineBackground( TSelf(wxStyledTextCtrl) _obj );
/* SetCaretLineBack is changed name to SetCaretLineBackground.
   So I avoid to use stc_gen.h for backward compatibility. */
void wxStyledTextCtrl_SetCaretLineBackground(TSelf(wxStyledTextCtrl) _obj, TColorRGB(back_r,back_g,back_b));
TClass(wxColour) wxStyledTextCtrl_GetCaretForeground( TSelf(wxStyledTextCtrl) _obj );
TClass(wxString) wxStyledTextCtrl_GetLine( TSelf(wxStyledTextCtrl) _obj, int line);
TClass(wxString) wxStyledTextCtrl_GetText( TSelf(wxStyledTextCtrl) _obj );
TClass(wxString) wxStyledTextCtrl_GetTextRange( TSelf(wxStyledTextCtrl) _obj, int startPos, int endPos);
TClass(wxString) wxStyledTextCtrl_GetSelectedText( TSelf(wxStyledTextCtrl) _obj );
TClass(wxSTCDoc) wxStyledTextCtrl_CreateDocument( TSelf(wxStyledTextCtrl) _obj );
TClass(wxColour) wxStyledTextCtrl_GetEdgeColour( TSelf(wxStyledTextCtrl) _obj );
TClass(wxSTCDoc) wxStyledTextCtrl_GetDocPointer( TSelf(wxStyledTextCtrl) _obj );
TClass(wxPoint) wxStyledTextCtrl_PointFromPosition( TSelf(wxStyledTextCtrl) _obj );


/* wxStyledTextEvent */
TClassDefExtend(wxStyledTextEvent, wxCommandEvent);

/* The wxStyledTextEvent's get-functions */
int wxStyledTextEvent_GetPosition( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetKey( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetModifiers( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetModificationType( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetLength( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetLinesAdded( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetLine( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetFoldLevelNow( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetFoldLevelPrev( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetMargin( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetMessage( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetWParam( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetLParam( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetListType( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetX( TSelf(wxStyledTextEvent) _obj);
int wxStyledTextEvent_GetY( TSelf(wxStyledTextEvent) _obj);
TClass(wxString) wxStyledTextEvent_GetDragText( TSelf(wxStyledTextEvent) _obj );
TBool wxStyledTextEvent_GetDragAllowMove( TSelf(wxStyledTextEvent) _obj );
int wxStyledTextEvent_GetDragResult( TSelf(wxStyledTextEvent) _obj );
TBool wxStyledTextEvent_GetShift( TSelf(wxStyledTextEvent) _obj );
TBool wxStyledTextEvent_GetControl( TSelf(wxStyledTextEvent) _obj );
TBool wxStyledTextEvent_GetAlt( TSelf(wxStyledTextEvent) _obj );

TClass(wxString) wxStyledTextEvent_GetText( TSelf(wxStyledTextEvent) _obj );
TClass(wxStyledTextEvent) wxStyledTextEvent_Clone( TSelf(wxStyledTextEvent) _obj );


/* The wxStyledTextEvent's set-functions */
void wxStyledTextEvent_SetPosition( TSelf(wxStyledTextEvent) _obj, int pos);
void wxStyledTextEvent_SetKey( TSelf(wxStyledTextEvent) _obj, int k);
void wxStyledTextEvent_SetModifiers( TSelf(wxStyledTextEvent) _obj, int m);
void wxStyledTextEvent_SetModificationType( TSelf(wxStyledTextEvent) _obj, int t);
void wxStyledTextEvent_SetText( TSelf(wxStyledTextEvent) _obj, TClass(wxString) t);
void wxStyledTextEvent_SetLength( TSelf(wxStyledTextEvent) _obj, int len);
void wxStyledTextEvent_SetLinesAdded( TSelf(wxStyledTextEvent) _obj, int num);
void wxStyledTextEvent_SetLine( TSelf(wxStyledTextEvent) _obj, int val);
void wxStyledTextEvent_SetFoldLevelNow( TSelf(wxStyledTextEvent) _obj, int val);
void wxStyledTextEvent_SetFoldLevelPrev( TSelf(wxStyledTextEvent) _obj, int val);
void wxStyledTextEvent_SetMargin( TSelf(wxStyledTextEvent) _obj, int val);
void wxStyledTextEvent_SetMessage( TSelf(wxStyledTextEvent) _obj, int val);
void wxStyledTextEvent_SetWParam( TSelf(wxStyledTextEvent) _obj, int val);
void wxStyledTextEvent_SetLParam( TSelf(wxStyledTextEvent) _obj, int val);
void wxStyledTextEvent_SetListType( TSelf(wxStyledTextEvent) _obj, int val);
void wxStyledTextEvent_SetX( TSelf(wxStyledTextEvent) _obj, int val);
void wxStyledTextEvent_SetY( TSelf(wxStyledTextEvent) _obj, int val);
void wxStyledTextEvent_SetDragText( TSelf(wxStyledTextEvent) _obj, TClass(wxString) val);
void wxStyledTextEvent_SetDragAllowMove( TSelf(wxStyledTextEvent) _obj, TBool val);
/* void wxStyledTextEvent_SetDragResult( TSelf(wxStyledTextEvent) _obj, TClass(wxDragResult) val); */
void wxStyledTextEvent_SetDragResult( TSelf(wxStyledTextEvent) _obj, int val);

/* The wxStyledTextEvent's events */
int expEVT_STC_CHANGE();
int expEVT_STC_STYLENEEDED();
int expEVT_STC_CHARADDED();
int expEVT_STC_SAVEPOINTREACHED();
int expEVT_STC_SAVEPOINTLEFT();
int expEVT_STC_ROMODIFYATTEMPT();
int expEVT_STC_KEY();
int expEVT_STC_DOUBLECLICK();
int expEVT_STC_UPDATEUI();
int expEVT_STC_MODIFIED();
int expEVT_STC_MACRORECORD();
int expEVT_STC_MARGINCLICK();
int expEVT_STC_NEEDSHOWN();
/* expEVT_STC_POSCHANGED is removed in wxWidgets-2.6.x. */
/* int expEVT_STC_POSCHANGED(); */
int expEVT_STC_PAINTED();
int expEVT_STC_USERLISTSELECTION();
int expEVT_STC_URIDROPPED();
int expEVT_STC_DWELLSTART();
int expEVT_STC_DWELLEND();
int expEVT_STC_START_DRAG();
int expEVT_STC_DRAG_OVER();
int expEVT_STC_DO_DROP();
int expEVT_STC_ZOOM();
int expEVT_STC_HOTSPOT_CLICK();
int expEVT_STC_HOTSPOT_DCLICK();
int expEVT_STC_CALLTIP_CLICK();
int expEVT_STC_AUTOCOMP_SELECTION(); 

/* Styled Text Control as an XML Resource */
TClassDefExtend(wxXmlResource,wxObject)
TClass(wxStyledTextCtrl) wxXmlResource_GetStyledTextCtrl( TSelf(wxWindow) _obj, TClass(wxString) str_id );
