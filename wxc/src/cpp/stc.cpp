/*==============================================================================
 * stc.cpp
 *
 * C wrapper for wxStyledTextCtrl
 *
 * (C) 2002-2011 wxEiffel and wxHaskell contributors. See contributors.txt
 * 
 *==============================================================================*/

#include "wxc/wrapper.h"

#ifdef wxUSE_STC
# include "wx/stc/stc.h"
#endif

extern "C"
{

////////////////////////////////////////////////////////////////////////////////
// Event Handlers
////////////////////////////////////////////////////////////////////////////////

#ifdef wxUSE_STC
MAKE_EVENT_WRAPPER(EVT_STC_CHANGE)
MAKE_EVENT_WRAPPER(EVT_STC_STYLENEEDED)
MAKE_EVENT_WRAPPER(EVT_STC_CHARADDED)
MAKE_EVENT_WRAPPER(EVT_STC_SAVEPOINTREACHED)
MAKE_EVENT_WRAPPER(EVT_STC_SAVEPOINTLEFT)
MAKE_EVENT_WRAPPER(EVT_STC_ROMODIFYATTEMPT)
MAKE_EVENT_WRAPPER(EVT_STC_KEY)
MAKE_EVENT_WRAPPER(EVT_STC_DOUBLECLICK)
MAKE_EVENT_WRAPPER(EVT_STC_UPDATEUI)
MAKE_EVENT_WRAPPER(EVT_STC_MODIFIED)
MAKE_EVENT_WRAPPER(EVT_STC_MACRORECORD)
MAKE_EVENT_WRAPPER(EVT_STC_MARGINCLICK)
MAKE_EVENT_WRAPPER(EVT_STC_NEEDSHOWN)
MAKE_EVENT_WRAPPER(EVT_STC_PAINTED)
MAKE_EVENT_WRAPPER(EVT_STC_USERLISTSELECTION)
MAKE_EVENT_WRAPPER(EVT_STC_URIDROPPED)
MAKE_EVENT_WRAPPER(EVT_STC_DWELLSTART)
MAKE_EVENT_WRAPPER(EVT_STC_DWELLEND)
MAKE_EVENT_WRAPPER(EVT_STC_START_DRAG)
MAKE_EVENT_WRAPPER(EVT_STC_DRAG_OVER)
MAKE_EVENT_WRAPPER(EVT_STC_DO_DROP)
MAKE_EVENT_WRAPPER(EVT_STC_ZOOM)
MAKE_EVENT_WRAPPER(EVT_STC_HOTSPOT_CLICK)
MAKE_EVENT_WRAPPER(EVT_STC_HOTSPOT_DCLICK)
MAKE_EVENT_WRAPPER(EVT_STC_CALLTIP_CLICK)
MAKE_EVENT_WRAPPER(EVT_STC_AUTOCOMP_SELECTION)
MAKE_EVENT_WRAPPER(EVT_STC_INDICATOR_CLICK)
MAKE_EVENT_WRAPPER(EVT_STC_INDICATOR_RELEASE)
MAKE_EVENT_WRAPPER(EVT_STC_AUTOCOMP_CANCELLED)
MAKE_EVENT_WRAPPER(EVT_STC_AUTOCOMP_CHAR_DELETED)
#else
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_CHANGE)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_STYLENEEDED)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_CHARADDED)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_SAVEPOINTREACHED)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_SAVEPOINTLEFT)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_ROMODIFYATTEMPT)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_KEY)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_DOUBLECLICK)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_UPDATEUI)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_MODIFIED)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_MACRORECORD)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_MARGINCLICK)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_NEEDSHOWN)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_PAINTED)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_USERLISTSELECTION)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_URIDROPPED)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_DWELLSTART)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_DWELLEND)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_START_DRAG)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_DRAG_OVER)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_DO_DROP)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_ZOOM)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_HOTSPOT_CLICK)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_HOTSPOT_DCLICK)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_CALLTIP_CLICK)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_AUTOCOMP_SELECTION)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_INDICATOR_CLICK)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_INDICATOR_RELEASE)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_AUTOCOMP_CANCELLED)
MAKE_UNDEFEVENT_WRAPPER(EVT_STC_AUTOCOMP_CHAR_DELETED)
#endif

////////////////////////////////////////////////////////////////////////////////
// Wrappers
////////////////////////////////////////////////////////////////////////////////

#include "stc_gen.cpp"

/* wxStyledTextCtrl */

EWXWEXPORT(void*,wxStyledTextCtrl_Create)(wxWindow* _prt,int _id, wxString* _txt,int _lft,int _top,int _wdt,int _hgt,int _stl)
{
#ifdef wxUSE_STC
  return (void*) new wxStyledTextCtrl(_prt, _id, wxPoint(_lft, _top), wxSize(_wdt, _hgt), _stl, *_txt);
#else
  return NULL;
#endif
}

  /* tricky handwritten functions */

  EWXWEXPORT(void*,wxStyledTextCtrl_IndicatorGetForeground)(void* _obj,int indic)
  {
#ifdef wxUSE_STC
    wxColour c = ((wxStyledTextCtrl*) _obj)->IndicatorGetForeground(indic);
    wxColour* cc = new wxColour(c);
    return cc;
#else
    return NULL;
#endif
  }
  EWXWEXPORT(void*,wxStyledTextCtrl_GetCaretLineBackground)(void* _obj)
  {
#ifdef wxUSE_STC
#if (wxVERSION_NUMBER < 2800)
    wxColour c = ((wxStyledTextCtrl*) _obj)->GetCaretLineBack();
#else
    wxColour c = ((wxStyledTextCtrl*) _obj)->GetCaretLineBackground();
#endif
    wxColour* cc = new wxColour(c);
    return cc;
#else
    return NULL;
#endif
  }
EWXWEXPORT(void,wxStyledTextCtrl_SetCaretLineBackground)(void* _obj,int back_r,int back_g,int back_b)
{
#ifdef wxUSE_STC
#if (wxVERSION_NUMBER < 2800)
   ((wxStyledTextCtrl*) _obj)->SetCaretLineBack(wxColour(back_r,back_g,back_b));
#else
    /* SetCaretLineBack is changed name to SetCaretLineBackground.
       So I avoid to use stc_gen.cpp for backward compatibility. */
   ((wxStyledTextCtrl*) _obj)->SetCaretLineBackground(wxColour(back_r,back_g,back_b));
#endif
#endif
}
  EWXWEXPORT(void*,wxStyledTextCtrl_GetCaretForeground)(void* _obj)
  {
#ifdef wxUSE_STC
    wxColour c = ((wxStyledTextCtrl*) _obj)->GetCaretForeground();
    wxColour* cc = new wxColour(c);
    return cc;
#else
    return NULL;
#endif
  }
  EWXWEXPORT(void*,wxStyledTextCtrl_GetLine)(void* _obj,int line)
  {
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextCtrl*) _obj)->GetLine(line);
    wxString* ss = new wxString(s);
    return ss;
#else
    return NULL;
#endif
  }

  EWXWEXPORT(void*,wxStyledTextCtrl_GetText)(void* _obj)
  {
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextCtrl*) _obj)->GetText();
    wxString* ss = new wxString(s);
    return ss;
#else
    return NULL;
#endif
  }

  EWXWEXPORT(void*,wxStyledTextCtrl_GetSelectedText)(void* _obj)
  {
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextCtrl*) _obj)->GetSelectedText();
    wxString* ss = new wxString(s);
    return ss;
#else
    return NULL;
#endif
  }

  EWXWEXPORT(void*,wxStyledTextCtrl_GetTextRange)(void* _obj,int startPos,int endPos)
  {
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextCtrl*) _obj)->GetTextRange(startPos, endPos);
    wxString* ss = new wxString(s);
    return ss;
#else
    return NULL;
#endif
  }

  EWXWEXPORT(void*,wxStyledTextCtrl_CreateDocument)(void* _obj)
  {
#ifdef wxUSE_STC
    return ((wxStyledTextCtrl*) _obj)->CreateDocument();
#else
    return NULL;
#endif
  }

  EWXWEXPORT(void*,wxStyledTextCtrl_GetEdgeColour)(void* _obj)
  {
#ifdef wxUSE_STC
    wxColour c = ((wxStyledTextCtrl*) _obj)->GetEdgeColour();
    wxColour* cc = new wxColour(c);
    return cc;
#else
    return NULL;
#endif
  }

  EWXWEXPORT(void*,wxStyledTextCtrl_GetDocPointer)(void* _obj)
  {
#ifdef wxUSE_STC
    return ((wxStyledTextCtrl*) _obj)->GetDocPointer();
#else
    return NULL;
#endif
  }

  EWXWEXPORT(void*,wxStyledTextCtrl_PointFromPosition)(void* _obj,int pos)
  {
#ifdef wxUSE_STC
    wxPoint p = ((wxStyledTextCtrl*) _obj)->PointFromPosition(pos);
    wxPoint* pp = new wxPoint(p);
    return pp;
#else
    return NULL;
#endif
  }

/*************************************/
/* wxStyledTextEvent's get functions */
/*************************************/

EWXWEXPORT(int,wxStyledTextEvent_GetPosition)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetPosition();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetKey)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetKey();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetModifiers)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetModifiers();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetModificationType)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetModificationType();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetLength)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetLength();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetLinesAdded)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetLinesAdded();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetLine)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetLine();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetFoldLevelNow)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetFoldLevelNow();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetFoldLevelPrev)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetFoldLevelPrev();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetMargin)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetMargin();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetMessage)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetMessage();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetWParam)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetWParam();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetLParam)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetLParam();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetListType)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetListType();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetX)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetX();
#else
  return 0;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetY)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetY();
#else
  return 0;
#endif
}

EWXWEXPORT(void*,wxStyledTextEvent_GetDragText)(void* _obj)
{
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextEvent*) _obj)->GetDragText();
    wxString* ss = new wxString(s);
    return ss;
#else
  return NULL;
#endif
}

EWXWEXPORT(bool,wxStyledTextEvent_GetDragAllowMove)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetDragAllowMove();
#else
  return false;
#endif
}

EWXWEXPORT(int,wxStyledTextEvent_GetDragResult)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetDragResult();
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxStyledTextEvent_GetShift)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetShift();
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxStyledTextEvent_GetControl)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetControl();
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxStyledTextEvent_GetAlt)(void* _obj)
{
#ifdef wxUSE_STC
  return ((wxStyledTextEvent*) _obj)->GetAlt();
#else
  return false;
#endif
}

EWXWEXPORT(void*,wxStyledTextEvent_GetText)(void* _obj)
{
#ifdef wxUSE_STC
    wxString s = ((wxStyledTextEvent*) _obj)->GetText();
    wxString* ss = new wxString(s);
    return ss;
#else
  return NULL;
#endif
}

EWXWEXPORT(void*,wxStyledTextEvent_Clone)(void* _obj)
{
#ifdef wxUSE_STC
  return (void*) ((wxStyledTextEvent*) _obj)->Clone();
#else
  return NULL;
#endif
}

/*************************************/
/* wxStyledTextEvent's set functions */
/*************************************/

EWXWEXPORT(void,wxStyledTextEvent_SetPosition)(void* _obj,int pos)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetPosition(pos);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetKey)(void* _obj,int k)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetKey(k);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetModifiers)(void* _obj,int m)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetModifiers(m);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetModificationType)(void* _obj,int t)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetModificationType(t);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetText)(void* _obj,void* t)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetText(*(wxString*)t);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetLength)(void* _obj,int len)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetLength(len);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetLinesAdded)(void* _obj,int num)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetLinesAdded(num);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetLine)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetLine(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetFoldLevelNow)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetFoldLevelNow(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetFoldLevelPrev)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetFoldLevelPrev(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetMargin)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetMargin(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetMessage)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetMessage(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetWParam)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetWParam(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetLParam)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetLParam(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetListType)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetListType(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetX)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetX(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetY)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetY(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetDragText)(void* _obj,void* val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetDragText(*(wxString*)val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetDragAllowMove)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetDragAllowMove(val);
#endif
}

EWXWEXPORT(void,wxStyledTextEvent_SetDragResult)(void* _obj,int val)
{
#ifdef wxUSE_STC
  ((wxStyledTextEvent*) _obj)->SetDragResult(*(wxDragResult*)val);
#endif
}

}
