#include "wxc/wrapper.h"
#if wxVERSION_NUMBER >= 2400
	#include "wx/plot/plot.h"
#else
	#include "wx/plot.h"
#endif

extern "C"
{

typedef int (*TGetPlotInt)(void* _obj);
typedef double (*TGetPlotDouble)(void* _obj, int _x);

}

class ELJPlotCurve : public wxPlotCurve
{
	private:
		void* EiffelObject;
		TGetPlotInt EGetStartX;
		TGetPlotInt EGetEndX;
		TGetPlotDouble EGetY;
	public:
		ELJPlotCurve(void* _obj, void* _str, void* _end, void* _y, int offsetY, double startY, double endY) : wxPlotCurve(offsetY, startY, endY)
		{
			EiffelObject = _obj;
			EGetStartX = (TGetPlotInt)_str;
			EGetEndX = (TGetPlotInt)_end;
			EGetY = (TGetPlotDouble)_y;
		}
    virtual wxInt32 GetStartX()
	{ return (wxInt32)EGetStartX(EiffelObject); }

    virtual wxInt32 GetEndX()
	{ return (wxInt32)EGetEndX(EiffelObject); }

    virtual double GetY(wxInt32 x)
	{ return EGetY(EiffelObject, (int)x); }
};

extern "C"
{

EWXWEXPORT(void*,wxPlotWindow_Create)(void* parent, int id, int x, int y, int w, int h, int flags)
{
    return (void*)new wxPlotWindow((wxWindow*)parent, (wxWindowID)id, wxPoint(x, y), wxSize(w, h), flags);
}

EWXWEXPORT(void,wxPlotWindow_Add)(void* _obj, void* curve)
{
	((wxPlotWindow*)_obj)->Add((wxPlotCurve*)curve);
}
	
EWXWEXPORT(void,wxPlotWindow_Delete)(void* _obj, void* curve)
{
	((wxPlotWindow*)_obj)->Delete((wxPlotCurve*)curve);
}
	
EWXWEXPORT(int,wxPlotWindow_GetCount)(void* _obj)
{
	return (int)((wxPlotWindow*)_obj)->GetCount();
}
	
EWXWEXPORT(void*,wxPlotWindow_GetAt)(void* _obj, int n)
{
	return (void*)((wxPlotWindow*)_obj)->GetAt((size_t)n);
}
	
EWXWEXPORT(void,wxPlotWindow_SetCurrentCurve)(void* _obj, void*  current)
{
	((wxPlotWindow*)_obj)->SetCurrentCurve((wxPlotCurve*)current);
}
	
EWXWEXPORT(void*,wxPlotWindow_GetCurrentCurve)(void* _obj)
{
	return (void*)((wxPlotWindow*)_obj)->GetCurrentCurve();
}
	
EWXWEXPORT(void,wxPlotWindow_AddOnOff)(void* _obj, void* curve)
{
	((wxPlotWindow*)_obj)->Add((wxPlotOnOffCurve*)curve);
}
	
EWXWEXPORT(void,wxPlotWindow_DeleteOnOff)(void* _obj, void* curve)
{
	((wxPlotWindow*)_obj)->Delete((wxPlotOnOffCurve*)curve);
}
	
EWXWEXPORT(int,wxPlotWindow_GetOnOffCurveCount)(void* _obj)
{
	return (int)((wxPlotWindow*)_obj)->GetOnOffCurveCount();
}
	
EWXWEXPORT(void*,wxPlotWindow_GetOnOffCurveAt)(void* _obj, int n)
{
	return (void*)((wxPlotWindow*)_obj)->GetOnOffCurveAt((size_t)n);
}
	
EWXWEXPORT(void,wxPlotWindow_Move)(void* _obj, void* curve, int pixels_up)
{
	((wxPlotWindow*)_obj)->Move((wxPlotCurve*)curve, pixels_up);
}
	
EWXWEXPORT(void,wxPlotWindow_Enlarge)(void* _obj, void* curve, double factor)
{
	((wxPlotWindow*)_obj)->Enlarge((wxPlotCurve*)curve, factor);
}
	
EWXWEXPORT(void,wxPlotWindow_SetUnitsPerValue)(void* _obj, double upv)
{
	((wxPlotWindow*)_obj)->SetUnitsPerValue(upv);
}
	
EWXWEXPORT(double,wxPlotWindow_GetUnitsPerValue)(void* _obj)
{
	return ((wxPlotWindow*)_obj)->GetUnitsPerValue();
}
	
EWXWEXPORT(void,wxPlotWindow_SetZoom)(void* _obj, double zoom)
{
	((wxPlotWindow*)_obj)->SetZoom(zoom);
}
	
EWXWEXPORT(double,wxPlotWindow_GetZoom)(void* _obj)
{
	return ((wxPlotWindow*)_obj)->GetZoom();
}
	
EWXWEXPORT(void,wxPlotWindow_SetScrollOnThumbRelease)(void* _obj, int scrollOnThumbRelease)
{
	((wxPlotWindow*)_obj)->SetScrollOnThumbRelease(scrollOnThumbRelease != 0);
}
	
EWXWEXPORT(int,wxPlotWindow_GetScrollOnThumbRelease)(void* _obj)
{
	return (int)((wxPlotWindow*)_obj)->GetScrollOnThumbRelease();
}
	
EWXWEXPORT(void,wxPlotWindow_SetEnlargeAroundWindowCentre)(void* _obj, int enlargeAroundWindowCentre)
{
	((wxPlotWindow*)_obj)->SetEnlargeAroundWindowCentre(enlargeAroundWindowCentre != 0);
}
	
EWXWEXPORT(int,wxPlotWindow_GetEnlargeAroundWindowCentre)(void* _obj)
{
	return (int)((wxPlotWindow*)_obj)->GetEnlargeAroundWindowCentre();
}
	
EWXWEXPORT(void,wxPlotWindow_RedrawEverything)(void* _obj)
{
	((wxPlotWindow*)_obj)->RedrawEverything();
}
	
EWXWEXPORT(void,wxPlotWindow_RedrawXAxis)(void* _obj)
{
	((wxPlotWindow*)_obj)->RedrawXAxis();
}
	
EWXWEXPORT(void,wxPlotWindow_RedrawYAxis)(void* _obj)
{
	((wxPlotWindow*)_obj)->RedrawYAxis();
}
	
EWXWEXPORT(void,wxPlotWindow_ResetScrollbar)(void* _obj)
{
	((wxPlotWindow*)_obj)->ResetScrollbar();
}
	

EWXWEXPORT(void*,wxPlotOnOffCurve_Create)(int offsetY)
{
	return (void*)new wxPlotOnOffCurve(offsetY);
}

EWXWEXPORT(void,wxPlotOnOffCurve_Delete)(void* _obj)
{
	delete (wxPlotOnOffCurve*)_obj;
}

EWXWEXPORT(int,wxPlotOnOffCurve_GetStartX)(void* _obj)
{
	return (int)((wxPlotOnOffCurve*)_obj)->GetStartX();
}
	
EWXWEXPORT(int,wxPlotOnOffCurve_GetEndX)(void* _obj)
{
	return (int)((wxPlotOnOffCurve*)_obj)->GetEndX();
}
	
EWXWEXPORT(void,wxPlotOnOffCurve_SetOffsetY)(void* _obj, int offsetY)
{
	((wxPlotOnOffCurve*)_obj)->SetOffsetY(offsetY);
}
	
EWXWEXPORT(int,wxPlotOnOffCurve_GetOffsetY)(void* _obj)
{
	return ((wxPlotOnOffCurve*)_obj)->GetOffsetY();
}
	
EWXWEXPORT(void,wxPlotOnOffCurve_Add)(void* _obj, int on, int off, void* clientData)
{
	((wxPlotOnOffCurve*)_obj)->Add((wxInt32)on, (wxInt32)off, clientData);
}
	
EWXWEXPORT(int,wxPlotOnOffCurve_GetCount)(void* _obj)
{
	return (int)((wxPlotOnOffCurve*)_obj)->GetCount();
}
	
EWXWEXPORT(int,wxPlotOnOffCurve_GetOn)(void* _obj, int index)
{
	return (int)((wxPlotOnOffCurve*)_obj)->GetOn((size_t)index);
}
	
EWXWEXPORT(int,wxPlotOnOffCurve_GetOff)(void* _obj, int index)
{
	return (int)((wxPlotOnOffCurve*)_obj)->GetOff((size_t)index);
}
	
EWXWEXPORT(void*,wxPlotOnOffCurve_GetClientData)(void* _obj, int index)
{
	return (void*)((wxPlotOnOffCurve*)_obj)->GetClientData((size_t)index);
}
	
EWXWEXPORT(void*,wxPlotOnOffCurve_GetAt)(void* _obj, int index)
{
	return (void*)((wxPlotOnOffCurve*)_obj)->GetAt((size_t)index);
}
	
EWXWEXPORT(void,wxPlotOnOffCurve_DrawOnLine)(void* _obj, void* dc, int y, int start, int end, void* clientData)
{
	((wxPlotOnOffCurve*)_obj)->DrawOnLine(*((wxDC*)dc), (wxCoord)y, (wxCoord)start, (wxCoord)end, clientData);
}
	
EWXWEXPORT(void,wxPlotOnOffCurve_DrawOffLine)(void* _obj, void* dc, int y, int start, int end)
{
	((wxPlotOnOffCurve*)_obj)->DrawOffLine(*((wxDC*)dc), (wxCoord)y, (wxCoord)start, (wxCoord)end);
}
	

EWXWEXPORT(void*,ELJPlotCurve_Create)(void* _obj, void* _str, void* _end, void* _y, int offsetY, double startY, double endY)
{
	return (void*)new ELJPlotCurve(_obj, _str, _end, _y, offsetY, startY, endY);
}

EWXWEXPORT(void,ELJPlotCurve_Delete)(void* _obj)
{
	delete (ELJPlotCurve*)_obj;
}

EWXWEXPORT(void,ELJPlotCurve_SetStartY)(void* _obj, double startY)
{
	((ELJPlotCurve*)_obj)->SetStartY(startY);
}
	
EWXWEXPORT(double,ELJPlotCurve_GetStartY)(void* _obj)
{
	return ((ELJPlotCurve*)_obj)->GetStartY();
}
	
EWXWEXPORT(void,ELJPlotCurve_SetEndY)(void* _obj, double endY)
{
	((ELJPlotCurve*)_obj)->SetEndY(endY);
}
	
EWXWEXPORT(double,ELJPlotCurve_GetEndY)(void* _obj)
{
	return ((ELJPlotCurve*)_obj)->GetEndY();
}
	
EWXWEXPORT(void,ELJPlotCurve_SetOffsetY)(void* _obj, int offsetY)
{
	((ELJPlotCurve*)_obj)->SetOffsetY(offsetY);
}
	
EWXWEXPORT(int,ELJPlotCurve_GetOffsetY)(void* _obj)
{
	return ((ELJPlotCurve*)_obj)->GetOffsetY();
}
	
EWXWEXPORT(void,ELJPlotCurve_SetPenNormal)(void* _obj, void* pen)
{
	((ELJPlotCurve*)_obj)->SetPenNormal(*((wxPen*)pen));
}
	
EWXWEXPORT(void,ELJPlotCurve_SetPenSelected)(void* _obj, void* pen)
{
	((ELJPlotCurve*)_obj)->SetPenSelected(*((wxPen*)pen));
}
	

EWXWEXPORT(void*,wxPlotEvent_GetCurve)(void* _obj)
{
	return (void*)((wxPlotEvent*)_obj)->GetCurve();
}
	
EWXWEXPORT(double,wxPlotEvent_GetZoom)(void* _obj)
{
	return ((wxPlotEvent*)_obj)->GetZoom();
}
	
EWXWEXPORT(void,wxPlotEvent_SetZoom)(void* _obj, double zoom)
{
	((wxPlotEvent*)_obj)->SetZoom(zoom);
}
	
EWXWEXPORT(int,wxPlotEvent_GetPosition)(void* _obj)
{
	return (int)((wxPlotEvent*)_obj)->GetPosition();
}
	
EWXWEXPORT(void,wxPlotEvent_SetPosition)(void* _obj, int pos)
{
	((wxPlotEvent*)_obj)->SetPosition((wxInt32)pos);
}
	
}
