#include "wxc/wrapper.h"
#include "wx/joystick.h"

extern "C"
{

#if wxUSE_JOYSTICK

EWXWEXPORT(wxJoystick*,wxJoystick_Create)(int joystick)
{
	return new wxJoystick(joystick);
}

EWXWEXPORT(void,wxJoystick_Delete)(wxJoystick* self)
{
	delete self;
}

EWXWEXPORT(wxPoint*,wxJoystick_GetPosition)(wxJoystick* self)
{
	wxPoint* pt = new wxPoint();
	*pt = self->GetPosition();
	return pt;
}
	
EWXWEXPORT(int,wxJoystick_GetZPosition)(wxJoystick* self)
{
	return self->GetZPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetButtonState)(wxJoystick* self)
{
	return self->GetButtonState();
}
	
EWXWEXPORT(int,wxJoystick_GetPOVPosition)(wxJoystick* self)
{
	return self->GetPOVPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetPOVCTSPosition)(wxJoystick* self)
{
	return self->GetPOVCTSPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetRudderPosition)(wxJoystick* self)
{
	return self->GetRudderPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetUPosition)(wxJoystick* self)
{
	return self->GetUPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetVPosition)(wxJoystick* self)
{
	return self->GetVPosition();
}
	
EWXWEXPORT(int,wxJoystick_GetMovementThreshold)(wxJoystick* self)
{
	return self->GetMovementThreshold();
}
	
EWXWEXPORT(void,wxJoystick_SetMovementThreshold)(wxJoystick* self,int threshold)
{
	self->SetMovementThreshold(threshold);
}
	
EWXWEXPORT(bool,wxJoystick_IsOk)(wxJoystick* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(int,wxJoystick_GetNumberJoysticks)(wxJoystick* self)
{
	return self->GetNumberJoysticks();
}
	
EWXWEXPORT(int,wxJoystick_GetManufacturerId)(wxJoystick* self)
{
	return self->GetManufacturerId();
}
	
EWXWEXPORT(int,wxJoystick_GetProductId)(wxJoystick* self)
{
	return self->GetProductId();
}
	
EWXWEXPORT(wxString*,wxJoystick_GetProductName)(wxJoystick* _obj)
{
	wxString *result = new wxString();
	*result = _obj->GetProductName();
	return result;
}
	
EWXWEXPORT(int,wxJoystick_GetXMin)(wxJoystick* self)
{
	return self->GetXMin();
}
	
EWXWEXPORT(int,wxJoystick_GetYMin)(wxJoystick* self)
{
	return self->GetYMin();
}
	
EWXWEXPORT(int,wxJoystick_GetZMin)(wxJoystick* self)
{
	return self->GetZMin();
}
	
EWXWEXPORT(int,wxJoystick_GetXMax)(wxJoystick* self)
{
	return self->GetXMax();
}
	
EWXWEXPORT(int,wxJoystick_GetYMax)(wxJoystick* self)
{
	return self->GetYMax();
}
	
EWXWEXPORT(int,wxJoystick_GetZMax)(wxJoystick* self)
{
	return self->GetZMax();
}
	
EWXWEXPORT(int,wxJoystick_GetNumberButtons)(wxJoystick* self)
{
	return self->GetNumberButtons();
}
	
EWXWEXPORT(int,wxJoystick_GetNumberAxes)(wxJoystick* self)
{
	return self->GetNumberAxes();
}
	
EWXWEXPORT(int,wxJoystick_GetMaxButtons)(wxJoystick* self)
{
	return self->GetMaxButtons();
}
	
EWXWEXPORT(int,wxJoystick_GetMaxAxes)(wxJoystick* self)
{
	return self->GetMaxAxes();
}
	
EWXWEXPORT(int,wxJoystick_GetPollingMin)(wxJoystick* self)
{
	return self->GetPollingMin();
}
	
EWXWEXPORT(int,wxJoystick_GetPollingMax)(wxJoystick* self)
{
	return self->GetPollingMax();
}
	
EWXWEXPORT(int,wxJoystick_GetRudderMin)(wxJoystick* self)
{
	return self->GetRudderMin();
}
	
EWXWEXPORT(int,wxJoystick_GetRudderMax)(wxJoystick* self)
{
	return self->GetRudderMax();
}
	
EWXWEXPORT(int,wxJoystick_GetUMin)(wxJoystick* self)
{
	return self->GetUMin();
}
	
EWXWEXPORT(int,wxJoystick_GetUMax)(wxJoystick* self)
{
	return self->GetUMax();
}
	
EWXWEXPORT(int,wxJoystick_GetVMin)(wxJoystick* self)
{
	return self->GetVMin();
}
	
EWXWEXPORT(int,wxJoystick_GetVMax)(wxJoystick* self)
{
	return self->GetVMax();
}
	
EWXWEXPORT(bool,wxJoystick_HasRudder)(wxJoystick* self)
{
	return self->HasRudder();
}
	
EWXWEXPORT(bool,wxJoystick_HasZ)(wxJoystick* self)
{
	return self->HasZ();
}
	
EWXWEXPORT(bool,wxJoystick_HasU)(wxJoystick* self)
{
	return self->HasU();
}
	
EWXWEXPORT(bool,wxJoystick_HasV)(wxJoystick* self)
{
	return self->HasV();
}
	
EWXWEXPORT(bool,wxJoystick_HasPOV)(wxJoystick* self)
{
	return self->HasPOV();
}
	
EWXWEXPORT(bool,wxJoystick_HasPOV4Dir)(wxJoystick* self)
{
	return self->HasPOV4Dir();
}
	
EWXWEXPORT(bool,wxJoystick_HasPOVCTS)(wxJoystick* self)
{
	return self->HasPOVCTS();
}
	
EWXWEXPORT(bool,wxJoystick_SetCapture)(wxJoystick* self,wxWindow* win,int pollingFreq)
{
	return self->SetCapture(win, pollingFreq);
}
	
EWXWEXPORT(bool,wxJoystick_ReleaseCapture)(wxJoystick* self)
{
	return self->ReleaseCapture();
}

#else

EWXWEXPORT(void*,wxJoystick_Create)(int joystick)
{
	return NULL;
}

EWXWEXPORT(void,wxJoystick_Delete)(void* _obj)
{
}

EWXWEXPORT(void,wxJoystick_GetPosition)(void* _obj,void* _x,void* _y)
{
}
	
EWXWEXPORT(int,wxJoystick_GetZPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetButtonState)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetPOVPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetPOVCTSPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetRudderPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetUPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetVPosition)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetMovementThreshold)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(void,wxJoystick_SetMovementThreshold)(void* _obj,int threshold)
{
}
	
EWXWEXPORT(bool,wxJoystick_IsOk)(wxJoystick* _obj)
{
	return _obj->isOk();
}
	
EWXWEXPORT(int,wxJoystick_GetNumberJoysticks)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetManufacturerId)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetProductId)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetProductName)(void* _obj,void* _buf)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetXMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetYMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetZMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetXMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetYMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetZMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetNumberButtons)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetNumberAxes)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetMaxButtons)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetMaxAxes)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetPollingMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetPollingMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetRudderMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetRudderMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetUMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetUMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetVMin)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_GetVMax)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasRudder)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasZ)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasU)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasV)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasPOV)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasPOV4Dir)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_HasPOVCTS)(void* _obj)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_SetCapture)(void* _obj,void* win,int pollingFreq)
{
	return 0;
}
	
EWXWEXPORT(int,wxJoystick_ReleaseCapture)(void* _obj)
{
	return 0;
}

#endif
	
}
