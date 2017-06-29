/*==============================================================================
 * taskbaricon.cpp
 *
 * C wrapper for wxTaskbarIcon
 *
 * (C) 2002-2011 wxEiffel and wxHaskell contributors. See contributors.txt
 * 
 * Licensed under the wxWidgets library license. see LICENSE.
 * 
 *==============================================================================*/

#include "wrapper.h"
#include <wx/taskbar.h>

extern "C" {

////////////////////////////////////////////////////////////////////////////////
// Event Handlers
////////////////////////////////////////////////////////////////////////////////

// #pragma message "wxWidgets taskbar event wrappers generated"
MAKE_EVENT_WRAPPER(EVT_TASKBAR_MOVE)
MAKE_EVENT_WRAPPER(EVT_TASKBAR_LEFT_DOWN)
MAKE_EVENT_WRAPPER(EVT_TASKBAR_LEFT_UP)
MAKE_EVENT_WRAPPER(EVT_TASKBAR_RIGHT_DOWN)
MAKE_EVENT_WRAPPER(EVT_TASKBAR_RIGHT_UP)
MAKE_EVENT_WRAPPER(EVT_TASKBAR_LEFT_DCLICK)
MAKE_EVENT_WRAPPER(EVT_TASKBAR_RIGHT_DCLICK)
#if (wxVERSION_NUMBER >= 2900)
MAKE_EVENT_WRAPPER(EVT_TASKBAR_BALLOON_TIMEOUT)
MAKE_EVENT_WRAPPER(EVT_TASKBAR_BALLOON_CLICK)
#else
MAKE_UNDEFEVENT_WRAPPER(EVT_TASKBAR_BALLOON_TIMEOUT)
MAKE_UNDEFEVENT_WRAPPER(EVT_TASKBAR_BALLOON_CLICK)
#endif

////////////////////////////////////////////////////////////////////////////////
// Wrappers
////////////////////////////////////////////////////////////////////////////////

/*-----------------------------------------------------------------------------
  TaskBarIcon
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxTaskBarIcon*,wxTaskBarIcon_Create)()
{
  return new wxTaskBarIcon();
}

EWXWEXPORT(void,wxTaskBarIcon_Delete)(wxTaskBarIcon* self)
{
  if (self) delete self;
}

EWXWEXPORT(bool,wxTaskBarIcon_IsIconInstalled)(wxTaskBarIcon* self)
{
  return self->IsIconInstalled();
}

EWXWEXPORT(bool,wxTaskBarIcon_IsOk)(wxTaskBarIcon* self)
{
  return self->IsOk();
}

EWXWEXPORT(bool,wxTaskBarIcon_PopupMenu)(wxTaskBarIcon* self,wxMenu* menu)
{
  return self->PopupMenu(menu);
}

EWXWEXPORT(bool,wxTaskBarIcon_RemoveIcon)(wxTaskBarIcon* self)
{
  return self->RemoveIcon();
}

EWXWEXPORT(bool,wxTaskBarIcon_SetIcon)(wxTaskBarIcon* self,wxIcon* icon,wxString* tooltip)
{
#if (wxVERSION_NUMBER < 2900)
  return self->SetIcon(*icon, (tooltip ? *tooltip : *wxEmptyString));
#else
  return tooltip ? self->SetIcon(*icon, *tooltip) : self->SetIcon(*icon);
#endif
}

}


