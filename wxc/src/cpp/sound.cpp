#include "wxc/wrapper.h"
#include "wx/sound.h"

/* testing */
// #define wxUSE_SOUND 0

/*-----------------------------------------------------------------------------
  We want to include the function signatures always -- even on 
  systems that don't support wxSound. This means that every function body is
  surrounded by #ifdef wxUSE_SOUND directives :-(
-----------------------------------------------------------------------------*/

#if defined(wxUSE_SOUND) && (wxUSE_SOUND==0)
# undef wxUSE_SOUND
#endif

#ifndef wxUSE_SOUND
# define wxSound      void
#endif

extern "C" {

/*-----------------------------------------------------------------------------
  Sound
-----------------------------------------------------------------------------*/
EWXWEXPORT(wxSound*,wxSound_Create)(wxString* fileName,bool isResource)
{
#ifdef wxUSE_SOUND 
  return new wxSound(*fileName,isResource);
#else
  return NULL;
#endif
}

EWXWEXPORT(void,wxSound_Delete)(wxSound* self)
{
#ifdef wxUSE_SOUND 
  if (self) delete self;
#endif
}

EWXWEXPORT(bool,wxSound_IsOk)(wxSound* self)
{
#ifdef wxUSE_SOUND 
  return self->IsOk();
#else
  return false;
#endif
}

EWXWEXPORT(bool,wxSound_Play)(wxSound* self,unsigned flag)
{
#ifdef wxUSE_SOUND 
  return ((wxSoundBase *)self)->Play(flag);
#else
  return false;
#endif
}

EWXWEXPORT(void,wxSound_Stop)(wxSound* self)
{
#ifdef wxUSE_SOUND
  self->Stop();
#endif
}

}


