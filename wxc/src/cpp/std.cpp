#include "wrapper.h"

/*-----------------------------------------------------------------------------
  License: BSD-style (BSD3)

  We sometimes want to use standard C++ types, containers and algorithms, to
  clean-up our code. Unfortunately, most platorms' FFI just support C language,
  not support C++ and its libraries. So, we define C-functions to use standard
  C++ types, containers and algorithms in this place.
-----------------------------------------------------------------------------*/

extern "C"
{
/*-----------------------------------------------------------------------------
  Boolean types, e.g. C++ bool and C99 _Bool
-----------------------------------------------------------------------------*/
EWXWEXPORT(int,boolToInt)(bool val)
{
	return val ? 1 :  0;
}

EWXWEXPORT(bool,intToBool)(int val)
{
	return val!= 0 ? true : false;
}

}
