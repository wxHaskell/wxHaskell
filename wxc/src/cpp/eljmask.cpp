#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*, wxMask_Create) (void* bitmap)
{
	return (void*) new wxMask (*(wxBitmap*)bitmap);
}

EWXWEXPORT(void*, wxMask_CreateColoured) (void* bitmap, void* colour)
{
	return (void*) new wxMask (*(wxBitmap*)bitmap, *(wxColour*)colour);
}

}
