#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxRegionIterator_Create)()
{
	return (void*)new wxRegionIterator();
}

EWXWEXPORT(void*,wxRegionIterator_CreateFromRegion)(void* region)
{
	return (void*)new wxRegionIterator(*((wxRegion*)region));
}

EWXWEXPORT(void,wxRegionIterator_Delete)(wxRegionIterator* self)
{
	delete self;
}

EWXWEXPORT(void,wxRegionIterator_Reset)(wxRegionIterator* self)
{
	self->Reset();
}
	
EWXWEXPORT(void,wxRegionIterator_ResetToRegion)(wxRegionIterator* self,wxRegion* region)
{
	self->Reset(*region);
}
	
EWXWEXPORT(bool,wxRegionIterator_HaveRects)(wxRegionIterator* self)
{
	return self->HaveRects();
}

EWXWEXPORT(void,wxRegionIterator_Next)(wxRegionIterator* self)
{
	(*self)++;
}
	
EWXWEXPORT(int,wxRegionIterator_GetX)(wxRegionIterator* self)
{
	return self->GetX();
}
	
EWXWEXPORT(int,wxRegionIterator_GetY)(wxRegionIterator* self)
{
	return self->GetY();
}
	
EWXWEXPORT(int,wxRegionIterator_GetWidth)(wxRegionIterator* self)
{
	return self->GetWidth();
}
	
EWXWEXPORT(int,wxRegionIterator_GetHeight)(wxRegionIterator* self)
{
	return self->GetHeight();
}
	
}
