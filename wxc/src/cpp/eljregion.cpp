#include "wxc/wrapper.h"

extern "C"
{
	
EWXWEXPORT(void*,wxRegion_CreateDefault)()
{
	return (void*)new wxRegion();
}

EWXWEXPORT(void*,wxRegion_CreateFromRect)(int x,int y,int w,int h)
{
	return (void*)new wxRegion((wxCoord)x, (wxCoord)y, (wxCoord)w, (wxCoord)h);
}

EWXWEXPORT(void,wxRegion_Delete)(wxRegion* self)
{
	delete self;
}

EWXWEXPORT(void,wxRegion_Assign)(wxRegion* self,wxRegion* region)
{
	*self = *region;
}

EWXWEXPORT(void,wxRegion_Clear)(wxRegion* self)
{
	self->Clear();
}
	
EWXWEXPORT(bool,wxRegion_UnionRect)(wxRegion* self,int x,int y,int width,int height)
{
	return self->Union((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(bool,wxRegion_UnionRegion)(wxRegion* self,wxRegion* region)
{
	return self->Union(*region);
}
	
EWXWEXPORT(bool,wxRegion_IntersectRect)(wxRegion* self,int x,int y,int width,int height)
{
	return self->Intersect((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(bool,wxRegion_IntersectRegion)(wxRegion* self,wxRegion* region)
{
	return self->Intersect(*region);
}
	
EWXWEXPORT(bool,wxRegion_SubtractRect)(wxRegion* self,int x,int y,int width,int height)
{
	return self->Subtract((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(bool,wxRegion_SubtractRegion)(wxRegion* self,wxRegion* region)
{
	return self->Subtract(*region);
}
	
EWXWEXPORT(bool,wxRegion_XorRect)(wxRegion* self,int x,int y,int width,int height)
{
	return self->Xor((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
EWXWEXPORT(bool,wxRegion_XorRegion)(wxRegion* self,wxRegion* region)
{
	return self->Xor(*region);
}
	
EWXWEXPORT(void,wxRegion_GetBox)(wxRegion* self,void* x,void* y,void* w,void* h)
{
	self->GetBox(*((wxCoord*)x),*((wxCoord*)y),*((wxCoord*)w),*((wxCoord*)h));
}
	
EWXWEXPORT(bool,wxRegion_IsEmpty)(wxRegion* self)
{
	return self->IsEmpty();
}
	
EWXWEXPORT(bool,wxRegion_ContainsPoint)(wxRegion* self,int x,int y)
{
	return self->Contains((wxCoord)x, (wxCoord)y);
}
	
EWXWEXPORT(bool,wxRegion_ContainsRect)(wxRegion* self,int x,int y,int width,int height)
{
	return self->Contains((wxCoord)x, (wxCoord)y, (wxCoord)width, (wxCoord)height);
}
	
}
