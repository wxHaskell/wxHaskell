#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxImageList*,wxImageList_Create)(int width,int height,bool mask,int initialCount)
{
	return new wxImageList(width, height, mask, initialCount);
}
	
EWXWEXPORT(void,wxImageList_Delete)(wxImageList* self)
{
	delete self;
}

EWXWEXPORT(int,wxImageList_GetImageCount)(void* self)
{
	return ((wxImageList*)self)->GetImageCount();
}
	
EWXWEXPORT(void,wxImageList_GetSize)(void* self,int index,int* width,int* height)
{
	bool success = ((wxImageList*)self)->GetSize(index,*width,*height);
        if (!success) {
          *width = -1;
          *height = -1;
        };
}
	
EWXWEXPORT(int,wxImageList_AddBitmap)(void* self,wxBitmap* bitmap,wxBitmap* mask)
{
	return ((wxImageList*)self)->Add(*bitmap,*mask);
}
	
EWXWEXPORT(int,wxImageList_AddMasked)(void* self,wxBitmap* bitmap,wxColour* maskColour)
{
	return ((wxImageList*)self)->Add(*bitmap,*maskColour);
}
	
EWXWEXPORT(int,wxImageList_AddIcon)(void* self,wxIcon* icon)
{
	return ((wxImageList*)self)->Add(*icon);
}
	
EWXWEXPORT(bool,wxImageList_Replace)(wxImageList* self,int index,wxBitmap* bitmap,wxBitmap* mask)
{
#ifdef __WIN32__
	return self->Replace(index,*bitmap,*mask);
#else
	return self->Replace(index,*bitmap);
#endif
}
	
EWXWEXPORT(bool,wxImageList_ReplaceIcon)(wxImageList* self,int index,wxIcon* icon)
{
	return self->Replace(index,*icon);
}
	
EWXWEXPORT(bool,wxImageList_Remove)(wxImageList* self,int index)
{
	return self->Remove(index);
}
	
EWXWEXPORT(bool,wxImageList_RemoveAll)(wxImageList* self)
{
	return self->RemoveAll();
}
	
EWXWEXPORT(bool,wxImageList_Draw)(wxImageList* self,int index,wxDC* dc,int x,int y,int flags,bool solidBackground)
{
	return self->Draw(index,*dc, x, y, flags, solidBackground);
}
	
}
