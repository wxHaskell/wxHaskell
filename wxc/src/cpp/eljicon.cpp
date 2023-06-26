#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxIcon_CreateDefault)()
{
	return (void*)new wxIcon();
}

EWXWEXPORT(void,wxIcon_Delete)(wxIcon* self)
{
	delete self;
}

EWXWEXPORT(void*,wxIcon_FromRaw)(void* data,int width,int height)
{
#ifdef __WIN32__
	return (void*)new wxIcon((const wxChar*)data, wxBITMAP_TYPE_ICO, width, height);
#else
	return (void*)new wxIcon((const wxChar*)data, wxBITMAP_TYPE_ANY, width, height);
#endif
}

EWXWEXPORT(void*,wxIcon_FromXPM)(void* data)
{
	return (void*)new wxIcon((const wxChar*)data);
}

EWXWEXPORT(void*,wxIcon_CreateLoad)(wxString* name,long type,int width,int height)
{
	return (void*)new wxIcon(*name, (wxBitmapType)type, width, height);
}

EWXWEXPORT(bool,wxIcon_Load)(wxIcon* self,wxString* name,long type,int width,int height)
{
#ifdef __WIN32__
	return self->LoadFile(*name, (wxBitmapType)type, width, height);
#else
	return self->LoadFile(*name, (wxBitmapType)type);
#endif
}

EWXWEXPORT(void,wxIcon_CopyFromBitmap)(wxIcon* self,wxBitmap* bmp)
{
#ifdef __WIN32__
	self->CopyFromBitmap(*bmp);
#endif
}

EWXWEXPORT(bool,wxIcon_IsOk)(wxIcon* self)
{
	return self->IsOk();
}

EWXWEXPORT(int,wxIcon_GetDepth)(wxIcon* self)
{
	return self->GetDepth();
}

EWXWEXPORT(int,wxIcon_GetWidth)(wxIcon* self)
{
	return self->GetWidth();
}

EWXWEXPORT(int,wxIcon_GetHeight)(wxIcon* self)
{
	return self->GetHeight();
}

#if (wxVERSION_NUMBER >= 2800)
EWXWEXPORT(void,wxIcon_SetDepth)(wxIcon* self,int depth)
{
	self->SetDepth(depth);
}

EWXWEXPORT(void,wxIcon_SetWidth)(wxIcon* self,int width)
{
	self->SetWidth(width);
}

EWXWEXPORT(void,wxIcon_SetHeight)(wxIcon* self,int height)
{
	self->SetHeight(height);
}
#endif

EWXWEXPORT(void,wxIcon_Assign)(wxIcon* self,void* other)
{
	*self = *((wxIcon*)other);
}

EWXWEXPORT(bool,wxIcon_IsEqual)(wxIcon* self,wxIcon* other)
{
#if (wxVERSION_NUMBER <= 2800)
	return *self == *other;
#else
	wxIcon* icon1 = self;
	wxIcon* icon2 = other;
	wxBitmap bmp1;
	wxBitmap bmp2;
	bmp1.CopyFromIcon(*icon1);
	bmp2.CopyFromIcon(*icon2);
	wxImage image1 = (wxImage)bmp1.ConvertToImage();
	wxImage image2 = (wxImage)bmp2.ConvertToImage();
	wxImage* img1 = &image1;
	wxImage* img2 = &image2;
	if( (icon1->GetWidth() == icon2->GetWidth()) &&
		(icon1->GetHeight() == icon2->GetHeight()) &&
		(icon1->GetDepth() == icon2->GetDepth())){
		bool equal = true;
		for(int sx=0;sx<(icon1->GetWidth());sx++){
			for(int sy=0;sy<(icon1->GetHeight());sy++){
				equal = equal &&
					(img1->GetRed(sx,sy)==img2->GetRed(sx,sy) &&
					img1->GetGreen(sx,sy)==img2->GetGreen(sx,sy) &&
					img1->GetBlue(sx,sy)==img2->GetBlue(sx,sy) &&
					img1->GetAlpha(sx,sy)==img2->GetAlpha(sx,sy));
			}
		}
		return equal;
	} else {
		return false;
	}
#endif
}

}
