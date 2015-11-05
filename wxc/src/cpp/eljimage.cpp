#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void,wxImage_Copy)(wxImage *self, wxImage* image)
{
	*image = self->Copy();
}

EWXWEXPORT(wxImage*,wxImage_CreateDefault)()
{
	return new wxImage();
}

EWXWEXPORT(wxImage*,wxImage_CreateSized)(int width,int height)
{
	return new wxImage(width, height);
}

EWXWEXPORT(wxImage*, wxImage_CreateFromByteString)(const wxSize &sz, unsigned char *data)
{
	return new wxImage(sz, data);
}

EWXWEXPORT(wxImage*, wxImage_CreateFromLazyByteString)(const wxSize &sz, unsigned char *data)
{
	return new wxImage(sz, data);
}

EWXWEXPORT(size_t, wxImage_ConvertToByteString)(wxImage *self, wxBitmapType type, char *data)
{
	wxMemoryOutputStream out;
	self->SaveFile(out, type);
	size_t len = out.GetLength();
	if( !data ) return len;
        return out.CopyTo(data, len);
}

EWXWEXPORT(size_t, wxImage_ConvertToLazyByteString)(wxImage *self, wxBitmapType type, char *data)
{
	wxMemoryOutputStream out;
	self->SaveFile(out, type);
	size_t len = out.GetLength();
	if( !data ) return len;
        return out.CopyTo(data, len);
}

EWXWEXPORT(wxImage*,wxImage_CreateFromData)(int width,int height,void* data)
{
	return new wxImage(width, height, (unsigned char*)data, true);
}

EWXWEXPORT(wxImage*,wxImage_CreateFromFile)(wxString* name)
{
	return new wxImage(*name);
}

EWXWEXPORT(wxImage*,wxImage_CreateFromBitmap)(wxBitmap* bitmap)
{
	return new wxImage(bitmap->ConvertToImage());
}

EWXWEXPORT(void,wxImage_ConvertToBitmap)(wxImage* self,wxBitmap* bitmap)
{
	wxBitmap tmp(*self);
	*bitmap = tmp;
}
	
EWXWEXPORT(void,wxImage_Initialize)(wxImage* self,int width,int height)
{
	self->Create(width, height);
}
	
EWXWEXPORT(void,wxImage_InitializeFromData)(wxImage* self,int width,int height,void* data)
{
	self->Create(width, height, (unsigned char*)data, true);
}
	
EWXWEXPORT(void,wxImage_Destroy)(wxImage* self)
{
	self->Destroy();
}

EWXWEXPORT(void,wxImage_GetSubImage)(wxImage* self,int x,int y,int w,int h,wxImage* image)
{
	*image = self->GetSubImage(wxRect(x, y, w, h));
}

EWXWEXPORT(int,wxImage_GetType)(wxImage* self)
{
	return self->GetType();
}

EWXWEXPORT(void,wxImage_SetType)(wxImage* self,int type)
{
	self->SetType((wxBitmapType) type);
}

EWXWEXPORT(void,wxImage_Paste)(wxImage* self,wxImage* image,int x,int y)
{
	self->Paste(*image, x, y);
}
	
EWXWEXPORT(void,wxImage_Scale)(wxImage* self,int width,int height,wxImage* image)
{
	*image = self->Scale(width, height);
}

EWXWEXPORT(void,wxImage_ScaleEx)(wxImage* self,int width,int height,int quality,wxImage* image)
{
	*image = self->Scale(width, height,(wxImageResizeQuality) quality);
}
	
EWXWEXPORT(void,wxImage_Rescale)(wxImage* self,int width,int height)
{
	self->Rescale(width, height);
}

EWXWEXPORT(void,wxImage_RescaleEx)(wxImage* self,int width,int height,int quality)
{
	self->Rescale(width, height,(wxImageResizeQuality) quality);
}
	
EWXWEXPORT(void,wxImage_Rotate)(wxImage* self,double angle,int c_x,int c_y,bool interpolating,void* offset_after_rotation,wxImage* image)
{
	*image = self->Rotate(angle, wxPoint(c_x, c_y), interpolating, (wxPoint*)offset_after_rotation);
}
	
EWXWEXPORT(void,wxImage_Rotate90)(wxImage* self,bool clockwise,wxImage* image)
{
	*image = self->Rotate90(clockwise);
}
	
EWXWEXPORT(void,wxImage_Mirror)(wxImage* self,bool horizontally,wxImage* image)
{
	*image = self->Mirror(horizontally);
}
	
EWXWEXPORT(void,wxImage_Replace)(wxImage* self,wxUint8 r1,wxUint8 g1,wxUint8 b1,wxUint8 r2,wxUint8 g2,wxUint8 b2)
{
	self->Replace(r1, g1, b1, r2, g2, b2);
}
	
EWXWEXPORT(void,wxImage_SetRGB)(wxImage* self,int x,int y,wxUint8 r,wxUint8 g,wxUint8 b)
{
	self->SetRGB(x, y, r, g, b);
}
	
EWXWEXPORT(wxUint8,wxImage_GetRed)(wxImage* self,int x,int y)
{
	return self->GetRed(x, y);
}
	
EWXWEXPORT(wxUint8,wxImage_GetGreen)(wxImage* self,int x,int y)
{
	return self->GetGreen(x, y);
}
	
EWXWEXPORT(wxUint8,wxImage_GetBlue)(wxImage* self,int x,int y)
{
	return self->GetBlue(x, y);
}
	
EWXWEXPORT(bool,wxImage_CanRead)(wxString* name)
{
	return wxImage::CanRead(*name);
}
	
EWXWEXPORT(bool, wxImage_LoadFile)(wxImage *self, wxString *name, wxBitmapType type)
{
	return self->LoadFile(*name, type);
}

EWXWEXPORT(bool, wxImage_LoadStream)(wxImage *self, wxInputStream *stream, wxBitmapType type, int index)
{
	return self->LoadFile(*stream, type, index);
}
	
EWXWEXPORT(bool, wxImage_SaveFile)(wxImage *self, wxString *name, wxBitmapType type)
{
	return self->SaveFile(*name, type);
}

EWXWEXPORT(bool, wxImage_SaveStream)(wxImage *self, wxOutputStream *stream, wxBitmapType type)
{
	return self->SaveFile(*stream, type);
}
	
EWXWEXPORT(bool,wxImage_IsOk)(wxImage* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(int,wxImage_GetWidth)(wxImage* self)
{
	return self->GetWidth();
}
	
EWXWEXPORT(int,wxImage_GetHeight)(wxImage* self)
{
	return self->GetHeight();
}
	
EWXWEXPORT(void*,wxImage_GetData)(wxImage* self)
{
	return (void*)self->GetData();
}
	
EWXWEXPORT(void,wxImage_SetData)(wxImage* self,void* data)
{
	self->SetData((unsigned char*)data);
}
	
EWXWEXPORT(void,wxImage_SetDataAndSize)(wxImage* self,char* data,int new_width,int new_height)
{
	self->SetData((unsigned char*)data, new_width, new_height);
}
	
EWXWEXPORT(void,wxImage_SetMaskColour)(wxImage* self,wxUint8 r,wxUint8 g,wxUint8 b)
{
	self->SetMaskColour(r, g, b);
}
	
EWXWEXPORT(wxUint8,wxImage_GetMaskRed)(wxImage* self)
{
	return self->GetMaskRed();
}
	
EWXWEXPORT(wxUint8,wxImage_GetMaskGreen)(wxImage* self)
{
	return self->GetMaskGreen();
}
	
EWXWEXPORT(wxUint8,wxImage_GetMaskBlue)(wxImage* self)
{
	return self->GetMaskBlue();
}
	
EWXWEXPORT(void,wxImage_SetMask)(wxImage* self,bool mask)
{
	self->SetMask(mask);
}
	
EWXWEXPORT(bool,wxImage_HasMask)(wxImage* self)
{
	return self->HasMask();
}
	
EWXWEXPORT(int,wxImage_CountColours)(wxImage* self,int stopafter)
{
	return self->CountColours((long)stopafter);
}

EWXWEXPORT (wxString*,wxImage_GetOption)(wxImage* self,wxString* key)
{
	wxString *result = new wxString();
	*result = self->GetOption(*key);
	return result;
}

EWXWEXPORT (int,wxImage_GetOptionInt)(wxImage* self,wxString* key)
{
	return  self->GetOptionInt(*key);
}

EWXWEXPORT(void,wxImage_SetOption)(wxImage* self,wxString* key,wxString* value)
{
	self->SetOption(*key,*value);
}

EWXWEXPORT(void,wxImage_SetOptionInt)(wxImage* self,wxString* key,int value)
{
	self->SetOption(*key, value);
}

EWXWEXPORT(int,wxImage_HasOption)(wxImage* self,wxString* key)
{
	return self->HasOption(*key);
}

}
