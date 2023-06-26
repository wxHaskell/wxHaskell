#include "wxc/wrapper.h"
#include "wx/fontenum.h"
#include "wx/fontmap.h"
#include "wx/encconv.h"

extern "C"
{

typedef int _cdecl (*TTextEnum) (void* self, void* _txt);

}

class ELJFontEnumerator : public wxFontEnumerator
{
	private:
		TTextEnum func;
		void*     EiffelObject;
	public:
		ELJFontEnumerator (void* self, void* _fnc) : wxFontEnumerator()
		{
			func = (TTextEnum)_fnc;
			EiffelObject = self;
		}
		
	    virtual bool OnFacename(const wxString& facename)
        { 
			return func(EiffelObject, (void*)facename.wchar_str()) != 0;
		}
		virtual bool OnFontEncoding(const wxString& WXUNUSED(facename), const wxString& encoding)
        {
			return func(EiffelObject, (void*)encoding.wchar_str()) != 0;
		}

};

extern "C"
{

EWXWEXPORT(wxFont*,wxFont_Create)(int pointSize,int family,int style,int weight,bool underlined,wxString* face,int enc)
{
	return new wxFont (pointSize, family, style, weight, underlined,*face, (wxFontEncoding)enc);
}

EWXWEXPORT(wxFont*,wxFont_CreateDefault)()
{
	return new wxFont ();
}

EWXWEXPORT(void*,wxFont_CreateFromStock)(int id)
{
	switch(id) {
	case 0:
		return (void*)wxITALIC_FONT;
	case 1:
		return (void*)wxNORMAL_FONT;
	case 2:
		return (void*)wxSMALL_FONT;
	case 3:
		return (void*)wxSWISS_FONT;
	}

	return NULL;
}

EWXWEXPORT(void,wxFont_Delete)(wxFont* self)
{
	delete self;
}

EWXWEXPORT(bool,wxFont_IsOk)(wxFont* self)
{
	return self->IsOk();
}
	
EWXWEXPORT(int,wxFont_GetPointSize)(wxFont* self)
{
	return self->GetPointSize();
}
	
EWXWEXPORT(int,wxFont_GetFamily)(wxFont* self)
{
	return self->GetFamily();
}
	
EWXWEXPORT(int,wxFont_GetStyle)(wxFont* self)
{
	return self->GetStyle();
}
	
EWXWEXPORT(int,wxFont_GetWeight)(wxFont* self)
{
	return self->GetWeight();
}
	
EWXWEXPORT(bool,wxFont_GetUnderlined)(wxFont* self)
{
	return self->GetUnderlined();
}
	
EWXWEXPORT(wxString*,wxFont_GetFaceName)(wxFont* self)
{
	wxString *result = new wxString();
	*result = self->GetFaceName();
	return result;
}
	
EWXWEXPORT(int,wxFont_GetEncoding)(wxFont* self)
{
	return (int)self->GetEncoding();
}
	
EWXWEXPORT(void,wxFont_SetPointSize)(wxFont* self,int pointSize)
{
	self->SetPointSize(pointSize);
}
	
EWXWEXPORT(void,wxFont_SetFamily)(wxFont* self,int family)
{
	self->SetFamily(family);
}
	
EWXWEXPORT(void,wxFont_SetStyle)(wxFont* self,int style)
{
	self->SetStyle(style);
}
	
EWXWEXPORT(void,wxFont_SetWeight)(wxFont* self,int weight)
{
	self->SetWeight(weight);
}
	
EWXWEXPORT(void,wxFont_SetFaceName)(wxFont* self,wxString* faceName)
{
	self->SetFaceName(*faceName);
}
	
EWXWEXPORT(void,wxFont_SetUnderlined)(wxFont* self,bool underlined)
{
	self->SetUnderlined(underlined);
}
	
EWXWEXPORT(void,wxFont_SetEncoding)(wxFont* self,int encoding)
{
	self->SetEncoding((wxFontEncoding)encoding);
}
	
EWXWEXPORT(wxString*,wxFont_GetFamilyString)(wxFont* self)
{
	wxString *result = new wxString();
	*result = self->GetFamilyString();
	return result;
}
	
EWXWEXPORT(wxString*,wxFont_GetStyleString)(wxFont* self)
{
	wxString *result = new wxString();
	*result = self->GetStyleString();
	return result;
}
	
EWXWEXPORT(wxString*,wxFont_GetWeightString)(wxFont* self)
{
	wxString *result = new wxString();
	*result = self->GetWeightString();
	return result;
}
	
EWXWEXPORT(int,wxFont_GetDefaultEncoding)(wxFont* self)
{
	return (int)self->GetDefaultEncoding();
}
	
EWXWEXPORT(void,wxFont_SetDefaultEncoding)(wxFont* self,int encoding)
{
	self->SetDefaultEncoding((wxFontEncoding) encoding);
}
	

EWXWEXPORT(void*,wxFontEnumerator_Create)(void* self,void* _fnc)
{
	return (void*)new ELJFontEnumerator(self, _fnc);
}

EWXWEXPORT(void,wxFontEnumerator_Delete)(ELJFontEnumerator* self)
{
	delete self;
}

EWXWEXPORT(bool,wxFontEnumerator_EnumerateFacenames)(ELJFontEnumerator* self,int encoding,bool fixedWidthOnly)
{
	return self->EnumerateFacenames((wxFontEncoding)encoding, fixedWidthOnly);
}
	
EWXWEXPORT(bool,wxFontEnumerator_EnumerateEncodings)(ELJFontEnumerator* self,wxString* facename)
{
	return self->EnumerateEncodings(*facename);
}
	

EWXWEXPORT(void*,wxFontMapper_Create)()
{
	return wxTheFontMapper;
}

EWXWEXPORT(bool,wxFontMapper_GetAltForEncoding)(wxFontMapper* self,int encoding,void* alt_encoding,wxString* facename)
{
	return self->GetAltForEncoding((wxFontEncoding)encoding, (wxFontEncoding*)alt_encoding,*facename, false);
}
	
EWXWEXPORT(bool,wxFontMapper_IsEncodingAvailable)(wxFontMapper* self,int encoding,wxString* _buf)
{
	return self->IsEncodingAvailable((wxFontEncoding)encoding,*_buf);
}
	

EWXWEXPORT(void*,wxEncodingConverter_Create)()
{
	return (void*)new wxEncodingConverter();
}

EWXWEXPORT(void,wxEncodingConverter_Delete)(void* self)
{
	delete (wxEncodingConverter*)self;
}

EWXWEXPORT(bool,wxEncodingConverter_Init)(wxEncodingConverter* self,int input_enc,int output_enc,int method)
{
	return self->Init((wxFontEncoding)input_enc, (wxFontEncoding)output_enc, method);
}
	
EWXWEXPORT(void,wxEncodingConverter_Convert)(void* self,void* input,void* output)
{
	((wxEncodingConverter*)self)->Convert((const char*)input, (char*)output);
}

EWXWEXPORT(int,wxEncodingConverter_GetPlatformEquivalents)(void* self,int enc,int platform,void* _lst)
{
	wxFontEncodingArray arr = ((wxEncodingConverter*)self)->GetPlatformEquivalents((wxFontEncoding)enc, platform);
	if (_lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((int*)_lst)[i] = (int)arr.Item(i);
	}
	return (int)arr.GetCount();
}

EWXWEXPORT(int,wxEncodingConverter_GetAllEquivalents)(void* self,int enc,void* _lst)
{
	wxFontEncodingArray arr = ((wxEncodingConverter*)self)->GetAllEquivalents((wxFontEncoding)enc);
	if (_lst)
	{
		for (unsigned int i = 0; i < arr.GetCount(); i++)
			((int*)_lst)[i] = (int)arr.Item(i);
	}
	return (int)arr.GetCount();
}

}
