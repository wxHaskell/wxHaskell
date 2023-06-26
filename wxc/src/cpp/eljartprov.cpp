#include "wxc/wrapper.h"
#if wxVERSION_NUMBER >= 2400
#include "wx/artprov.h"

extern "C"
{
typedef void* _cdecl (*TCreateBmp)(void* _obj, void* id, void* clt, int w, int h);
}


class ELJArtProv: public wxArtProvider
{
	private:
		void* EiffelObject;
		TCreateBmp cb;
	protected:
    	virtual wxBitmap CreateBitmap(const wxArtID& id, const wxArtClient& client, const wxSize& size)
		{
			if (EiffelObject)
			{
				void* res = cb (EiffelObject, (void*)id.wchar_str(), (void*)client.wchar_str(), size.GetWidth(), size.GetHeight());
				
				if (res)
					return (*((wxBitmap*)res));
				else
					return wxNullBitmap;
			}
			return wxNullBitmap;
		}
	public:
		ELJArtProv (void* obj, void* clb){EiffelObject = obj; cb = (TCreateBmp)clb;};
		void Release(){EiffelObject = NULL; cb = NULL;};
};

extern "C"
{

EWXWEXPORT(void*,ELJArtProv_Create)(void* _obj,void* _clb)
{
	return (void*)new ELJArtProv(_obj, _clb);
}
	
EWXWEXPORT(void,ELJArtProv_Release)(ELJArtProv* self)
{
	self->Release();
	delete self;
}
	
EWXWEXPORT(void,PushProvider)(wxArtProvider* provider)
{
#if WXWIN_COMPATIBILITY_2_6
	wxArtProvider::PushProvider(provider);
#else
	wxArtProvider::Push(provider);
#endif
}

EWXWEXPORT(bool,PopProvider)()
{
#if WXWIN_COMPATIBILITY_2_6
	return wxArtProvider::PopProvider();
#else
	return wxArtProvider::Pop();
#endif
}

EWXWEXPORT(bool,RemoveProvider)(wxArtProvider* provider)
{
#if WXWIN_COMPATIBILITY_2_6
	return wxArtProvider::RemoveProvider(provider);
#else
	return wxArtProvider::Remove(provider);
#endif
}

}
#endif
