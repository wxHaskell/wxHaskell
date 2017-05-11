#include "wrapper.h"

extern "C"
{

#if wxCHECK_VERSION(3, 1, 0)
EWXWEXPORT(wxCursor*,Cursor_CreateFromStock)(wxStockCursor _id)
#else
EWXWEXPORT(wxCursor*,Cursor_CreateFromStock)(int _id)
#endif
{
	return  new wxCursor(_id);
}

EWXWEXPORT(wxCursor*,Cursor_CreateFromImage)(wxImage* image)
{
	return  new wxCursor(*image);
}

EWXWEXPORT(wxCursor*,Cursor_CreateLoad)(wxString* name,long type,int width,int height)
{
#if (wxVERSION_NUMBER >= 2900)
    wxBitmapType bm_type = (wxBitmapType) type;
#else
    long bm_type = type;
#endif
#if defined(__WXGTK__)
    return NULL;
#else
    return new wxCursor(*name, bm_type, width, height);
#endif
}

}
