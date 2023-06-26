#include "wxc/wrapper.h"

#if (wxVERSION_NUMBER < 2900)
# ifdef wxUSE_SVG
#  include "wx/svg/dcsvg.h"
# endif
#else
# ifndef wxUSE_SVG
#  define wxUSE_SVG 1
# endif
# include "wx/dcsvg.h"
#endif

extern "C"
{

EWXWEXPORT(void*,wxSVGFileDC_Create)(wxString* a_filename)
{
#ifdef wxUSE_SVG
	return (void*) new wxSVGFileDC(*a_filename);
#else
	return NULL;
#endif
}

EWXWEXPORT(void*,wxSVGFileDC_CreateWithSize)(wxString* a_filename,int a_width,int a_height)
{
#ifdef wxUSE_SVG
	return (void*) new wxSVGFileDC(*a_filename, a_width, a_height);
#else
	return NULL;
#endif
}

EWXWEXPORT(void*,wxSVGFileDC_CreateWithSizeAndResolution)(wxString* a_filename,int a_width,int a_height,float a_dpi)
{
#ifdef wxUSE_SVG
	return (void*) new wxSVGFileDC(*a_filename, a_width, a_height, a_dpi);
#else
	return NULL;
#endif
}

EWXWEXPORT(void,wxSVGFileDC_Delete)(void* _obj)
{
#ifdef wxUSE_SVG
	delete (wxSVGFileDC*)_obj;
#endif
}

}
