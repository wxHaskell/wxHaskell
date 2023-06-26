#ifndef __EWXW_DEF_H
#define __EWXW_DEF_H

#ifdef EXPORT
#undef EXPORT
#endif
#define EXPORT extern "C"

#ifdef __WATCOMC__
  #include <windows.h>
  #define EWXWEXPORT(TYPE,FUNC_NAME) TYPE __export __cdecl FUNC_NAME
#else
  #ifdef _WIN32
    #define EWXWEXPORT(TYPE,FUNC_NAME) __declspec(dllexport) TYPE __cdecl FUNC_NAME
    #undef EXPORT
    #define EXPORT extern "C" __declspec(dllexport) 
  #else
    #define EWXWEXPORT(TYPE,FUNC_NAME) TYPE FUNC_NAME
  #endif
  #ifndef _cdecl
    #define _cdecl
  #endif
#endif

#define EWXWCONSTANTINT(NAME,VAL) \
  EWXWEXPORT(int,exp##NAME)() \
    { return (int)VAL;};
#define EWXWCONSTANTSTR(NAME,VAL) \
  EWXWEXPORT(wxString*,exp##NAME)() \
    { return new wxString((const wchar_t*)VAL,wxConvLocal);};
#ifdef __EWX_PREPROCESS
# undef EWXWEXPORT
# undef EWXWCONSTANTINT
# undef EWXWCONSTANTSTR
# define EWXWCONSTANTINT(NAME,VAL) def_const_int(#NAME,VAL);
# define EWXWCONSTANTSTR(NAME,VAL) def_const_str(#NAME,VAL);
#endif

#endif /* #ifndef __EWXW_DEF_H */
