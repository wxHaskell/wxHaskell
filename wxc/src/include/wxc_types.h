#ifndef WXC_TYPES_H
#define WXC_TYPES_H

/* Types: we use standard pre-processor definitions to add more
   type information to the C signatures. These 'types' can be
   either read by other tools to automatically generate a marshalling
   layer for foreign languages, or you can define the macros in such
   a way that they contain more type information while compiling the
   library itself. 
   
   All macros start with "T" to avoid clashes with other libraries. 
*/
#undef TClassDef
#undef TClassDefExtend
#undef TChar
#undef TUInt8
#undef TInt64
#undef TIntPtr
#undef TBool
#undef TBoolInt
#undef TClass
#undef TSelf
#undef TClassRef      
#undef TClosureFun
#undef TString
#undef TStringVoid
#undef TStringOut
#undef TStringOutVoid
#undef TStringLen
#undef TPoint
#undef TPointLong
#undef TPointOut
#undef TPointOutVoid
#undef TSize
#undef TSizeOut
#undef TSizeOutVoid
#undef TVector
#undef TVectorOut
#undef TVectorOutVoid
#undef TRect
#undef TRectOut
#undef TRectOutVoid
#undef TArrayString
#undef TArrayInt
#undef TArrayIntPtr
#undef TArrayObject
#undef TArrayLen
#undef TArrayIntOut
#undef TArrayIntOutVoid
#undef TArrayIntPtrOut
#undef TArrayIntPtrOutVoid
#undef TArrayStringOut
#undef TArrayStringOutVoid
#undef TArrayObjectOut
#undef TArrayObjectOutVoid
#undef TColorRGB

/* Class definitions */
#define TClassDef(tp)     
#define TClassDefExtend(tp,parent)

/* Types that can be 'untyped' or C++ typed */
#ifdef WXC_USE_TYPED_INTERFACE
# define TClass(tp)     tp*
# define TBool          bool
# define TClosureFun    ClosureFun
#else
# define TClass(tp)     void*
# define TBool          int
# define TClosureFun    void*
#endif

/* basic types */
#ifdef wxUSE_UNICODE
#define TChar             wchar_t
#else
#define TChar             char
#endif

/* integer which can hold a pointer */
#define TIntPtr           intptr_t

/* 64 bit integer */
#define TInt64            int64_t

/* unsigned integer */
#define TUInt             uint32_t

/* 8 bit unsigned integer */
#define TUInt8            uint8_t

/* 32 bit unsigned integer */
#define TUInt32           uint32_t

/* boolean as int */
#define TBoolInt          int

/* classes. 
   "Ref" is used for classes assigned by reference.
   "Self" is used for the 'this' or 'self' pointer.
*/
#define TSelf(tp)         TClass(tp)
#define TClassRef(tp)     TClass(tp)

/* strings */
#define TString           TChar*
#define TStringOut        TChar*
#define TStringLen        int

#define TByteData            char*
#define TByteString(d,n)     TByteData* d, int n
#define TByteStringLazy(d,n) TByteData* d, int n
#define TByteStringOut       TByteData
#define TByteStringLazyOut   TByteData
#define TByteStringLen       int

/* structures */
#define TPoint(x,y)       int x,  int y
#define TPointOut(x,y)    int* x, int* y
#define TVector(x,y)      int x,  int y
#define TVectorOut(x,y)   int* x, int* y
#define TSize(w,h)        int w,  int h
#define TSizeOut(w,h)     int* w, int* h
#define TRect(x,y,w,h)    int x,  int y,  int w,  int h
#define TRectOut(x,y,w,h) int* x, int* y, int* w, int* h
#define TColorRGB(r,g,b)  TUInt8 r, TUInt8 g, TUInt8 b

/* arrays */
#define TArrayLen               int
#define TArrayIntOut            intptr_t*
#define TArrayIntPtrOut         intptr_t*
#define TArrayStringOut         TString*
#define TArrayObjectOut(tp)     TClass(tp)*

#define TArrayString(n,p)       int n, TString* p
#define TArrayInt(n,p)          int n, int* p
#define TArrayIntPtr(n,p)       int n, intptr_t* p
#define TArrayObject(n,tp,p)    int n, TClass(tp)* p

/* Define "Void" variants for void* declared signatures.
   we only use this for compatibility with the original ewxw_glue.h */
#ifdef WXC_USE_TYPED_INTERFACE
# define TStringVoid             TString
# define TStringOutVoid          TStringOut
# define TPointOutVoid(x,y)      TPointOut(x,y)
# define TVectorOutVoid(x,y)     TVectorOut(x,y)
# define TSizeOutVoid(w,h)       TSizeOut(w,h)
# define TRectOutVoid(x,y,w,h)   TRectOut(x,y,w,h)
# define TArrayIntOutVoid        TArrayIntOut
# define TArrayIntPtrOutVoid     TArrayIntPtrOut
# define TArrayStringOutVoid     TArrayStringOut
# define TArrayObjectOutVoid(tp) TArrayObjectOut(tp)
#else
# define TStringVoid           void*
# define TStringOutVoid        void*
# define TPointOutVoid(x,y)    void* x, void* y
# define TVectorOutVoid(x,y)   void* x, void* y
# define TSizeOutVoid(w,h)     void* w, void* h
# define TRectOutVoid(x,y,w,h) void* x, void* y, void* w, void* h
# define TArrayIntOutVoid        void*
# define TArrayIntPtrOutVoid     void*
# define TArrayStringOutVoid     void*
# define TArrayObjectOutVoid(tp) void*
#endif

/* Define "Long" variants for long declared signatures.
   we only use this for compatibility with the original ewxw_glue.h */
#define TPointLong(x,y)       long x,  long y
#define TPointOutLong(x,y)    long* x, long* y
#define TVectorLong(x,y)      long x,  long y
#define TVectorOutLong(x,y)   long* x, long* y
#define TSizeLong(w,h)        long w,  long h
#define TSizeOutLong(w,h)     long* w, long* h
#define TRectLong(x,y,w,h)    long x,  long y,  long w,  long h
#define TRectOutLong(x,y,w,h) long* x, long* y, long* w, long* h

/* Define "Double" variants for long declared signatures. */
#define TPointDouble(x,y)       double x,  double y
#define TPointOutDouble(x,y)    double* x, double* y
#define TVectorDouble(w,h)      double x,  double y
#define TVectorOutDouble(w,h)   double* x, double* y
#define TRectDouble(x,y,w,h)    double x,  double y,  double w,  double h
#define TRectOutDouble(x,y,w,h) double* x, double* y, double* w, double* h
#define TSizeDouble(w,h)        double x,  double y
#define TSizeOutDouble(w,h)     double* x, double* y

#endif /* WXC_TYPES_H */
