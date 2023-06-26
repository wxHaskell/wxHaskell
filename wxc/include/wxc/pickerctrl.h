/*-----------------------------------------------------------------------------
   PickerBase
-----------------------------------------------------------------------------*/

TClassDefExtend( wxPickerBase,wxControl);

/*-----------------------------------------------------------------------------
  ColourPickerCtrl
-----------------------------------------------------------------------------*/

TClassDefExtend( wxColourPickerCtrl,wxPickerBase);

// wxdirect ignore wxColour.. -> wxColorPickerCtrl_.. without u
TClass(wxColourPickerCtrl) wxColorPickerCtrl_Create( TClass(wxWindow) parent, int id, TClass(wxColour) colour, TRect(x,y,w,h), int style);
void                       wxColorPickerCtrl_GetColour( TSelf(wxColourPickerCtrl) self,TClassRef(wxColour) colour);
void                       wxColorPickerCtrl_SetColour( TSelf(wxColourPickerCtrl) self, TClass(wxColour) colour);


