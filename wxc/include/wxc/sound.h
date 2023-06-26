/*-----------------------------------------------------------------------------
  Sound
-----------------------------------------------------------------------------*/
TClassDefExtend(wxSound,wxObject);

/** Usage: @soundCreate fileName isResource@. As yet (Nov 2003) unsupported on MacOS X */
TClass(wxSound)  wxSound_Create( TClass(wxString) fileName, TBool isResource );
void  wxSound_Delete(TSelf(wxSound) self);
TBool  wxSound_IsOk(TSelf(wxSound) self);
TBool  wxSound_Play(TSelf(wxSound) self, int flag );
void  wxSound_Stop(TSelf(wxSound) self);
