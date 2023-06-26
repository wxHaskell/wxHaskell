/*-----------------------------------------------------------------------------
  MediaCtrl
-----------------------------------------------------------------------------*/
TClassDefExtend(wxMediaCtrl,wxWindow);

TClass(wxMediaCtrl)  wxMediaCtrl_Create( TClass(wxWindow) parent, int windowID, TClass(wxString) fileName, int x, int y, int w, int h, long style, TClass(wxString) szBackend, TClass(wxString) name );
void  wxMediaCtrl_Delete(TSelf(wxMediaCtrl) self);
TClass(wxSize) wxMediaCtrl_GetBestSize(TSelf(wxMediaCtrl) self );
double  wxMediaCtrl_GetPlaybackRate(TSelf(wxMediaCtrl) self);
double  wxMediaCtrl_GetVolume(TSelf(wxMediaCtrl) self);
int  wxMediaCtrl_GetState(TSelf(wxMediaCtrl) self);
TInt64 wxMediaCtrl_Length(TSelf(wxMediaCtrl) self);
TBool  wxMediaCtrl_Load(TSelf(wxMediaCtrl) self, TClass(wxString) fileName );
TBool  wxMediaCtrl_LoadURI(TSelf(wxMediaCtrl) self, TClass(wxString) uri );
TBool  wxMediaCtrl_LoadURIWithProxy(TSelf(wxMediaCtrl) self, TClass(wxString) uri, TClass(wxString) proxy );
TBool  wxMediaCtrl_Pause(TSelf(wxMediaCtrl) self);
TBool  wxMediaCtrl_Play(TSelf(wxMediaCtrl) self);
TInt64 wxMediaCtrl_Seek(TSelf(wxMediaCtrl) self, TInt64 offsetWhere, int mode );
TBool  wxMediaCtrl_SetPlaybackRate(TSelf(wxMediaCtrl) self, double dRate );
TBool  wxMediaCtrl_SetVolume(TSelf(wxMediaCtrl) self, double dVolume );
TBool  wxMediaCtrl_ShowPlayerControls(TSelf(wxMediaCtrl) self, int flags );
TBool  wxMediaCtrl_Stop(TSelf(wxMediaCtrl) self);
TInt64 wxMediaCtrl_Tell(TSelf(wxMediaCtrl) self);

TClassDefExtend(wxMediaEvent,wxNotifyEvent);

/* The wxMediaEvent's events */
