/* update checked 100% with 2.8.9 manual*/
#include "wxc/wrapper.h"

extern "C"
{
#if wxUSE_SOCKETS

/* wxSockAddress */
/* EWXWEXPORT(wxSockAddress*,wxSockAddress_wxSockAddress)() disabled because it has virtual function */
EWXWEXPORT(void,wxSockAddress_Delete)(wxSockAddress* self)
{
  delete self;
}
EWXWEXPORT(void,wxSockAddress_Clear)(wxSockAddress* self)
{
  self->Clear();
}

/* wxIPaddress */
EWXWEXPORT(wxString*,wxIPaddress_Hostname)(wxIPaddress* self)
{
  return new wxString(self->Hostname());
}
EWXWEXPORT(bool,wxIPaddress_HostnameSet)(wxIPaddress* self,wxString* hostname)
{
  return self->Hostname(*hostname);
}
EWXWEXPORT(wxString*,wxIPaddress_IPAddress)(wxIPaddress* self)
{
  return new wxString(self->IPAddress());
}
EWXWEXPORT(wxUint16,wxIPaddress_Service)(wxIPaddress* self)
{
  return self->Service();
}
EWXWEXPORT(bool,wxIPaddress_ServiceSet)(wxIPaddress* self,wxString* service)
{
  return self->Service(*service);
}
EWXWEXPORT(bool,wxIPaddress_ServiceSetPort)(wxIPaddress* self,wxUint16 service)
{
  return self->Service(service);
}

EWXWEXPORT(bool,wxIPaddress_AnyAddress)(wxIPaddress* self)
{
  return self->AnyAddress();
}
EWXWEXPORT(bool,wxIPaddress_LocalHost)(wxIPaddress* self)
{
  return self->LocalHost();
}
EWXWEXPORT(bool,wxIPaddress_IsLocalHost)(wxIPaddress* self)
{
  return self->IsLocalHost();
}

/* wxIPV4address */
EWXWEXPORT(wxIPV4address*,wxIPV4address_Create)()
{
  return new wxIPV4address();
}
EWXWEXPORT(void,wxIPV4address_Delete)(wxIPV4address* self)
{
  delete self;
}

#endif /* wxUSE_SOCKETS */
}
