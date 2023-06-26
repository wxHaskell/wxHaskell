#include "wxc/wrapper.h"

extern "C"
{

EWXWEXPORT(wxDateTime*,wxDateTime_Create)()
{
	return new wxDateTime();
}

EWXWEXPORT(void,wxDateTime_SetCountry)(int country)
{
	wxDateTime::SetCountry((wxDateTime::Country)country);
}
	
EWXWEXPORT(int,wxDateTime_GetCountry)()
{
	return (int)wxDateTime::GetCountry();
}
	
EWXWEXPORT(bool,wxDateTime_IsWestEuropeanCountry)(int country)
{
	return wxDateTime::IsWestEuropeanCountry((wxDateTime::Country)country);
}
	
EWXWEXPORT(int,wxDateTime_GetCurrentYear)(int cal)
{
	return wxDateTime::GetCurrentYear((wxDateTime::Calendar)cal);
}
	
EWXWEXPORT(int,wxDateTime_ConvertYearToBC)(int year)
{
	return wxDateTime::ConvertYearToBC(year);
}
	
EWXWEXPORT(int,wxDateTime_GetCurrentMonth)(int cal)
{
	return (int)wxDateTime::GetCurrentMonth((wxDateTime::Calendar)cal);
}
	
EWXWEXPORT(bool,wxDateTime_IsLeapYear)(int year,int cal)
{
	return wxDateTime::IsLeapYear(year, (wxDateTime::Calendar)cal);
}
	
EWXWEXPORT(int,wxDateTime_GetCentury)(int year)
{
	return wxDateTime::GetCentury(year);
}
	
EWXWEXPORT(int,wxDateTime_GetNumberOfDays)(int year,int cal)
{
	return (int)wxDateTime::GetNumberOfDays(year, (wxDateTime::Calendar)cal);
}
	
EWXWEXPORT(int,wxDateTime_GetNumberOfDaysMonth)(int month,int year,int cal)
{
	return wxDateTime::GetNumberOfDays((wxDateTime::Month)month, year, (wxDateTime::Calendar)cal);
}
	
EWXWEXPORT(wxString*,wxDateTime_GetMonthName)(int month,int flags)
{
	wxString *result = new wxString();
	*result = wxDateTime::GetMonthName((wxDateTime::Month)month, (wxDateTime::NameFlags)flags);
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_GetWeekDayName)(int weekday,int flags)
{
	wxString *result = new wxString();
	*result = wxDateTime::GetWeekDayName((wxDateTime::WeekDay)weekday, (wxDateTime::NameFlags)flags);
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_GetAmString)()
{
	wxString *result = new wxString();
	wxString pm;
	wxDateTime::GetAmPmStrings(result, &pm);
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_GetPmString)()
{
	wxString *result = new wxString();
	wxString am;
	wxDateTime::GetAmPmStrings(&am, result);
	return result;
}
	
EWXWEXPORT(bool,wxDateTime_IsDSTApplicable)(int year,int country)
{
	return wxDateTime::IsDSTApplicable(year, (wxDateTime::Country)country);
}
	
EWXWEXPORT(void,wxDateTime_GetBeginDST)(int year,int country,wxDateTime* dt)
{
	*dt = wxDateTime::GetBeginDST(year, (wxDateTime::Country)country);
}
	
EWXWEXPORT(void,wxDateTime_GetEndDST)(int year,int country,wxDateTime* dt)
{
	*dt = wxDateTime::GetEndDST(year, (wxDateTime::Country)country);
}
	
EWXWEXPORT(void,wxDateTime_Now)(wxDateTime* dt)
{
	*dt = wxDateTime::Now();
}
	
EWXWEXPORT(void,wxDateTime_UNow)(wxDateTime* dt)
{
	*dt = wxDateTime::UNow();
}
	
EWXWEXPORT(void,wxDateTime_Today)(wxDateTime* dt)
{
	*dt = wxDateTime::Today();
}
	
EWXWEXPORT(void,wxDateTime_SetToCurrent)(wxDateTime* self)
{
	self->SetToCurrent();
}
	
EWXWEXPORT(void,wxDateTime_SetTime)(wxDateTime* self,int hour,int minute,int second,int millisec)
{
	self->Set((wxDateTime::wxDateTime_t)hour, (wxDateTime::wxDateTime_t)minute, (wxDateTime::wxDateTime_t)second, (wxDateTime::wxDateTime_t)millisec);
}
	
EWXWEXPORT(void,wxDateTime_Set)(wxDateTime* self,int day,int month,int year,int hour,int minute,int second,int millisec)
{
	self->Set((wxDateTime::wxDateTime_t)day, (wxDateTime::Month)month, year, (wxDateTime::wxDateTime_t)hour,  (wxDateTime::wxDateTime_t)minute, (wxDateTime::wxDateTime_t)second, (wxDateTime::wxDateTime_t)millisec);
}
	
EWXWEXPORT(void,wxDateTime_ResetTime)(wxDateTime* self)
{
	self->ResetTime();
}
	
EWXWEXPORT(void,wxDateTime_SetYear)(wxDateTime* self,int year)
{
	self->SetYear(year);
}
	
EWXWEXPORT(void,wxDateTime_SetMonth)(wxDateTime* self,int month)
{
	self->SetMonth((wxDateTime::Month)month);
}
	
EWXWEXPORT(void,wxDateTime_SetDay)(wxDateTime* self,int day)
{
	self->SetDay((wxDateTime::wxDateTime_t)day);
}
	
EWXWEXPORT(void,wxDateTime_SetHour)(wxDateTime* self,int hour)
{
	self->SetHour((wxDateTime::wxDateTime_t)hour);
}
	
EWXWEXPORT(void,wxDateTime_SetMinute)(wxDateTime* self,int minute)
{
	self->SetMinute((wxDateTime::wxDateTime_t)minute);
}
	
EWXWEXPORT(void,wxDateTime_SetSecond)(wxDateTime* self,int second)
{
	self->SetSecond((wxDateTime::wxDateTime_t)second);
}
	
EWXWEXPORT(void,wxDateTime_SetMillisecond)(wxDateTime* self,int millisecond)
{
	self->SetMillisecond((wxDateTime::wxDateTime_t)millisecond);
}
	
EWXWEXPORT(void,wxDateTime_SetToWeekDayInSameWeek)(wxDateTime* self,int weekday)
{
	self->SetToWeekDayInSameWeek((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(void,wxDateTime_GetWeekDayInSameWeek)(wxDateTime* self,int weekday,wxDateTime* _ref)
{
	*_ref = self->GetWeekDayInSameWeek((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(void,wxDateTime_SetToNextWeekDay)(wxDateTime* self,int weekday)
{
	self->SetToNextWeekDay((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(void,wxDateTime_GetNextWeekDay)(wxDateTime* self,int weekday,wxDateTime* _ref)
{
	*_ref = self->GetNextWeekDay((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(void,wxDateTime_SetToPrevWeekDay)(wxDateTime* self,int weekday)
{
	self->SetToPrevWeekDay((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(void,wxDateTime_GetPrevWeekDay)(wxDateTime* self,int weekday,wxDateTime* _ref)
{
	*_ref = self->GetPrevWeekDay((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(bool,wxDateTime_SetToWeekDay)(wxDateTime* self,int weekday,int n,int month,int year)
{
	return self->SetToWeekDay((wxDateTime::WeekDay)weekday, n, (wxDateTime::Month)month, year);
}
	
EWXWEXPORT(void,wxDateTime_GetWeekDay)(wxDateTime* self,int weekday,int n,int month,int year,wxDateTime* _ref)
{
	*_ref = self->GetWeekDay((wxDateTime::WeekDay)weekday, n, (wxDateTime::Month)month, year);
}
	
EWXWEXPORT(bool,wxDateTime_SetToLastWeekDay)(wxDateTime* self,int weekday,int month,int year)
{
	return self->SetToLastWeekDay((wxDateTime::WeekDay)weekday, (wxDateTime::Month)month, year);
}
	
EWXWEXPORT(void,wxDateTime_GetLastWeekDay)(wxDateTime* self,int weekday,int month,int year,wxDateTime* _ref)
{
	*_ref = self->GetLastWeekDay((wxDateTime::WeekDay)weekday, (wxDateTime::Month)month, year);
}
	
EWXWEXPORT(void,wxDateTime_SetToLastMonthDay)(wxDateTime* self,int month,int year)
{
	self->SetToLastMonthDay((wxDateTime::Month)month, year);
}
	
EWXWEXPORT(void,wxDateTime_GetLastMonthDay)(wxDateTime* self,int month,int year,wxDateTime* _ref)
{
	*_ref = self->GetLastMonthDay((wxDateTime::Month)month, year);
}
	
EWXWEXPORT(void,wxDateTime_ToTimezone)(wxDateTime* self,int tz,bool noDST)
{
	self->ToTimezone(wxDateTime::TimeZone((wxDateTime::TZ)tz), noDST);
}
	
EWXWEXPORT(void,wxDateTime_MakeTimezone)(wxDateTime* self,int tz,bool noDST)
{
	self->MakeTimezone(wxDateTime::TimeZone((wxDateTime::TZ)tz), noDST);
}
	
EWXWEXPORT(void,wxDateTime_ToGMT)(wxDateTime* self,bool noDST)
{
	self->ToGMT(noDST);
}
	
EWXWEXPORT(void,wxDateTime_MakeGMT)(wxDateTime* self,bool noDST)
{
	self->MakeGMT(noDST);
}
	
EWXWEXPORT(int,wxDateTime_IsDST)(wxDateTime* self,int country)
{
	return self->IsDST((wxDateTime::Country)country);
}
	
EWXWEXPORT(bool,wxDateTime_IsValid)(wxDateTime* self)
{
	return self->IsValid();
}
	
EWXWEXPORT(time_t,wxDateTime_GetTicks)(wxDateTime* self)
{
	return self->GetTicks();
}
	
EWXWEXPORT(int,wxDateTime_GetYear)(wxDateTime* self,int tz)
{
	return self->GetYear(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetMonth)(wxDateTime* self,int tz)
{
	return (int)self->GetMonth(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetDay)(wxDateTime* self,int tz)
{
	return (int)self->GetDay(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetWeekDayTZ)(wxDateTime* self,int tz)
{
	return (int)self->GetWeekDay(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetHour)(wxDateTime* self,int tz)
{
	return (int)self->GetHour(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetMinute)(wxDateTime* self,int tz)
{
	return (int)self->GetMinute(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetSecond)(wxDateTime* self,int tz)
{
	return (int)self->GetSecond(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetMillisecond)(wxDateTime* self,int tz)
{
	return (int)self->GetMillisecond(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetDayOfYear)(wxDateTime* self,int tz)
{
	return (int)self->GetDayOfYear(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetWeekOfYear)(wxDateTime* self,int flags,int tz)
{
	return (int)self->GetWeekOfYear((wxDateTime::WeekFlags)flags, wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetWeekOfMonth)(wxDateTime* self,int flags,int tz)
{
	return (int)self->GetWeekOfMonth((wxDateTime::WeekFlags)flags, wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(bool,wxDateTime_IsWorkDay)(wxDateTime* self,int country)
{
	return self->IsWorkDay((wxDateTime::Country)country);
}
	
/*
EWXWEXPORT(bool,wxDateTime_IsGregorianDate)(wxDateTime* self,int country)
{
	return self->IsGregorianDate((wxDateTime::GregorianAdoption)country);
}
*/
	
EWXWEXPORT(bool,wxDateTime_IsEqualTo)(wxDateTime* self,wxDateTime* datetime)
{
	return self->IsEqualTo(*datetime);
}
	
EWXWEXPORT(bool,wxDateTime_IsEarlierThan)(wxDateTime* self,wxDateTime* datetime)
{
	return self->IsEarlierThan(*datetime);
}
	
EWXWEXPORT(bool,wxDateTime_IsLaterThan)(wxDateTime* self,wxDateTime* datetime)
{
	return self->IsLaterThan(*datetime);
}
	
EWXWEXPORT(bool,wxDateTime_IsStrictlyBetween)(wxDateTime* self,wxDateTime* t1,wxDateTime* t2)
{
	return self->IsStrictlyBetween(*t1,*t2);
}
	
EWXWEXPORT(bool,wxDateTime_IsBetween)(wxDateTime* self,wxDateTime* t1,wxDateTime* t2)
{
	return self->IsBetween(*t1,*t2);
}
	
EWXWEXPORT(bool,wxDateTime_IsSameDate)(wxDateTime* self,wxDateTime* dt)
{
	return self->IsSameDate(*dt);
}
	
EWXWEXPORT(bool,wxDateTime_IsSameTime)(wxDateTime* self,wxDateTime* dt)
{
	return self->IsSameTime(*dt);
}
	
EWXWEXPORT(bool,wxDateTime_IsEqualUpTo)(wxDateTime* self,wxDateTime* dt,wxTimeSpan* ts)
{
	return self->IsEqualUpTo(*dt,*ts);
}
	
EWXWEXPORT(void,wxDateTime_AddTime)(wxDateTime* self,wxTimeSpan* diff,wxDateTime* _ref)
{
	*_ref = self->Add(*diff);
}
	
EWXWEXPORT(void,wxDateTime_SubtractTime)(wxDateTime* self,wxTimeSpan* diff,wxDateTime* _ref)
{
	*_ref = self->Subtract(*diff);
}
	
EWXWEXPORT(void,wxDateTime_AddDate)(wxDateTime* self,wxDateSpan* diff,wxDateTime* _ref)
{
	*_ref = self->Add(*diff);
}
	
EWXWEXPORT(void,wxDateTime_SubtractDate)(wxDateTime* self,wxDateSpan* diff,wxDateTime* _ref)
{
	*_ref = self->Subtract(*diff);
}

#if (wxVERSION_NUMBER < 2900)	
EWXWEXPORT(void*,wxDateTime_ParseRfc822Date)(wxDateTime* self,void* date)
{
	return (void*)self->ParseRfc822Date((const wxChar*)date);
}
#else
EWXWEXPORT(void*,wxDateTime_ParseRfc822Date)(wxDateTime* self,void* date)
{
    wxString dateStr((const wxChar*)date);
    wxString::const_iterator parseOut;
    wxString remainStr;

    if (self->ParseRfc822Date(dateStr, &parseOut))
    {
        // The iterator points to the first character after the string
        // We should return a pointer to the remainder, but this is complicated
        // in Unicode by the fact that code points are not simple pointers...
        for (; parseOut != dateStr.end(); ++parseOut)
            remainStr += *parseOut;

        delete ((const wxChar*) date);
        return (void*) remainStr.wchar_str();
    }
    else
        return NULL;
}
#endif
	
#if (wxVERSION_NUMBER < 2900)	
EWXWEXPORT(void*,wxDateTime_ParseFormat)(wxDateTime* self,void* date,void* format,wxDateTime* dateDef)
{
	return (void*)self->ParseFormat((const wxChar*)date, (const wxChar*)format,*dateDef);
}	
#else
EWXWEXPORT(void*,wxDateTime_ParseFormat)(wxDateTime* self,void* date,void* format,wxDateTime* dateDef)
{
    wxString dateStr((const wxChar*)date);
    wxString parseFmt((const wxChar*)format);
    wxString::const_iterator parseOut;
    wxString remainStr;

    if (self->ParseFormat(dateStr, parseFmt, *dateDef, &parseOut))
    {
        for (; parseOut != dateStr.end(); ++parseOut)
            remainStr += *parseOut;

        delete ((const wxChar*) date);
        return (void*) remainStr.wchar_str();
    }
    else
        return NULL;
}
#endif
	
#if (wxVERSION_NUMBER < 2900)	
EWXWEXPORT(void*,wxDateTime_ParseDateTime)(wxDateTime* self,void* datetime)
{
	return (void*)self->ParseDateTime((const wxChar*)datetime);
}	
#else
EWXWEXPORT(void*,wxDateTime_ParseDateTime)(wxDateTime* self,void* datetime)
{
    wxString dateStr((const wxChar*)datetime);
    wxString::const_iterator parseOut;
    wxString remainStr;

    if (self->ParseDateTime(dateStr, &parseOut))
    {
        for (; parseOut != dateStr.end(); ++parseOut)
            remainStr += *parseOut;

        delete ((const wxChar*) datetime);
        return (void*) remainStr.wchar_str();
    }
    else
        return NULL;
}
#endif
	
#if (wxVERSION_NUMBER < 2900)	
EWXWEXPORT(void*,wxDateTime_ParseDate)(wxDateTime* self,void* date)
{
	return (void*)self->ParseDate((const wxChar*)date);
}
#else
EWXWEXPORT(void*,wxDateTime_ParseDate)(wxDateTime* self,void* date)
{
    wxString dateStr((const wxChar*)date);
    wxString::const_iterator parseOut;
    wxString remainStr;

    if (self->ParseDate(dateStr, &parseOut))
    {
        for (; parseOut != dateStr.end(); ++parseOut)
            remainStr += *parseOut;

        delete ((const wxChar*) date);
        return (void*) remainStr.wchar_str();
    }
    else
        return NULL;
}
#endif
	
#if (wxVERSION_NUMBER < 2900)	
EWXWEXPORT(void*,wxDateTime_ParseTime)(wxDateTime* self,void* time)
{
	return (void*)self->ParseTime((const wxChar*)time);
}
#else
EWXWEXPORT(void*,wxDateTime_ParseTime)(wxDateTime* self,void* time)
{
    wxString timeStr((const wxChar*)time);
    wxString::const_iterator parseOut;
    wxString remainStr;

    if (self->ParseTime(timeStr, &parseOut))
    {
        for (; parseOut != timeStr.end(); ++parseOut)
            remainStr += *parseOut;

        delete ((const wxChar*) time);
        return (void*) remainStr.wchar_str();
    }
    else
        return NULL;
}
#endif
	
EWXWEXPORT(wxString*,wxDateTime_Format)(wxDateTime* self,void* format,int tz)
{
	wxString *result = new wxString();
	*result = self->Format((const wxChar*)format, wxDateTime::TimeZone((wxDateTime::TZ)tz));
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_FormatDate)(wxDateTime* self)
{
	wxString *result = new wxString();
	*result = self->FormatDate();
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_FormatTime)(wxDateTime* self)
{
	wxString *result = new wxString();
	*result = self->FormatTime();
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_FormatISODate)(wxDateTime* self)
{
	wxString *result = new wxString();
	*result =  self->FormatISODate();
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_FormatISOTime)(wxDateTime* self)
{
	wxString *result = new wxString();
	*result = self->FormatISOTime();
	return result;
}
	
EWXWEXPORT(wxDateTime*,wxDateTime_wxDateTime)(long hi_long,unsigned long lo_long)
{
	return new wxDateTime(wxLongLong(hi_long, lo_long));
}
	
EWXWEXPORT(void,wxDateTime_GetValue)(wxDateTime* self,long* hi_long,unsigned long* lo_long)
{
	wxLongLong val = self->GetValue();
	*hi_long = val.GetHi();
	*lo_long = val.GetLo();
}
	
EWXWEXPORT(int,wxDateTime_GetTimeNow)()
{
	return (int)wxDateTime::GetTimeNow();
}
	
EWXWEXPORT(void,wxDateTime_AddTimeValues)(wxDateTime* self,int _hrs,int _min,int _sec,int _mls)
{
	self->Add(wxTimeSpan((long)_hrs, (long)_min, (long)_sec, (long)_mls));
}
	
EWXWEXPORT(void,wxDateTime_AddDateValues)(wxDateTime* self,int _yrs,int _mnt,int _wek,int _day)
{
	self->Add(wxDateSpan((long)_yrs, (long)_mnt, (long)_wek, (long)_day));
}
	
}
