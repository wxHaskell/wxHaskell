#include "wrapper.h"

extern "C"
{

EWXWEXPORT(void*,wxDateTime_Create)()
{
	return (void*) new wxDateTime();
}

EWXWEXPORT(void,wxDateTime_SetCountry)(int country)
{
	wxDateTime::SetCountry((wxDateTime::Country)country);
}
	
EWXWEXPORT(int,wxDateTime_GetCountry)()
{
	return (int)wxDateTime::GetCountry();
}
	
EWXWEXPORT(int,wxDateTime_IsWestEuropeanCountry)(int country)
{
	return (int)wxDateTime::IsWestEuropeanCountry((wxDateTime::Country)country);
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
	
EWXWEXPORT(int,wxDateTime_IsLeapYear)(int year,int cal)
{
	return (int)wxDateTime::IsLeapYear(year, (wxDateTime::Calendar)cal);
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
	
EWXWEXPORT(int,wxDateTime_IsDSTApplicable)(int year,int country)
{
	return (int)wxDateTime::IsDSTApplicable(year, (wxDateTime::Country)country);
}
	
EWXWEXPORT(void,wxDateTime_GetBeginDST)(int year,int country,void* dt)
{
	*((wxDateTime*)dt) = wxDateTime::GetBeginDST(year, (wxDateTime::Country)country);
}
	
EWXWEXPORT(void,wxDateTime_GetEndDST)(int year,int country,void* dt)
{
	*((wxDateTime*)dt) = wxDateTime::GetEndDST(year, (wxDateTime::Country)country);
}
	
EWXWEXPORT(void,wxDateTime_Now)(void* dt)
{
	*((wxDateTime*)dt) = wxDateTime::Now();
}
	
EWXWEXPORT(void,wxDateTime_UNow)(void* dt)
{
	*((wxDateTime*)dt) = wxDateTime::UNow();
}
	
EWXWEXPORT(void,wxDateTime_Today)(void* dt)
{
	*((wxDateTime*)dt) = wxDateTime::Today();
}
	
EWXWEXPORT(void,wxDateTime_SetToCurrent)(void* _obj)
{
	((wxDateTime*)_obj)->SetToCurrent();
}
	
EWXWEXPORT(void,wxDateTime_SetTime)(void* _obj,int hour,int minute,int second,int millisec)
{
	((wxDateTime*)_obj)->Set((wxDateTime::wxDateTime_t)hour, (wxDateTime::wxDateTime_t)minute, (wxDateTime::wxDateTime_t)second, (wxDateTime::wxDateTime_t)millisec);
}
	
EWXWEXPORT(void,wxDateTime_Set)(void* _obj,int day,int month,int year,int hour,int minute,int second,int millisec)
{
	((wxDateTime*)_obj)->Set((wxDateTime::wxDateTime_t)day, (wxDateTime::Month)month, year, (wxDateTime::wxDateTime_t)hour,  (wxDateTime::wxDateTime_t)minute, (wxDateTime::wxDateTime_t)second, (wxDateTime::wxDateTime_t)millisec);
}
	
EWXWEXPORT(void,wxDateTime_ResetTime)(void* _obj)
{
	((wxDateTime*)_obj)->ResetTime();
}
	
EWXWEXPORT(void,wxDateTime_SetYear)(void* _obj,int year)
{
	((wxDateTime*)_obj)->SetYear(year);
}
	
EWXWEXPORT(void,wxDateTime_SetMonth)(void* _obj,int month)
{
	((wxDateTime*)_obj)->SetMonth((wxDateTime::Month)month);
}
	
EWXWEXPORT(void,wxDateTime_SetDay)(void* _obj,int day)
{
	((wxDateTime*)_obj)->SetDay((wxDateTime::wxDateTime_t)day);
}
	
EWXWEXPORT(void,wxDateTime_SetHour)(void* _obj,int hour)
{
	((wxDateTime*)_obj)->SetHour((wxDateTime::wxDateTime_t)hour);
}
	
EWXWEXPORT(void,wxDateTime_SetMinute)(void* _obj,int minute)
{
	((wxDateTime*)_obj)->SetMinute((wxDateTime::wxDateTime_t)minute);
}
	
EWXWEXPORT(void,wxDateTime_SetSecond)(void* _obj,int second)
{
	((wxDateTime*)_obj)->SetSecond((wxDateTime::wxDateTime_t)second);
}
	
EWXWEXPORT(void,wxDateTime_SetMillisecond)(void* _obj,int millisecond)
{
	((wxDateTime*)_obj)->SetMillisecond((wxDateTime::wxDateTime_t)millisecond);
}
	
EWXWEXPORT(void,wxDateTime_SetToWeekDayInSameWeek)(void* _obj,int weekday)
{
	((wxDateTime*)_obj)->SetToWeekDayInSameWeek((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(void,wxDateTime_GetWeekDayInSameWeek)(void* _obj,int weekday,void* _ref)
{
	*((wxDateTime*)_ref) = ((wxDateTime*)_obj)->GetWeekDayInSameWeek((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(void,wxDateTime_SetToNextWeekDay)(void* _obj,int weekday)
{
	((wxDateTime*)_obj)->SetToNextWeekDay((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(void,wxDateTime_GetNextWeekDay)(void* _obj,int weekday,void* _ref)
{
	*((wxDateTime*)_ref) = ((wxDateTime*)_obj)->GetNextWeekDay((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(void,wxDateTime_SetToPrevWeekDay)(void* _obj,int weekday)
{
	((wxDateTime*)_obj)->SetToPrevWeekDay((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(void,wxDateTime_GetPrevWeekDay)(void* _obj,int weekday,void* _ref)
{
	*((wxDateTime*)_ref) = ((wxDateTime*)_obj)->GetPrevWeekDay((wxDateTime::WeekDay)weekday);
}
	
EWXWEXPORT(int,wxDateTime_SetToWeekDay)(void* _obj,int weekday,int n,int month,int year)
{
	return (int)((wxDateTime*)_obj)->SetToWeekDay((wxDateTime::WeekDay)weekday, n, (wxDateTime::Month)month, year);
}
	
EWXWEXPORT(void,wxDateTime_GetWeekDay)(void* _obj,int weekday,int n,int month,int year,void* _ref)
{
	*((wxDateTime*)_ref) = ((wxDateTime*)_obj)->GetWeekDay((wxDateTime::WeekDay)weekday, n, (wxDateTime::Month)month, year);
}
	
EWXWEXPORT(int,wxDateTime_SetToLastWeekDay)(void* _obj,int weekday,int month,int year)
{
	return (int)((wxDateTime*)_obj)->SetToLastWeekDay((wxDateTime::WeekDay)weekday, (wxDateTime::Month)month, year);
}
	
EWXWEXPORT(void,wxDateTime_GetLastWeekDay)(void* _obj,int weekday,int month,int year,void* _ref)
{
	*((wxDateTime*)_ref) = ((wxDateTime*)_obj)->GetLastWeekDay((wxDateTime::WeekDay)weekday, (wxDateTime::Month)month, year);
}
	
EWXWEXPORT(void,wxDateTime_SetToLastMonthDay)(void* _obj,int month,int year)
{
	((wxDateTime*)_obj)->SetToLastMonthDay((wxDateTime::Month)month, year);
}
	
EWXWEXPORT(void,wxDateTime_GetLastMonthDay)(void* _obj,int month,int year,void* _ref)
{
	*((wxDateTime*)_ref) = ((wxDateTime*)_obj)->GetLastMonthDay((wxDateTime::Month)month, year);
}
	
EWXWEXPORT(void,wxDateTime_ToTimezone)(void* _obj,int tz,int noDST)
{
	((wxDateTime*)_obj)->ToTimezone(wxDateTime::TimeZone((wxDateTime::TZ)tz), noDST != 0);
}
	
EWXWEXPORT(void,wxDateTime_MakeTimezone)(void* _obj,int tz,int noDST)
{
	((wxDateTime*)_obj)->MakeTimezone(wxDateTime::TimeZone((wxDateTime::TZ)tz), noDST != 0);
}
	
EWXWEXPORT(void,wxDateTime_ToGMT)(void* _obj,int noDST)
{
	((wxDateTime*)_obj)->ToGMT(noDST != 0);
}
	
EWXWEXPORT(void,wxDateTime_MakeGMT)(void* _obj,int noDST)
{
	((wxDateTime*)_obj)->MakeGMT(noDST != 0);
}
	
EWXWEXPORT(int,wxDateTime_IsDST)(wxDateTime* _obj,int country)
{
	return _obj->IsDST((wxDateTime::Country)country);
}
	
EWXWEXPORT(int,wxDateTime_IsValid)(wxDateTime* _obj)
{
	return (int)_obj->IsValid();
}
	
EWXWEXPORT(time_t,wxDateTime_GetTicks)(void* _obj)
{
	return ((wxDateTime*)_obj)->GetTicks();
}
	
EWXWEXPORT(int,wxDateTime_GetYear)(void* _obj,int tz)
{
	return ((wxDateTime*)_obj)->GetYear(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetMonth)(void* _obj,int tz)
{
	return (int)((wxDateTime*)_obj)->GetMonth(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetDay)(void* _obj,int tz)
{
	return (int)((wxDateTime*)_obj)->GetDay(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetWeekDayTZ)(void* _obj,int tz)
{
	return (int)((wxDateTime*)_obj)->GetWeekDay(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetHour)(void* _obj,int tz)
{
	return (int)((wxDateTime*)_obj)->GetHour(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetMinute)(void* _obj,int tz)
{
	return (int)((wxDateTime*)_obj)->GetMinute(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetSecond)(void* _obj,int tz)
{
	return (int)((wxDateTime*)_obj)->GetSecond(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetMillisecond)(void* _obj,int tz)
{
	return (int)((wxDateTime*)_obj)->GetMillisecond(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetDayOfYear)(void* _obj,int tz)
{
	return (int)((wxDateTime*)_obj)->GetDayOfYear(wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetWeekOfYear)(void* _obj,int flags,int tz)
{
	return (int)((wxDateTime*)_obj)->GetWeekOfYear((wxDateTime::WeekFlags)flags, wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_GetWeekOfMonth)(void* _obj,int flags,int tz)
{
	return (int)((wxDateTime*)_obj)->GetWeekOfMonth((wxDateTime::WeekFlags)flags, wxDateTime::TimeZone((wxDateTime::TZ)tz));
}
	
EWXWEXPORT(int,wxDateTime_IsWorkDay)(wxDateTime* _obj,int country)
{
	return (int)_obj->IsWorkDay((wxDateTime::Country)country);
}
	
/*
EWXWEXPORT(bool,wxDateTime_IsGregorianDate)(wxDateTime* _obj,int country)
{
	return _obj->IsGregorianDate((wxDateTime::GregorianAdoption)country);
}
*/
	
EWXWEXPORT(int,wxDateTime_IsEqualTo)(wxDateTime* _obj,wxDateTime* datetime)
{
	return (int)_obj->IsEqualTo(*datetime);
}
	
EWXWEXPORT(int,wxDateTime_IsEarlierThan)(wxDateTime* _obj,wxDateTime* datetime)
{
	return (int)_obj->IsEarlierThan(*datetime);
}
	
EWXWEXPORT(int,wxDateTime_IsLaterThan)(wxDateTime* _obj,wxDateTime* datetime)
{
	return (int)_obj->IsLaterThan(*datetime);
}
	
EWXWEXPORT(int,wxDateTime_IsStrictlyBetween)(wxDateTime* _obj,wxDateTime* t1,wxDateTime* t2)
{
	return (int)_obj->IsStrictlyBetween(*t1, *t2);
}
	
EWXWEXPORT(int,wxDateTime_IsBetween)(wxDateTime* _obj,wxDateTime* t1,wxDateTime* t2)
{
	return (int)_obj->IsBetween(*t1, *t2);
}
	
EWXWEXPORT(int,wxDateTime_IsSameDate)(wxDateTime* _obj,wxDateTime* dt)
{
	return (int)_obj->IsSameDate(*dt);
}
	
EWXWEXPORT(int,wxDateTime_IsSameTime)(wxDateTime* _obj,wxDateTime* dt)
{
	return (int)_obj->IsSameTime(*dt);
}
	
EWXWEXPORT(int,wxDateTime_IsEqualUpTo)(wxDateTime* _obj,wxDateTime* dt,wxTimeSpan* ts)
{
	return (int)_obj->IsEqualUpTo(*dt, *ts);
}
	
EWXWEXPORT(void,wxDateTime_AddTime)(void* _obj,void* diff,void* _ref)
{
	*((wxDateTime*)_ref) = ((wxDateTime*)_obj)->Add(*((wxTimeSpan*)diff));
}
	
EWXWEXPORT(void,wxDateTime_SubtractTime)(void* _obj,void* diff,void* _ref)
{
	*((wxDateTime*)_ref) = ((wxDateTime*)_obj)->Subtract(*((wxTimeSpan*)diff));
}
	
EWXWEXPORT(void,wxDateTime_AddDate)(void* _obj,void* diff,void* _ref)
{
	*((wxDateTime*)_ref) = ((wxDateTime*)_obj)->Add(*((wxDateSpan*)diff));
}
	
EWXWEXPORT(void,wxDateTime_SubtractDate)(void* _obj,void* diff,void* _ref)
{
	*((wxDateTime*)_ref) = ((wxDateTime*)_obj)->Subtract(*((wxDateSpan*)diff));
}
	
EWXWEXPORT(void*,wxDateTime_ParseRfc822Date)(void* _obj,void* date)
{
	return (void*)((wxDateTime*)_obj)->ParseRfc822Date((const wxChar*)date);
}
	
EWXWEXPORT(void*,wxDateTime_ParseFormat)(void* _obj,void* date,void* format,void* dateDef)
{
	return (void*)((wxDateTime*)_obj)->ParseFormat((const wxChar*)date, (const wxChar*)format, *((wxDateTime*)dateDef));
}
	
EWXWEXPORT(void*,wxDateTime_ParseDateTime)(void* _obj,void* datetime)
{
	return (void*)((wxDateTime*)_obj)->ParseDateTime((const wxChar*)datetime);
}
	
EWXWEXPORT(void*,wxDateTime_ParseDate)(void* _obj,void* date)
{
	return (void*)((wxDateTime*)_obj)->ParseDate((const wxChar*)date);
}
	
EWXWEXPORT(void*,wxDateTime_ParseTime)(void* _obj,void* time)
{
	return (void*)((wxDateTime*)_obj)->ParseTime((const wxChar*)time);
}
	
EWXWEXPORT(wxString*,wxDateTime_Format)(void* _obj,void* format,int tz)
{
	wxString *result = new wxString();
	*result = ((wxDateTime*)_obj)->Format((const wxChar*)format, wxDateTime::TimeZone((wxDateTime::TZ)tz));
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_FormatDate)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxDateTime*)_obj)->FormatDate();
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_FormatTime)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxDateTime*)_obj)->FormatTime();
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_FormatISODate)(void* _obj)
{
	wxString *result = new wxString();
	*result =  ((wxDateTime*)_obj)->FormatISODate();
	return result;
}
	
EWXWEXPORT(wxString*,wxDateTime_FormatISOTime)(void* _obj)
{
	wxString *result = new wxString();
	*result = ((wxDateTime*)_obj)->FormatISOTime();
	return result;
}
	
EWXWEXPORT(void*,wxDateTime_wxDateTime)(long hi_long,unsigned long lo_long)
{
	return (void*) new wxDateTime(wxLongLong(hi_long, lo_long));
}
	
EWXWEXPORT(void,wxDateTime_GetValue)(void* _obj,long* hi_long,unsigned long* lo_long)
{
	wxLongLong val = ((wxDateTime*)_obj)->GetValue();
	*hi_long = val.GetHi();
	*lo_long = val.GetLo();
}
	
EWXWEXPORT(int,wxDateTime_GetTimeNow)()
{
	return (int)wxDateTime::GetTimeNow();
}
	
EWXWEXPORT(void,wxDateTime_AddTimeValues)(void* _obj,int _hrs,int _min,int _sec,int _mls)
{
	((wxDateTime*)_obj)->Add(wxTimeSpan((long)_hrs, (long)_min, (long)_sec, (long)_mls));
}
	
EWXWEXPORT(void,wxDateTime_AddDateValues)(void* _obj,int _yrs,int _mnt,int _wek,int _day)
{
	((wxDateTime*)_obj)->Add(wxDateSpan((long)_yrs, (long)_mnt, (long)_wek, (long)_day));
}
	
}
