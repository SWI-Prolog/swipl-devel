/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <time.h>
#include <gnu/system.h>
#include <h/unix.h>

extern time_t get_date(char *p, struct timeb *now);

#ifndef HAVE_TIMELOCAL
static long timelocal(struct tm *);
#endif

static status	setDate(Date d, Int s, Int m, Int h, Int D, Int M, Int Y);

Date
CtoDate(long int time)
{ Date d = newObject(ClassDate, 0);

  d->date = time;

  answer(d);
}


static status
initialiseDate(Date d, Int s, Int m, Int h, Int D, Int M, Int Y)
{ d->date = time(0);

  if ( notDefault(s) || notDefault(m) || notDefault(h) ||
       notDefault(D) || notDefault(M) || notDefault(Y) )
    setDate(d, s, m, h, D, M, Y);

  succeed;
}


static status
storeDate(Date d, FileObj file)
{ TRY(storeSlotsObject(d, file));

  return storeWordFile(file, (Any) d->date);
}


static status
loadDate(Date d, FILE *fd, ClassDef def)
{ if ( restoreVersion != 2 )
    TRY(loadSlotsObject(d, fd, def));
  d->date = loadWord(fd);

  succeed;
}


static Date
getConvertDate(Class class, StringObj str)
{ if ( isstr8(&str->data) )
  { long t = get_date((char *)str->data.s_text8, NULL);

    if ( t != -1 )
    { Date d = answerObject(ClassDate, 0);
      d->date = t;

      answer(d);
    }
  }

  fail;
}


static status
equalDate(Date d1, Date d2)
{ if (d1->date == d2->date)
    succeed;
  fail;
}


static status
setDate(Date d, Int s, Int m, Int h, Int D, Int M, Int Y)
{ struct tm *tm;
  int v;

  tm = localtime(&d->date);
  if ( notDefault(s) && (v=valInt(s)) >= 0    && v <= 59   ) tm->tm_sec  = v;
  if ( notDefault(m) && (v=valInt(m)) >= 0    && v <= 59   ) tm->tm_min  = v;
  if ( notDefault(h) && (v=valInt(h)) >= 0    && v <= 23   ) tm->tm_hour = v;
  if ( notDefault(D) && (v=valInt(D)) >= 1    && v <= 31   ) tm->tm_mday = v;
  if ( notDefault(M) && (v=valInt(M)-1) >= 0  && v <= 11   ) tm->tm_mon  = v;
  if ( notDefault(Y) && (v=valInt(Y)-1900) >= 70 && v <= 1050 ) tm->tm_year = v;
  d->date = timelocal(tm);

  succeed;
}


static status
convertDate(Date d, CharArray s)
{ if ( isstr8(&s->data) )
  { long t = get_date((char *)s->data.s_text8, NULL);

    if ( t == -1 )
      return errorPce(d, NAME_syntaxError, s);
    d->date = t;
    succeed;
  }
  
  return errorPce(d, NAME_notSupportedForChar16);
}


static status
currentDate(Date d)
{ d->date = time(0);

  succeed;
}


static status
beforeDate(Date d1, Date d2)
{ if ( d1->date < d2->date )
    succeed;
  fail;
}


static status
afterDate(Date d1, Date d2)
{ if ( d1->date > d2->date )
    succeed;
  fail;
}


static status
secondDate(Date d, Int s)
{ return setDate(d, s, DEFAULT, DEFAULT, DEFAULT, DEFAULT, DEFAULT );
}


static status
minuteDate(Date d, Int m)
{ return setDate(d, DEFAULT, m, DEFAULT, DEFAULT, DEFAULT, DEFAULT );
}


static status
hourDate(Date d, Int h)
{ return setDate(d, DEFAULT, DEFAULT, h, DEFAULT, DEFAULT, DEFAULT );
}


static status
dayDate(Date d, Int D)
{ return setDate(d, DEFAULT, DEFAULT, DEFAULT, D, DEFAULT, DEFAULT );
}


static status
monthDate(Date d, Int M)
{ return setDate(d, DEFAULT, DEFAULT, DEFAULT, DEFAULT, M, DEFAULT );
}


static status
yearDate(Date d, Int Y)
{ return setDate(d, DEFAULT, DEFAULT, DEFAULT, DEFAULT, DEFAULT, Y );
}


static Int
getSecondDate(Date d)
{ struct tm *tm = localtime(&d->date);
  answer(toInt(tm->tm_sec));
}


static Int
getMinuteDate(Date d)
{ struct tm *tm = localtime(&d->date);
  answer(toInt(tm->tm_min));
}


static Int
getHourDate(Date d)
{ struct tm *tm = localtime(&d->date);
  answer(toInt(tm->tm_hour));
}


static Int
getDayDate(Date d)
{ struct tm *tm = localtime(&d->date);
  answer(toInt(tm->tm_mday));
}


static Int
getMonthDate(Date d)
{ struct tm *tm = localtime(&d->date);
  answer(toInt(tm->tm_mon + 1));
}


static Int
getYearDate(Date d)
{ struct tm *tm = localtime(&d->date);
  answer(toInt(tm->tm_year + 1900));
}


static Int
getWeekDayDate(Date d)
{ struct tm *tm = localtime(&d->date);
  answer(toInt(tm->tm_wday));
}

static char * dayName[] = 
  { "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday"
  };
static char * shortDayName[] = 
  { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
  };
static char * monthName[] =
  { "January",	"February",	"March",	"April",	"May",
    "June",	"July",		"August",	"September",	"October",
    "November",	"December"
  };
static char * shortMonthName[] =
  { "Jan",	"Feb",	"Mar",	"Apr",	"May",
    "Jun",	"Jul",	"Aug",	"Sep",	"Oct",
    "Nov",	"Dec"
  };
			  

static Name
getDayNameDate(Date d, Bool shrt)
{ struct tm *tm = localtime(&d->date);

  answer(shrt == ON ? CtoName(shortDayName[tm->tm_wday])
		    : CtoName(dayName[tm->tm_wday]));
}


static Name
getMonthNameDate(Date d, Bool shrt)
{ struct tm *tm = localtime(&d->date);

  answer(shrt == ON ? CtoName(shortMonthName[tm->tm_mon])
	            : CtoName(monthName[tm->tm_mon]));
}


static StringObj
getStringDate(Date d)
{ char *s = ctime(&d->date);
  s[24] = '\0';

  answer(CtoString(s));
}


static Name
getCompareDate(Date d1, Date d2)
{ answer(d1->date < d2->date ? NAME_smaller :
	 d1->date > d2->date ? NAME_larger :
			       NAME_equal);
}


static Int
getDifferenceDate(Date d1, Date d2, Name units)
{ long t = isDefault(d2) ? 0 : d2->date;
  
  if ( isDefault(units) )
    units = NAME_second;

  t = d1->date - t;

  if ( units == NAME_second )
  { if ( (t > 0 && t > PCE_MAX_INT) || (t < 0 && t < PCE_MIN_INT) )
    { errorPce(d1, NAME_intRange);
      fail;
    }
    answer(toInt(t));
  } else if ( units == NAME_minute )
    answer(toInt(t/60));
  else if ( units == NAME_hour )
    answer(toInt(t/(60*60)));
  else if ( units == NAME_day )
    answer(toInt(t/(60*60*24)));
  else if ( units == NAME_week )
    answer(toInt(t/(60*60*24*7)));
  else /*if ( units == NAME_year )*/
    answer(toInt(t/(60*60*24*365)));
}




status
makeClassDate(Class class)
{ sourceClass(class, makeClassDate, __FILE__, "$Revision$");

  localClass(class, NAME_date, NAME_storage, "alien:long", NAME_none,
	     "Unix's notion of date");

  setLoadStoreFunctionClass(class, loadDate, storeDate);
  termClass(class, "date", 6, NAME_second, NAME_minute, NAME_hour,
			      NAME_day, NAME_month, NAME_year);

  sendMethod(class, NAME_initialise, DEFAULT,
	     6, "seconds=[0..59]", "minutes=[0..59]", "hours=[0..23]",
	        "day=[1..31]", "month=[1..12]", "year=[1970..2050]",
	     "Create a date from smhDMY (default now)",
	     initialiseDate);
  sendMethod(class, NAME_equal, NAME_compare, 1, "date",
	     "Test if date is equal",
	     equalDate);
  sendMethod(class, NAME_set, NAME_set,
	     6, "[0..59]", "[0..59]", "[0..23]",
	        "[1..31]", "[1..12]", "[1970..2050]",
	     "Set date from smhDMY",
	     setDate);
  sendMethod(class, NAME_before, NAME_compare, 1, "date",
	     "Test if date is before argument",
	     beforeDate);
  sendMethod(class, NAME_after, NAME_compare, 1, "date",
	     "Test if date is after argument",
	     afterDate);
  sendMethod(class, NAME_current, NAME_set, 0,
	     "Change date to be `now'",
	     currentDate);

  sendMethod(class, NAME_second, NAME_dateComponent, 1, "0..59",
	     "Set seconds",
	     secondDate);
  sendMethod(class, NAME_minute, NAME_dateComponent, 1, "0..59",
	     "Set minute",
	     minuteDate);
  sendMethod(class, NAME_hour, NAME_dateComponent, 1, "0..23",
	     "Set hour",
	     hourDate);
  sendMethod(class, NAME_day, NAME_dateComponent, 1, "1..31",
	     "Set day",
	     dayDate);
  sendMethod(class, NAME_month, NAME_dateComponent, 1, "1..12",
	     "Set month",
	     monthDate);
  sendMethod(class, NAME_year, NAME_dateComponent, 1, "1970..2050",
	     "Set year",
	     yearDate);
  sendMethod(class, NAME_convert, NAME_textual, 1, "description=char_array",
	     "Set date conform time description",
	     convertDate);

  getMethod(class, NAME_string, NAME_textual, "string", 0,
	    "New string representing date",
	    getStringDate);
  getMethod(class, NAME_printName, NAME_textual, "string", 0,
	    "Same as <-string",
	    getStringDate);
  getMethod(class, NAME_weekDay, NAME_dateComponent, "0..6", 0,
	    "Day in the week",
	    getWeekDayDate);
  getMethod(class, NAME_dayName, NAME_dateComponent, "name", 1, "short=[bool]",
	    "Name of day in the week",
	    getDayNameDate);
  getMethod(class, NAME_monthName, NAME_dateComponent, "name", 1, "short=[bool]",
	    "Name of the month",
	    getMonthNameDate);
  getMethod(class, NAME_second, NAME_dateComponent, "0..59", 0,
	    "Second in the minute",
	    getSecondDate);
  getMethod(class, NAME_minute, NAME_dateComponent, "0..59", 0,
	    "Minute in the hour",
	    getMinuteDate);
  getMethod(class, NAME_hour, NAME_dateComponent, "0..23", 0,
	    "Hour in the day",
	    getHourDate);
  getMethod(class, NAME_day, NAME_dateComponent, "1..31", 0,
	    "Day in the month",
	    getDayDate);
  getMethod(class, NAME_month, NAME_dateComponent, "1..12", 0,
	    "Month in the year",
	    getMonthDate);
  getMethod(class, NAME_year, NAME_dateComponent, "1970..2050", 0,
	    "Year of the date",
	    getYearDate);
  getMethod(class, NAME_convert, NAME_textual, "date", 1, "string",
	    "Convert Day/Month/Year to date",
	    getConvertDate);
  getMethod(class, NAME_compare, NAME_compare, "{smaller,equal,larger}",
	    1, "date",
	    "Compare two dates for `chain ->sort'",
	    getCompareDate);
  getMethod(class, NAME_difference, NAME_calculate, "units=int", 2,
	    "to=[date]", "unit=[{second,minute,hour,day,week,year}]",
	    "Difference between dates in specified units",
	    getDifferenceDate);

  succeed;
}


#ifndef HAVE_TIMELOCAL
#define MINUTE	60
#define HOUR	(60 * MINUTE)
#define DAY	(24 * HOUR)

#define leapYear(y)	 ((y % 4) && (!(y % 100) || y % 400))
static int monthsize[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

static long
timelocal(tm)
struct tm *tm;
{ register long sec;
  register int n;

  sec  = tm->tm_sec;
  sec += tm->tm_min * MINUTE;
  sec += (tm->tm_hour - 1) * HOUR;		/* why - 1 ?? */
  sec += (tm->tm_mday - 1)  * DAY;
  for( n=0; n<tm->tm_mon; n++ )
    sec += (n == 1 && leapYear(tm->tm_year) ? 29 : monthsize[n]) * DAY;
  for( n=70; n < tm->tm_year; n++ )
    sec += (leapYear(n) ? 365 : 366) * DAY;

  return sec;
}
#endif
