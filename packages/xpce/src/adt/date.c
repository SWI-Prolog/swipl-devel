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

#define unix_date date.date

Date
CtoDate(long int time)
{ Date d = newObject(ClassDate, 0);

  d->unix_date = time;

  answer(d);
}


static status
initialiseDate(Date d, Int s, Int m, Int h, Int D, Int M, Int Y)
{ d->unix_date = time(0);

  if ( notDefault(s) || notDefault(m) || notDefault(h) ||
       notDefault(D) || notDefault(M) || notDefault(Y) )
    setDate(d, s, m, h, D, M, Y);

  succeed;
}


static status
storeDate(Date d, FileObj file)
{ TRY(storeSlotsObject(d, file));

  return storeWordFile(file, (Any) d->unix_date);
}


static status
loadDate(Date d, FILE *fd, ClassDef def)
{ if ( restoreVersion != 2 )
    TRY(loadSlotsObject(d, fd, def));
  d->unix_date = loadWord(fd);

  succeed;
}


static Date
getConvertDate(Class class, StringObj str)
{ if ( isstr8(&str->data) )
  { long t = get_date((char *)str->data.s_text8, NULL);

    if ( t != -1 )
    { Date d = answerObject(ClassDate, 0);
      d->unix_date = t;

      answer(d);
    }
  }

  fail;
}


static status
equalDate(Date d1, Date d2)
{ if ( d1->unix_date == d2->unix_date )
    succeed;
  fail;
}


static status
setDate(Date d, Int s, Int m, Int h, Int D, Int M, Int Y)
{ struct tm *tm;
  int v;

  tm = localtime(&d->unix_date);
  if ( notDefault(s) && (v=valInt(s)) >= 0    && v <= 59   ) tm->tm_sec  = v;
  if ( notDefault(m) && (v=valInt(m)) >= 0    && v <= 59   ) tm->tm_min  = v;
  if ( notDefault(h) && (v=valInt(h)) >= 0    && v <= 23   ) tm->tm_hour = v;
  if ( notDefault(D) && (v=valInt(D)) >= 1    && v <= 31   ) tm->tm_mday = v;
  if ( notDefault(M) && (v=valInt(M)-1) >= 0  && v <= 11   ) tm->tm_mon  = v;
  if ( notDefault(Y) && (v=valInt(Y)-1900) >= 70 && v <= 1050 ) tm->tm_year = v;
  d->unix_date = timelocal(tm);

  succeed;
}


static status
convertDate(Date d, CharArray s)
{ if ( isstr8(&s->data) )
  { long t = get_date((char *)s->data.s_text8, NULL);

    if ( t == -1 )
      return errorPce(d, NAME_syntaxError, s);
    d->unix_date = t;
    succeed;
  }
  
  return errorPce(d, NAME_notSupportedForChar16);
}


static status
currentDate(Date d)
{ d->unix_date = time(0);

  succeed;
}


static status
copyDate(Date d, Date d2)
{ d->unix_date = d2->unix_date;

  succeed;
}


static status
beforeDate(Date d1, Date d2)
{ if ( d1->unix_date < d2->unix_date )
    succeed;
  fail;
}


static status
afterDate(Date d1, Date d2)
{ if ( d1->unix_date > d2->unix_date )
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
{ struct tm *tm = localtime(&d->unix_date);
  answer(toInt(tm->tm_sec));
}


static Int
getMinuteDate(Date d)
{ struct tm *tm = localtime(&d->unix_date);
  answer(toInt(tm->tm_min));
}


static Int
getHourDate(Date d)
{ struct tm *tm = localtime(&d->unix_date);
  answer(toInt(tm->tm_hour));
}


static Int
getDayDate(Date d)
{ struct tm *tm = localtime(&d->unix_date);
  answer(toInt(tm->tm_mday));
}


static Int
getMonthDate(Date d)
{ struct tm *tm = localtime(&d->unix_date);
  answer(toInt(tm->tm_mon + 1));
}


static Int
getYearDate(Date d)
{ struct tm *tm = localtime(&d->unix_date);
  answer(toInt(tm->tm_year + 1900));
}


static Int
getWeekDayDate(Date d)
{ struct tm *tm = localtime(&d->unix_date);
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
{ struct tm *tm = localtime(&d->unix_date);

  answer(shrt == ON ? CtoName(shortDayName[tm->tm_wday])
		    : CtoName(dayName[tm->tm_wday]));
}


static Name
getMonthNameDate(Date d, Bool shrt)
{ struct tm *tm = localtime(&d->unix_date);

  answer(shrt == ON ? CtoName(shortMonthName[tm->tm_mon])
	            : CtoName(monthName[tm->tm_mon]));
}


static StringObj
getStringDate(Date d)
{ char *s = ctime(&d->unix_date);
  s[24] = '\0';

  answer(CtoString(s));
}


static Name
getCompareDate(Date d1, Date d2)
{ answer(d1->unix_date < d2->unix_date ? NAME_smaller :
	 d1->unix_date > d2->unix_date ? NAME_larger :
					 NAME_equal);
}


static Int
getDifferenceDate(Date d1, Date d2, Name units)
{ long t = isDefault(d2) ? 0 : d2->unix_date;
  
  if ( isDefault(units) )
    units = NAME_second;

  t = d1->unix_date - t;

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


		 /*******************************
		 *	 CLASS DECLARATION	*
		 *******************************/

/* Type declaractions */

static char *T_initialise[] =
        { "seconds=[0..59]", "minutes=[0..59]",
	  "hours=[0..23]", "day=[1..31]", "month=[1..12]",
	  "year=[1970..2050]" };
static char *T_difference[] =
        { "to=[date]", "unit=[{second,minute,hour,day,week,year}]" };

/* Instance Variables */

static vardecl var_date[] =
{ IV(NAME_date, "alien:long", IV_NONE,
     NAME_storage, "Unix's notion of date")
};

/* Send Methods */

static senddecl send_date[] =
{ SM(NAME_initialise, 6, T_initialise, initialiseDate,
     DEFAULT, "Create a date from smhDMY (default now)"),
  SM(NAME_after, 1, "date", afterDate,
     NAME_compare, "Test if date is after argument"),
  SM(NAME_before, 1, "date", beforeDate,
     NAME_compare, "Test if date is before argument"),
  SM(NAME_equal, 1, "date", equalDate,
     NAME_compare, "Test if date is equal"),
  SM(NAME_day, 1, "1..31", dayDate,
     NAME_dateComponent, "Set day"),
  SM(NAME_hour, 1, "0..23", hourDate,
     NAME_dateComponent, "Set hour"),
  SM(NAME_minute, 1, "0..59", minuteDate,
     NAME_dateComponent, "Set minute"),
  SM(NAME_month, 1, "1..12", monthDate,
     NAME_dateComponent, "Set month"),
  SM(NAME_second, 1, "0..59", secondDate,
     NAME_dateComponent, "Set seconds"),
  SM(NAME_year, 1, "1970..2050", yearDate,
     NAME_dateComponent, "Set year"),
  SM(NAME_current, 0, NULL, currentDate,
     NAME_set, "Change date to be `now'"),
  SM(NAME_copy, 1, "date", copyDate,
     NAME_set, "Copy time from argment date object"),
  SM(NAME_set, 6, T_initialise, setDate,
     NAME_set, "Set date from smhDMY"),
  SM(NAME_convert, 1, "description=char_array", convertDate,
     NAME_textual, "Set date conform time description")
};

/* Get Methods */

static getdecl doget_date[] =
{ GM(NAME_difference, 2, "units=int", T_difference, getDifferenceDate,
     NAME_calculate, "Difference between dates in specified units"),
  GM(NAME_compare, 1, "{smaller,equal,larger}", "date", getCompareDate,
     NAME_compare, "Compare two dates for `chain ->sort'"),
  GM(NAME_day, 0, "1..31", NULL, getDayDate,
     NAME_dateComponent, "Day in the month"),
  GM(NAME_dayName, 1, "name", "short=[bool]", getDayNameDate,
     NAME_dateComponent, "Name of day in the week"),
  GM(NAME_hour, 0, "0..23", NULL, getHourDate,
     NAME_dateComponent, "Hour in the day"),
  GM(NAME_minute, 0, "0..59", NULL, getMinuteDate,
     NAME_dateComponent, "Minute in the hour"),
  GM(NAME_month, 0, "1..12", NULL, getMonthDate,
     NAME_dateComponent, "Month in the year"),
  GM(NAME_monthName, 1, "name", "short=[bool]", getMonthNameDate,
     NAME_dateComponent, "Name of the month"),
  GM(NAME_second, 0, "0..59", NULL, getSecondDate,
     NAME_dateComponent, "Second in the minute"),
  GM(NAME_weekDay, 0, "0..6", NULL, getWeekDayDate,
     NAME_dateComponent, "Day in the week"),
  GM(NAME_year, 0, "1970..2050", NULL, getYearDate,
     NAME_dateComponent, "Year of the date"),
  GM(NAME_convert, 1, "date", "string", getConvertDate,
     NAME_textual, "Convert Day/Month/Year to date"),
  GM(NAME_printName, 0, "string", NULL, getStringDate,
     NAME_textual, "Same as <-string"),
  GM(NAME_string, 0, "string", NULL, getStringDate,
     NAME_textual, "New string representing date")
};

/* Resources */

#define rc_date NULL
/*
static resourcedecl rc_date[] =
{ 
};
*/

/* Class Declaration */

static Name date_termnames[] = { NAME_second, NAME_minute, NAME_hour,
				 NAME_day, NAME_month, NAME_year };

ClassDecl(date_decls,
          var_date, send_date, doget_date, rc_date,
          6, date_termnames,
          "$Rev$");



status
makeClassDate(Class class)
{ assert(sizeof(time_t) <= sizeof(Any));
  declareClass(class, &date_decls);

  setLoadStoreFunctionClass(class, loadDate, storeDate);

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
