/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
			      VU University Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#define __MINGW_USE_VC2005_COMPAT		/* Get Windows time_t as 64-bit */

#include <math.h>
#include "pl-incl.h"
#include "libtai/taia.h"
#include "libtai/caltime.h"
#include <stdio.h>
#include <ctype.h>

#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

#if defined(__WINDOWS__) || defined (__CYGWIN__)
#define timezone _timezone
#ifndef HAVE_VAR_TIMEZONE
#define HAVE_VAR_TIMEZONE
#endif
#else
extern char *tzname[2];
#ifdef HAVE_VAR_TIMEZONE
extern long timezone;
#endif
#endif

#define TAI_UTC_OFFSET LL(4611686018427387914)

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
struct ftm is a `floating' version of the system struct tm.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define		HAS_STAMP	0x0001
#define		HAS_WYDAY	0x0002

#define		NO_UTC_OFFSET	0x7fffffff

typedef struct ftm
{ struct	tm tm;			/* System time structure */
  double	sec;			/* float version of tm.tm_sec */
  int		utcoff;			/* offset to UTC (seconds) */
  atom_t	tzname;			/* Name of timezone */
  int		isdst;			/* Daylight saving time */
  double	stamp;			/* Time stamp (sec since 1970-1-1) */
  int		flags;			/* Filled fields */
} ftm;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
tz_offset() returns the offset from UTC in seconds.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
do_tzset(void)
{ static int done = FALSE;

  if ( !done )
  { tzset();
    done = TRUE;
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
POSIX provides the variable  timezone,  providing   the  offset  of  the
current timezone WEST of GMT in seconds.   Some systems (FreeBSD) do not
provide that. Instead thet provide  tm_gmtoff   in  struct  tm, but this
value is EAST and includes the DST offset.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
tz_offset(void)
{
#ifdef HAVE_VAR_TIMEZONE
  do_tzset();
  return timezone;
#else
#ifdef HAVE_STRUCT_TIME_TM_GMTOFF
  static int offset = -1;
  if ( offset == -1 )
  { time_t t = time(NULL);
    struct tm tm;

    PL_localtime_r(&t, &tm);

    offset = -tm.tm_gmtoff;
    if ( tm.tm_isdst > 0 )
      offset += 3600;
  /*Use to verify on systems where we know both.  In Western Europe the
    offset must be -3600, both in winter and summer.*/
  /*Sdprintf("timezone offset = %d (must be %d)\n", offset, timezone);*/
  }
  return offset;
#else
#error "Do not know how to get timezone info"
#endif
#endif
}


static char *
tz_name(int dst)
{ dst = (dst != 0);

  do_tzset();
  return tzname[dst];
}


static atom_t
tz_name_as_atom(int dst)
{ static atom_t a[2];

  dst = (dst > 0);			/* 0 or 1 */

  if ( !a[dst] )
  { wchar_t wbuf[256];
    const char *str = tz_name(dst);
    size_t n;

    if ( (n = mbstowcs(wbuf, str, sizeof(wbuf)/sizeof(wbuf[0])-1)) != (size_t)-1 )
    { a[dst] = PL_new_atom_wchars(n, wbuf);
    } else
    { a[dst] = PL_new_atom(str);
    }
  }

  return a[dst];
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
unify_taia(): Unify a TAIA date as a Prolog double using the POSIX 1970
origin;
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
static int
unify_taia(term_t t, struct taia *taia)
{ double d = (double)((int64_t)taia->sec.x - TAI_UTC_OFFSET);

  d += taia->nano / 1e9;

  return PL_unify_float(t, d);
}
*/


static int
get_taia(term_t t, struct taia *taia, double *seconds)
{ double d;

  if ( PL_get_float(t, &d) )
  { double fp, ip;

    if ( seconds )
      *seconds = d;

    fp = modf(d, &ip);
    if ( fp < 0 )
    { fp += 1.0;
      ip -= 1.0;
    }

    taia->sec.x = (int64_t)ip + TAI_UTC_OFFSET;
    taia->nano  = (long)(fp*1e9);
    taia->atto  = 0L;

    return TRUE;
  }

  return FALSE;
}


static int
get_tz_arg(int i, term_t t, term_t a, atom_t *tz)
{ GET_LD
  atom_t name;

  _PL_get_arg(i, t, a);
  if ( !PL_is_variable(a) )
  { if ( !PL_get_atom_ex(a, &name) )
      fail;
    if ( name != ATOM_minus )
      *tz = name;
  }

  succeed;
}


static int
get_int_arg(int i, term_t t, term_t a, int *val)
{ GET_LD

  _PL_get_arg(i, t, a);

  return PL_get_integer_ex(a, val);
}


static int
get_voff_arg(int i, term_t t, term_t a, int *val)
{ GET_LD

  _PL_get_arg(i, t, a);

  if ( PL_is_variable(a) )
  { *val = NO_UTC_OFFSET;
    return TRUE;
  } else
  { return PL_get_integer_ex(a, val);
  }
}


static int
get_float_arg(int i, term_t t, term_t a, double *val)
{ GET_LD

  _PL_get_arg(i, t, a);

  return PL_get_float_ex(a, val);
}


static int
get_dst_arg(int i, term_t t, term_t a, int *val)
{ GET_LD
  atom_t name;

  _PL_get_arg(i, t, a);
  if ( PL_get_atom(a, &name) )
  { if ( name == ATOM_true )
    { *val = TRUE;
      return TRUE;
    } else if ( name == ATOM_false )
    { *val = FALSE;
      return TRUE;
    } else if ( name == ATOM_minus )
    { *val = -1;
      return TRUE;
    }
  } else if ( PL_is_variable(a) )
  { *val = -2;
    return TRUE;
  }

  return PL_get_bool_ex(a, val);	/* generate an error */
}


static int
get_ftm(term_t t, ftm *ftm)
{ GET_LD
  term_t tmp = PL_new_term_ref();
  int date9;

  memset(ftm, 0, sizeof(*ftm));

  if ( (date9=PL_is_functor(t, FUNCTOR_date9)) )
  { if ( get_int_arg  (1, t, tmp, &ftm->tm.tm_year) &&
	 get_int_arg  (2, t, tmp, &ftm->tm.tm_mon)  &&
	 get_int_arg  (3, t, tmp, &ftm->tm.tm_mday) &&
	 get_int_arg  (4, t, tmp, &ftm->tm.tm_hour) &&
	 get_int_arg  (5, t, tmp, &ftm->tm.tm_min)  &&
	 get_float_arg(6, t, tmp, &ftm->sec)	    &&
	 get_voff_arg (7, t, tmp, &ftm->utcoff)     &&
	 get_tz_arg   (8, t, tmp, &ftm->tzname)     &&
	 get_dst_arg  (9, t, tmp, &ftm->isdst) )
    { double fp, ip;

      ftm->tm.tm_isdst = (ftm->isdst == -2 ? -1 : ftm->isdst);

    fixup:
      fp = modf(ftm->sec, &ip);
      if ( fp < 0.0 )
      { fp += 1.0;
	ip -= 1.0;
      }

      ftm->tm.tm_sec = (int)ip;
      ftm->tm.tm_year -= 1900;		/* 1900 based */
      ftm->tm.tm_mon--;			/* 0-based */

      if ( ftm->utcoff == NO_UTC_OFFSET )
      { if ( ftm->tm.tm_isdst < 0 )	/* unknown DST */
	{ int offset;

	  if ( mktime(&ftm->tm) == (time_t)-1 )
	    return PL_representation_error("dst");
	  ftm->flags |= HAS_WYDAY;

	  offset = tz_offset();
	  if ( ftm->tm.tm_isdst > 0 )
	    offset -= 3600;
	  ftm->utcoff = offset;

	  if ( date9 ) /* variable */
	  { _PL_get_arg(7, t, tmp);
	    if ( !PL_unify_integer(tmp, ftm->utcoff) )
	      return FALSE;
	  } else
	  { ftm->utcoff = offset;
	  }
	}

	if ( ftm->isdst == -2 )
	{ ftm->isdst = ftm->tm.tm_isdst;
	  _PL_get_arg(9, t, tmp);
	  if ( ftm->isdst < 0 )
	  { if ( !PL_unify_atom(tmp, ATOM_minus) )
	      return FALSE;
	  } else
	  { if ( !PL_unify_bool(tmp, ftm->isdst) )
	      return FALSE;
	  }
	}

	if ( !ftm->tzname )
	{ ftm->tzname = tz_name_as_atom(ftm->isdst);
	  _PL_get_arg(8, t, tmp);
	  if ( PL_is_variable(tmp) &&
	       !PL_unify_atom(tmp, ftm->tzname) )
	    return FALSE;
	}
      }

      succeed;
    }
  } else if ( PL_is_functor(t, FUNCTOR_date3) )
  { if ( get_int_arg  (1, t, tmp, &ftm->tm.tm_year) &&
	 get_int_arg  (2, t, tmp, &ftm->tm.tm_mon)  &&
	 get_int_arg  (3, t, tmp, &ftm->tm.tm_mday) )
    { ftm->tm.tm_isdst = -1;
      ftm->utcoff = NO_UTC_OFFSET;
      goto fixup;
    }
  }

  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_time, t);
}


/** void cal_ftm(ftm *ftm, int required)
    compute missing fields from fmt
*/

static void
cal_ftm(ftm *ftm, int required)
{ int missing = ftm->flags^required;

  if ( missing )			/* we need something, so we always */
  { struct caltime ct;			/* need the stamp */
    struct tai tai;

    ct.date.year  = ftm->tm.tm_year+1900;
    ct.date.month = ftm->tm.tm_mon+1;
    ct.date.day   = ftm->tm.tm_mday;
    ct.hour       = ftm->tm.tm_hour;
    ct.minute     = ftm->tm.tm_min;
    ct.second     = ftm->tm.tm_sec;
    ct.offset     = -ftm->utcoff / 60;	/* TBD: make libtai speak seconds */

    caltime_tai(&ct, &tai);
    ftm->stamp  = (double)((int64_t)tai.x - TAI_UTC_OFFSET);
    ftm->stamp -= (double)ct.second;
    ftm->stamp += ftm->sec;
    ftm->flags |= HAS_STAMP;

    if ( missing & HAS_WYDAY )
    { caltime_utc(&ct, &tai, &ftm->tm.tm_wday, &ftm->tm.tm_yday);
      ftm->flags |= HAS_WYDAY;
    }
  }
}


static
PRED_IMPL("stamp_date_time", 3, stamp_date_time, 0)
{ PRED_LD
  struct taia taia;
  term_t compound = A2;
  double argsec;

  if ( get_taia(A1, &taia, &argsec) )
  { struct caltime ct;
    int weekday, yearday;
    double sec;
    int utcoffset;
    int done = FALSE;
    atom_t alocal;
    atom_t tzatom = ATOM_minus;
    atom_t dstatom = ATOM_minus;

    if ( PL_get_atom(A3, &alocal) )
    { if ( alocal == ATOM_local )
      { time_t unixt;
	int64_t ut64;
	struct tm tm;

	utcoffset = tz_offset();

	ut64 = taia.sec.x - TAI_UTC_OFFSET;
	unixt = (time_t) ut64;

	if ( (int64_t)unixt == ut64 )
	{ double ip;

	  PL_localtime_r(&unixt, &tm);
	  sec = (double)tm.tm_sec + modf(argsec, &ip);
	  ct.date.year  = tm.tm_year+1900;
	  ct.date.month = tm.tm_mon+1;
	  ct.date.day   = tm.tm_mday;
	  ct.hour       = tm.tm_hour;
	  ct.minute     = tm.tm_min;
	  tzatom = tz_name_as_atom(tm.tm_isdst);
	  if ( tm.tm_isdst > 0 )
	  { utcoffset -= 3600;
	    dstatom    = ATOM_true;
	  } else
	  { dstatom    = ATOM_false;
	  }
	  done = TRUE;
	}
      } else if ( alocal == ATOM_utc )
      { utcoffset = 0;
	tzatom = alocal;
      } else
      { return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_timezone, A3);
      }
    } else if ( !PL_get_integer_ex(A3, &utcoffset) )
    { fail;
    }

    if ( !done )
    { taia.sec.x -= utcoffset;
      caltime_utc(&ct, &taia.sec, &weekday, &yearday);
      sec = (double)ct.second+(double)taia.nano/1e9;
    }

    return PL_unify_term(compound,
			 PL_FUNCTOR, FUNCTOR_date9,
			   PL_LONG,  ct.date.year,
			   PL_INT,   ct.date.month,
			   PL_INT,   ct.date.day,
			   PL_INT,   ct.hour,
			   PL_INT,   ct.minute,
			   PL_FLOAT, sec,
			   PL_INT,   utcoffset,
			   PL_ATOM,  tzatom,
			   PL_ATOM,  dstatom);
  }

					/* time_stamp */
  return PL_error(NULL, 0, NULL, ERR_TYPE, ATOM_float, A1);
}


static
PRED_IMPL("date_time_stamp", 2, date_time_stamp, 0)
{ ftm ftm;

  if ( !get_ftm(A1, &ftm) )
    fail;
  cal_ftm(&ftm, HAS_STAMP);

  return PL_unify_float(A2, ftm.stamp);
}


		 /*******************************
		 *	  GLIBC FUNCTIONS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
These functions support strftime() %g, %G and   %V.  Code is copied from
glibc 2.3.5. As Glibc is LGPL, there are no license issues.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifndef __isleap
/* Nonzero if YEAR is a leap year (every 4 years,
   except every 100th isn't, and every 400th is).  */
# define __isleap(year)	\
  ((year) % 4 == 0 && ((year) % 100 != 0 || (year) % 400 == 0))
#endif

/* The number of days from the first day of the first ISO week of this
   year to the year day YDAY with week day WDAY.  ISO weeks start on
   Monday; the first ISO week has the year's first Thursday.  YDAY may
   be as small as YDAY_MINIMUM.  */
#define ISO_WEEK_START_WDAY 1 /* Monday */
#define ISO_WEEK1_WDAY 4 /* Thursday */
#define YDAY_MINIMUM (-366)
#ifdef __GNUC__
__inline__
#endif
static int
iso_week_days(int yday, int wday)
{ /* Add enough to the first operand of % to make it nonnegative.  */
  int big_enough_multiple_of_7 = (-YDAY_MINIMUM / 7 + 2) * 7;
  return (yday
	  - (yday - wday + ISO_WEEK1_WDAY + big_enough_multiple_of_7) % 7
	  + ISO_WEEK1_WDAY - ISO_WEEK_START_WDAY);
}


		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static int
fmt_domain_error(const char *key, int value)
{ GET_LD
  term_t t = PL_new_term_ref();

  PL_put_integer(t, value);

  return PL_error(NULL, 0, NULL, ERR_DOMAIN, PL_new_atom(key), t);
}

static int
fmt_not_implemented(int c)
{ GET_LD
  term_t t = PL_new_term_ref();
  char key[3];

  key[0] = '%';
  key[1] = c;
  key[2] = 0;

  PL_put_atom_chars(t, key);

  return PL_error(NULL, 0, NULL, ERR_EXISTENCE, PL_new_atom("format"), t);
}


		 /*******************************
		 *	    FORMATTING		*
		 *******************************/

#define OUT1DIGIT(fd, val) \
	{ Sputcode('0'+(val)%10, fd); \
	}
#define OUT2DIGITS(fd, val) \
	{ Sputcode('0'+((val)/10)%10, fd); \
	  Sputcode('0'+(val)%10, fd); \
	}
#define OUT3DIGITS(fd, val) \
	{ Sputcode('0'+((val)/100)%10, fd); \
	  Sputcode('0'+((val)/10)%10, fd); \
	  Sputcode('0'+(val)%10, fd); \
	}
#define OUT2DIGITS_SPC(fd, val) \
	{ Sputcode(((val)/10 == 0 ? ' ' : '0'+((val)/10)%10), fd); \
	  Sputcode('0'+(val)%10, fd); \
	}
#define OUTNUMBER(fd, fmt, val) \
	{ Sfprintf(fd, fmt, val); \
	}
#define SUBFORMAT(f) \
	{ format_time(fd, f, ftm, posix); \
	}
#define OUTCHR(fd, c) \
	{ Sputcode(c, fd); \
	}
#define OUTSTR(str) \
	{ Sfputs(str, fd); \
	}
#define OUTSTRA(str) \
	{ foutstra(str, fd); \
	}
#define OUTATOM(a) \
	{ writeAtomToStream(fd, a); \
	}

static void
foutstra(const char *str, IOSTREAM *fd)
{ wchar_t wbuf[256];
  size_t n;

  if ( (n = mbstowcs(wbuf, str, sizeof(wbuf)/sizeof(wbuf[0])-1)) != (size_t)-1 )
  { wchar_t *p;

    for(p=wbuf; n-- > 0; p++)
      Sputcode(*p, fd);
  }
}


static const char *abbred_weekday[] =
{ "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
static const char *weekday[] =
{ "Sunday", "Monday", "Tuesday", "Wednesday",
  "Thursday", "Friday", "Saturday" };
static const char *abbred_month[] =
{ "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};
static const char *month[] =
{ "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
};

#define NOARG (-1)

static int
format_time(IOSTREAM *fd, const wchar_t *format, ftm *ftm, int posix)
{ wint_t c;

  while((c = *format++))
  { int arg = NOARG;
    int altO = FALSE;

    switch(c)
    { case '%':
	arg = NOARG;
      fmt_next:
	switch((c = *format++))
	{ case 'a':			/* %a: abbreviated weekday */
	  case 'A':			/* %A: weekday */
	  case 'b':			/* %b: abbreviated month */
	  case 'B':			/* %B: month */
	    if ( posix )
	    { const char *s;
	      cal_ftm(ftm, HAS_STAMP|HAS_WYDAY);

	      switch( c )
	      { case 'a':
		  s = abbred_weekday[ftm->tm.tm_wday];
		  break;
		case 'A':
		  s = weekday[ftm->tm.tm_wday];
		  break;
		case 'b':
		  s = abbred_month[ftm->tm.tm_mon];
		  break;
		case 'B':
		  s = month[ftm->tm.tm_mon];
		  break;
		default:
		  s = NULL;
		  assert(0);
	      }
	      OUTSTR(s);
	      break;
	    }
	    /*FALLTHROUGH*/
	  case 'c':			/* %c: default representation */
	  case 'p':			/* %p: AM/PM (locale) */
	  case 'P':			/* %P: am/pm (locale) */
	  case 'x':			/* %x: date in locale */
	  case 'X':			/* %X: time in locale */
	  case_b:
	  { char fmt[3];
	    char buf[256];

	    fmt[0] = '%';
	    fmt[1] = (char)c;
	    fmt[2] = EOS;

	    cal_ftm(ftm, HAS_STAMP|HAS_WYDAY);
					/* conversion is not thread-safe under locale switch */
	    strftime(buf, sizeof(buf), fmt, &ftm->tm);
	    OUTSTRA(buf);
	    break;
	  }
	  case 'C':			/* (year/100) as a 2-digit int */
	  { int year = ftm->tm.tm_year+1900;

	    if ( year >= 0 && year < 10000 )
	    { int century = year/100;
	      OUT2DIGITS(fd, century);
	    } else
	    { return fmt_domain_error("%C", year);
	    }
	    break;
	  }
	  case 'd':			/* day of the month */
	    OUT2DIGITS(fd, ftm->tm.tm_mday);
	    break;
	  case 'D':			/* %m/%d/%y */
	    SUBFORMAT(L"%m/%d/%y");
	    break;
	  case 'e':			/* day of the month */
	    OUT2DIGITS_SPC(fd, ftm->tm.tm_mday);
	    break;
	  case 'E':			/* alternative format */
	    return fmt_not_implemented(c);
	  case 'F':			/* ISO 8601 date format */
	    SUBFORMAT(L"%Y-%m-%d");
	    break;
	  case 'G':
	  case 'g':
	  case 'V':
	  { int year, days;

	    cal_ftm(ftm, HAS_STAMP|HAS_WYDAY);
	    year = ftm->tm.tm_year+1900;
	    days = iso_week_days(ftm->tm.tm_yday, ftm->tm.tm_wday);

	    if ( days < 0 )
	    { year--;
	      days = iso_week_days(ftm->tm.tm_yday + (365 + __isleap (year)),
				   ftm->tm.tm_wday);
	    } else
	    { int d = iso_week_days(ftm->tm.tm_yday - (365 + __isleap (year)),
				    ftm->tm.tm_wday);
	      if (0 <= d)
	      { /* This ISO week belongs to the next year.  */
		year++;
		days = d;
	      }
	    }

	    switch(c)
	    { case 'g':
		OUT2DIGITS(fd, (year % 100 + 100) % 100);
		break;
	      case 'G':
		OUTNUMBER(fd, "%d", year);
		break;
	      case 'V':
		OUT2DIGITS(fd, days/7+1);
		break;
	    }
	    break;
	  }
	  case 'h':			/* Equivalent to %b. (SU) */
	    c = 'b';
	    goto case_b;
	  case 'H':			/* 0..23 hours */
	    OUT2DIGITS(fd, ftm->tm.tm_hour);
	    break;
	  case 'I':			/* 01..12 hours */
	    OUT2DIGITS(fd, (ftm->tm.tm_hour)%12);
	    break;
	  case 'j':			/* yday (001..366) */
	    cal_ftm(ftm, HAS_WYDAY);
	    OUT3DIGITS(fd, ftm->tm.tm_yday+1);
	    break;
	  case 'k':			/* 0..23 hours (leading space) */
	    OUT2DIGITS_SPC(fd, ftm->tm.tm_hour);
	    break;
	  case 'l':			/* 1..12 hours (leading space) */
	    OUT2DIGITS_SPC(fd, (ftm->tm.tm_hour)%12);
	    break;
	  case 'm':			/* 01..12 month  */
	    OUT2DIGITS(fd, ftm->tm.tm_mon+1);
	    break;
	  case 'M':			/* 00..59 minute  */
	    OUT2DIGITS(fd, ftm->tm.tm_min);
	    break;
	  case 'n':			/* newline */
	    OUTCHR(fd, '\n');
	    break;
	  case 'O':
	  case ':':
	    if ( format[0] == 'z' )
	    { altO = TRUE;
	      goto fmt_next;
	    }

	    return fmt_not_implemented(c);
	  case 'r':			/* The  time in a.m./p.m. notation */
	    SUBFORMAT(L"%I:%M:%S %p");	/* TBD: :-separator locale handling */
	    break;
	  case 'R':
	    SUBFORMAT(L"%H:%M");
	    break;
	  case 'f':			/* Microseconds */
	  { int digits = (arg == NOARG ? 6 : arg);

	    if ( digits > 0 )
	    { double ip;
	      char fmt[64];

	      cal_ftm(ftm, HAS_STAMP);
	      Ssprintf(fmt, "%%0%dlld", digits);
	      OUTNUMBER(fd, fmt, (long)(modf(ftm->stamp, &ip) *
					pow(10, digits)));
	    }
	    break;
	  }
	  case 's':			/* Seconds since 1970 */
	    cal_ftm(ftm, HAS_STAMP);
	    OUTNUMBER(fd, "%.0f", ftm->stamp);
	    break;
	  case 'S':			/* Seconds */
	    OUT2DIGITS(fd, ftm->tm.tm_sec);
	    break;
	  case 't':			/* tab */
	    OUTCHR(fd, '\t');
	    break;
	  case 'T':
	    SUBFORMAT(L"%H:%M:%S");
	    break;
	  case 'u':			/* 1..7 weekday, mon=1 */
	  { int wday;

	    cal_ftm(ftm, HAS_WYDAY);
	    wday = (ftm->tm.tm_wday - 1 + 7) % 7 + 1;
	    OUT1DIGIT(fd, wday);
	    break;
	  }
	  case 'U':			/* 00..53 weeknumber */
	  { int wk;

	    cal_ftm(ftm, HAS_WYDAY);
	    wk = (ftm->tm.tm_yday - (ftm->tm.tm_yday - ftm->tm.tm_wday + 7) % 7 + 7) / 7;
	    OUT2DIGITS(fd, wk);
	    break;
	  }
	  case 'w':			/* 0..6 weekday */
	    cal_ftm(ftm, HAS_WYDAY);
	    OUT1DIGIT(fd, ftm->tm.tm_wday);
	    break;
	  case 'W':			/* 00..53 monday-based week number */
	  { int wk;

	    cal_ftm(ftm, HAS_WYDAY);
	    wk = (ftm->tm.tm_yday - (ftm->tm.tm_yday - ftm->tm.tm_wday + 8) % 7 + 7) / 7;
	    OUT2DIGITS(fd, wk);
	    break;
	  }
	  case 'y':			/* 00..99 (year) */
	    OUT2DIGITS(fd, (ftm->tm.tm_year+1900) % 100);
	    break;
	  case 'Y':			/* Year (decimal) */
	    OUTNUMBER(fd, "%d", ftm->tm.tm_year+1900);
	    break;
	  case 'z':			/* Time-zone as offset */
	  { int min = -ftm->utcoff/60;

	    if ( min >= 0 )
	    { OUTCHR(fd, '+');
	    } else
	    { min = -min;
	      OUTCHR(fd, '-');
	    }
	    OUT2DIGITS(fd, min/60);
	    if ( altO )
	      OUTCHR(fd, ':');
	    OUT2DIGITS(fd, min%60);
	    break;
	  }
	  case 'Z':			/* Time-zone as name */
	    if ( ftm->tzname )
	    { OUTATOM(ftm->tzname);
	    } else
	    { OUTSTRA(tz_name(ftm->tm.tm_isdst));
	    }
	    break;
	  case '+':
	    { char buf[26];

	      cal_ftm(ftm, HAS_WYDAY);
	      PL_asctime_r(&ftm->tm, buf);
	      buf[24] = EOS;
	      OUTSTRA(buf);
	    }
	    break;
	  case '%':
	    OUTCHR(fd, '%');
	    break;
	  default:
	    if ( isdigit(c) )
	    { if ( arg == NOARG )
		arg = c - '0';
	      else
		arg = arg*10+(c-'0');
	      goto fmt_next;
	    } else
	    { return fmt_not_implemented(c);
	    }
	}
        break;
      default:
	OUTCHR(fd, c);
    }
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
format_time(+Spec, +Format, +Stamp)

Issues:
	* Localtime/DST
	* Year is an int (not so bad)
	* Portability
	* Sub-second times
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static  foreign_t
pl_format_time(term_t out, term_t format, term_t time, int posix)
{ struct taia taia;
  struct caltime ct;
  struct ftm tb;
  int weekday, yearday;
  wchar_t *fmt;
  time_t unixt;
  int64_t ut64;
  size_t fmtlen;
  redir_context ctx;

  if ( !PL_get_wchars(format, &fmtlen, &fmt,
		      CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
    fail;

  memset(&tb, 0, sizeof(tb));
  if ( get_taia(time, &taia, &tb.stamp) )
  { double ip;

    ut64 = taia.sec.x - TAI_UTC_OFFSET;
    unixt = (time_t) ut64;

    if ( (int64_t)unixt == ut64 )
    { tb.utcoff = tz_offset();
      PL_localtime_r(&unixt, &tb.tm);
      tb.sec = (double)tb.tm.tm_sec + modf(tb.stamp, &ip);
      if ( tb.tm.tm_isdst > 0 )
      { tb.utcoff -= 3600;
	tb.isdst = TRUE;
      }
      tb.tzname = tz_name_as_atom(tb.tm.tm_isdst);
      tb.flags  = HAS_STAMP|HAS_WYDAY;
    } else
    { caltime_utc(&ct, &taia.sec, &weekday, &yearday);
      tb.tm.tm_sec  = ct.second;
      tb.tm.tm_min  = ct.minute;
      tb.tm.tm_hour = ct.hour;
      tb.tm.tm_mday = ct.date.day;
      tb.tm.tm_mon  = ct.date.month - 1;
      tb.tm.tm_year = ct.date.year - 1900;
      tb.tm.tm_wday = weekday;
      tb.tm.tm_yday = yearday;
      tb.tzname     = ATOM_utc;
      tb.utcoff     = 0;
    }
  } else if ( !get_ftm(time, &tb) )
  { return FALSE;
  }

  if ( !setupOutputRedirect(out, &ctx, FALSE) )
    fail;
  if ( format_time(ctx.stream, fmt, &tb, posix) )
    return closeOutputRedirect(&ctx);	/* takes care of I/O errors */

  discardOutputRedirect(&ctx);
  fail;
}

static
PRED_IMPL("format_time", 3, format_time3, 0)
{ return pl_format_time(A1, A2, A3, FALSE);
}

static
PRED_IMPL("format_time", 4, format_time4, 0)
{ PRED_LD
  int posix = FALSE;
  atom_t locale;

  if ( !PL_get_atom_ex(A4, &locale) )
    return FALSE;
  if ( locale == ATOM_posix )
    posix = TRUE;
  else
    return PL_error(NULL, 0, NULL, ERR_DOMAIN, ATOM_locale, A4);

  return pl_format_time(A1, A2, A3, posix);
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/


BeginPredDefs(tai)
  PRED_DEF("stamp_date_time", 3, stamp_date_time, 0)
  PRED_DEF("date_time_stamp", 2, date_time_stamp, 0)
  PRED_DEF("format_time",     3, format_time3,    0)
  PRED_DEF("format_time",     4, format_time4,    0)
EndPredDefs
