/*
** Copyright 1998 - 1999 Double Precision, Inc.
** See COPYING for distribution information.
*/

/*
** $Id$
*/
#include	<stdio.h>
#include	<string.h>
#include	<ctype.h>
#include	<time.h>

/*
** time_t rfc822_parsedate(const char *p)
**
** p - contents of the Date: header, attempt to parse it into a time_t.
**
** returns - time_t, or 0 if the date cannot be parsed
*/

static unsigned parsedig(const char **p)
{
unsigned i=0;

	while (isdigit((int)(unsigned char)**p))
	{
		i=i*10 + **p - '0';
		++*p;
	}
	return (i);
}

static const char * const weekdays[7]={
	"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
	} ;

static const char * const mnames[13]={
	"Jan", "Feb", "Mar", "Apr",
	"May", "Jun", "Jul", "Aug",
	"Sep", "Oct", "Nov", "Dec", NULL};

#define	leap(y)	( \
			((y) % 400) == 0 || \
			(((y) % 4) == 0 && (y) % 100) )

static unsigned mlength[]={31,28,31,30,31,30,31,31,30,31,30,31};
#define	mdays(m,y)	( (m) != 2 ? mlength[(m)-1] : leap(y) ? 29:28)

static const char * const zonenames[] = {
	"UT","GMT",
	"EST","EDT",
	"CST","CDT",
	"MST","MDT",
	"PST","PDT",
	"Z",
	"A", "B", "C", "D", "E", "F", "G", "H", "I", "K", "L", "M", 
	"N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y",
	NULL};

#define	ZH(n)	( (n) * 60 * 60 )

static int zoneoffset[] = {
	0, 0,
	ZH(-5), ZH(-4),
	ZH(-6), ZH(-5),
	ZH(-7), ZH(-6),
	ZH(-8), ZH(-7),
	0,

	ZH(-1), ZH(-2), ZH(-3), ZH(-4), ZH(-5), ZH(-6), ZH(-7), ZH(-8), ZH(-9), ZH(-10), ZH(-11), ZH(-12),
	ZH(1), ZH(2), ZH(3), ZH(4), ZH(5), ZH(6), ZH(7), ZH(8), ZH(9), ZH(10), ZH(11), ZH(12) };

static unsigned parsekey(const char **mon, const char * const *ary)
{
unsigned m, j;

	for (m=0; ary[m]; m++)
	{
		for (j=0; ary[m][j]; j++)
			if (tolower(ary[m][j]) != tolower((*mon)[j]))
				break;
		if (!ary[m][j])
		{
			*mon += j;
			return (m+1);
		}
	}
	return (0);
}

static int parsetime(const char **t)
{
unsigned h,m,s=0;

	if (!isdigit((int)(unsigned char)**t))	return (-1);
	h=parsedig(t);
	if (h > 23)		return (-1);
	if (**t != ':')		return (-1);
	++*t;
	if (!isdigit((int)(unsigned char)**t))	return (-1);
	m=parsedig(t);
	if (**t == ':')
	{
		++*t;
		if (!isdigit((int)(unsigned char)**t))	return (-1);
		s=parsedig(t);
	}
	if (m > 59 || s > 59)	return (-1);
	return (h * 60 * 60 + m * 60 + s);
}

time_t rfc822_parsedt(const char *rfcdt)
{
unsigned day=0, mon=0, year;
int secs;
int offset;
time_t t;
unsigned y;

	/* Ignore day of the week.  Tolerate "Tue, 25 Feb 1997 ... "
	** without the comma.  Tolerate "Feb 25 1997 ...".
	*/

	while (!day || !mon)
	{
		if (!*rfcdt)	return (0);
		if (isalpha((int)(unsigned char)*rfcdt))
		{
			if (mon)	return (0);
			mon=parsekey(&rfcdt, mnames);
			if (!mon)
				while (*rfcdt && isalpha((int)(unsigned char)*rfcdt))
					++rfcdt;
			continue;
		}

		if (isdigit((int)(unsigned char)*rfcdt))
		{
			if (day)	return (0);
			day=parsedig(&rfcdt);
			if (!day)	return (0);
			continue;
		}
		++rfcdt;
	}

	while (*rfcdt && isspace((int)(unsigned char)*rfcdt))
		++rfcdt;
	if (!isdigit((int)(unsigned char)*rfcdt))	return (0);
	year=parsedig(&rfcdt);
	if (year < 70)	year += 2000;
	if (year < 100)	year += 1900;

	while (*rfcdt && isspace((int)(unsigned char)*rfcdt))
		++rfcdt;

	if (day == 0 || mon == 0 || mon > 12 || day > mdays(mon,year))
		return (0);

	secs=parsetime(&rfcdt);
	if (secs < 0)	return (0);

	offset=0;

	/* RFC822 sez no parenthesis, but I've seen (EST) */

	while ( *rfcdt )
	{
		if (isalnum((int)(unsigned char)*rfcdt) || *rfcdt == '+' || *rfcdt == '-')
			break;
		++rfcdt;
	}

	if (isalpha((int)(unsigned char)*rfcdt))
	{
	int	n=parsekey(&rfcdt, zonenames);

		if (n > 0)	offset= zoneoffset[n-1];
	}
	else
	{
	int	sign=1;
	unsigned n;

		switch (*rfcdt)	{
		case '-':
			sign= -1;
		case '+':
			++rfcdt;
		}

		if (isdigit((int)(unsigned char)*rfcdt))
		{
			n=parsedig(&rfcdt);
			if (n > 2359 || (n % 100) > 59)	n=0;
			offset = sign * ( (n % 100) * 60 + n / 100 * 60 * 60);
		}
	}

	if (year < 1970)	return (0);

	t=0;
	for (y=1970; y<year; y++)
	{
		if ( leap(y) )
		{
			if (year-y >= 4)
			{
				y += 3;
				t += ( 365*3+366 ) * 24 * 60 * 60;
				continue;
			}
			t += 24 * 60 * 60;
		}
		t += 365 * 24 * 60 * 60;
	}

	for (y=1; y < mon; y++)
		t += mdays(y, year) * 24 * 60 * 60;

	return ( t + (day-1) * 24 * 60 * 60 + secs - offset );
}

const char *rfc822_mkdt(time_t t)
{
static char buf[80];
struct	tm *tmptr=gmtime(&t);

	buf[0]=0;
	if (tmptr)
	{
		sprintf(buf, "%s, %02d %s %04d %02d:%02d:%02d GMT",
			weekdays[tmptr->tm_wday],
			tmptr->tm_mday,
			mnames[tmptr->tm_mon],
			tmptr->tm_year + 1900,
			tmptr->tm_hour,
			tmptr->tm_min,
			tmptr->tm_sec);
	}
	return (buf);
}
