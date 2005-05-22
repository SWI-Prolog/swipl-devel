/*
** Copyright 1998 - 1999 Double Precision, Inc.
** See COPYING for distribution information.
*/

/*
** $Id$
*/

#include	"rfc822.h"

#include	<sys/types.h>
#include	<time.h>
#include	<stdio.h>
#include	<string.h>
#if	HAVE_UNISTD_H
#include	<unistd.h>
#endif

static const char * const months[]={
	"Jan",
	"Feb",
	"Mar",
	"Apr",
	"May",
	"Jun",
	"Jul",
	"Aug",
	"Sep",
	"Oct",
	"Nov",
	"Dec"};

static const char * const wdays[]={
	"Sun",
	"Mon",
	"Tue",
	"Wed",
	"Thu",
	"Fri",
	"Sat"};

void rfc822_mkdate_buf(time_t t, char *buf)
{
struct	tm *p;
int	offset;

#if	USE_TIME_ALTZONE

	p=localtime(&t);
	offset= -timezone;

	if (p->tm_isdst > 0)
		offset= -altzone;

	if (offset % 60)
	{
		offset=0;
		p=gmtime(&t);
	}
	offset /= 60;
#else
#if	USE_TIME_DAYLIGHT

	p=localtime(&t);
	offset= -timezone;

	if (p->tm_isdst > 0)
		offset += 60*60;
	if (offset % 60)
	{
		offset=0;
		p=gmtime(&t);
	}
	offset /= 60;
#else
#if	USE_TIME_GMTOFF
	p=localtime(&t);
	offset= p->tm_gmtoff;

	if (offset % 60)
	{
		offset=0;
		p=gmtime(&t);
	}
	offset /= 60;
#else
	p=gmtime(&t);
	offset=0;
#endif
#endif
#endif

	offset = (offset % 60) + offset / 60 * 100;

	sprintf(buf, "%s, %02d %s %04d %02d:%02d:%02d %+05d",
		wdays[p->tm_wday],
		p->tm_mday,
		months[p->tm_mon],
		p->tm_year+1900,
		p->tm_hour,
		p->tm_min,
		p->tm_sec,
		offset);
}

const char *rfc822_mkdate(time_t t)
{
static char buf[50];

	rfc822_mkdate_buf(t, buf);
	return (buf);
}
