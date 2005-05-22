/*
** Copyright 1998 - 1999 Double Precision, Inc.  See COPYING for
** distribution information.
*/

#if	HAVE_CONFIG_H
#include	"config.h"
#endif
#include	"rfc2045.h"
#if	HAVE_UNISTD_H
#include	<unistd.h>
#endif
#include	<stdio.h>
#include	<stdlib.h>
#include	<time.h>
#ifdef WIN32
#define NUMBUFSIZE 60
#define HAS_GETHOSTNAME 1
#include <windows.h>
#else
#define NUMBUFSIZE 60
#endif

/* $Id$ */

#if	HAS_GETHOSTNAME

#else

extern int gethostname(char *, size_t);
#endif

extern void rfc2045_enomem();

char *rfc2045_mk_boundary(struct rfc2045 *s, int fd)
{
char	pidbuf[NUMBUFSIZE];
char	timebuf[NUMBUFSIZE];
char	cntbuf[60];
int	cnt=0;
time_t	mytime;
#ifndef WIN32
char	hostnamebuf[256];
pid_t	mypid;
#endif
char	*p;
int	rc;

	time(&mytime);

#ifdef WIN32
	sprintf(pidbuf, "%ld", GetCurrentThreadId());
	sprintf(timebuf, "%ld", (long)mytime);
#else
	hostnamebuf[sizeof(hostnamebuf)-1]=0;
	if (gethostname(hostnamebuf, sizeof(hostnamebuf)-1))
		hostnamebuf[0]=0;
	mypid=getpid();
	sprintf(pidbuf, "%d", mypid);
	sprintf(timebuf, "%ld", mytime);
#endif

	for (;;)
	{
		sprintf(cntbuf, "%d", ++cnt);
		p=malloc(strlen(pidbuf)+strlen(timebuf)+
			strlen(cntbuf)+10);
		if (!p)
		{
			rfc2045_enomem();
			return (NULL);
		}

		sprintf(p, "=_%s-%s-%s", pidbuf, timebuf, cntbuf);
		if ((rc=rfc2045_try_boundary(s, fd, p)) == 0)
			break;
		free(p);
		if (rc < 0)
			return (NULL);
	}
	return (p);
}
