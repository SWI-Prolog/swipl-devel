/*
** Copyright 1998 - 1999 Double Precision, Inc.  See COPYING for
** distribution information.
*/

#include	"rfc2045.h"
#include	<ctype.h>
#include	<string.h>

/* $Id$ */

static void start_rwprep(struct rfc2045 *);
static void do_rwprep(const char *, size_t);
static void end_rwprep();

static struct rfc2045ac rfc2045acprep={
	&start_rwprep,
	&do_rwprep,
	&end_rwprep};

static struct rfc2045 *currwp;
static int curlinepos=0;

typedef enum {
	raw,
	quotedprint,
	qpseeneq,
	qpseeneqh,
	base64} state_t;

static state_t curstate;
static int statechar;

#define	h2nyb(c) ( (c) >= 'a' && (c) <= 'f' ? (c)-('a'-10): \
		   (c) >= 'A' && (c) <= 'F' ? (c)-('A'-10): (c)-'0')

struct rfc2045 *rfc2045_alloc_ac()
{
struct rfc2045 *p=rfc2045_alloc();

	if (p)	p->rfc2045acptr= &rfc2045acprep;
	currwp=0;
	return (p);
}


static void start_rwprep(struct rfc2045 *p)
{
	currwp=p;
	curlinepos=0;
	curstate=raw;
	if (p->content_transfer_encoding)
	{
		if (strcmp(p->content_transfer_encoding,
			"quoted-printable") == 0)
			curstate=quotedprint;
		else if (strcmp(p->content_transfer_encoding, "base64") == 0)
			curstate=base64;
	}
}

static void do_rwprep(const char * p, size_t n)
{
	if (!currwp)	return;
	for ( ; n; --n, ++p)
		switch (curstate)	{
		case quotedprint:
			if (*p == '=')
			{
				curstate=qpseeneq;
				continue;
			}
			/* FALLTHRU */
		case raw:
			if (*p == '\r' || *p == '\n')
				curlinepos=0;
			else if (++curlinepos > 500)
				currwp->haslongline=1;
			if ((unsigned char)*p >= 127)
				currwp->has8bitchars=1;
			break;
		case qpseeneq:
			if (*p == '\n')
			{
				curstate=quotedprint;
				continue;
			}
			if (isspace((int)(unsigned char)*p))	continue; /* Ignore WSP */
			statechar=*p;
			curstate=qpseeneqh;
			continue;
		case qpseeneqh:
			curstate=quotedprint;
			if ( (unsigned char)
				( (h2nyb(statechar) << 4) + h2nyb(*p) ) >= 127
				) currwp->has8bitchars=1;
			if (++curlinepos > 500)
				currwp->haslongline=1;
			continue;
		case base64:
			break;
		}
}

static void end_rwprep()
{
}
