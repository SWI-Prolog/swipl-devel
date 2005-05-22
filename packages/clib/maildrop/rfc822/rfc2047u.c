/*
** Copyright 1998 - 2000 Double Precision, Inc.  See COPYING for
** distribution information.
*/


#include	<stdio.h>
#include	<ctype.h>
#include	<string.h>
#include	<stdlib.h>
#include	<errno.h>

#include	"rfc822.h"
#include	"rfc2047.h"

static const char rcsid[]="$Id$";

#if HAVE_LIBUNICODE

#include "../unicode/unicode.h"

struct decode_unicode_s {
	const struct unicode_info *mychset;
	int options;

	char *bufptr;
	size_t bufsize;
} ;

static void save_unicode_text(const char *p, int l, struct decode_unicode_s *s)
{
	if (s->bufptr)
		memcpy(s->bufptr+s->bufsize, p, l);

	s->bufsize += l;
}

static int save_unicode(const char *txt, int len, const char *chset,
		void *arg)
{
	struct decode_unicode_s *p=(struct decode_unicode_s *)arg;
	char *txts=malloc(len+1);
	char *s;

	if (!txts)
		return (-1);
	memcpy(txts, txt, len);
	txts[len]=0;

	if (!chset)
		chset=unicode_ISO8859_1.chset;

	s=unicode_convert_fromchset(txts, chset, p->mychset);
	free(txts);
	if (s)
	{
		save_unicode_text(s, strlen(s), p);
		free(s);
		return (0);
	}

	if (p->options & RFC2047_DECODE_ABORT)
	{
		errno=EINVAL;
		return (-1);
	}

	if (p->options & RFC2047_DECODE_DISCARD)
		return (0);

	save_unicode_text(" [", 2, p);
	save_unicode_text(chset, strlen(chset), p);
	save_unicode_text("] ", 2, p);
	save_unicode_text(txt, len, p);
	return (0);
}

char *rfc2047_decode_unicode(const char *text,
	const struct unicode_info *mychset,
	int options)
{
	struct decode_unicode_s s;
	char *p=0;

	s.mychset=mychset;
	s.options=0;

	s.bufptr=0;
	s.bufsize=1;


	if (rfc2047_decode(text, &save_unicode, &s))
		return (0);

	s.bufptr=p=malloc(s.bufsize);
	if (!s.bufptr)
		return (0);

	s.bufsize=0;
	if (rfc2047_decode(text, &save_unicode, &s))
	{
		free(p);
		return (0);
	}
	save_unicode_text("", 1, (void *)&s);
	return (p);
}

#endif
