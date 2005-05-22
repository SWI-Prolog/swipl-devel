/*
** Copyright 2000 Double Precision, Inc.  See COPYING for
** distribution information.
*/

/*
** $Id$
*/
#if    HAVE_CONFIG_H
#include       "config.h"
#endif
#include       <stdlib.h>
#include       <stdio.h>
#include       <string.h>
#if    HAVE_STRINGS_H
#include       <strings.h>
#endif
#include	<ctype.h>
#include	"rfc2045.h"

extern void rfc2045_enomem();

/*
** ---------------------------------------------------------------------
** Attempt to parse Content-Base: and Content-Location:, and return the
** "base" of all the relative URLs in the section.
** ---------------------------------------------------------------------
*/

static void get_method_path(const char *p,
	const char **method,
	unsigned *methodl,
	const char **path)
{
unsigned	i;

	for (i=0; p && p[i]; i++)
	{
		if (p[i] == ':')
		{
			*method=p;
			*methodl= ++i;
			*path=p+i;
			return;
		}

		if (!isalpha( (int)(unsigned char)p[i]))
			break;
	}

	*method=0;
	*methodl=0;
	*path=p;
}

char *rfc2045_append_url(const char *base, const char *loc)
{
const char *base_method;
unsigned base_method_l;
const char *base_path;

const char *loc_method;
unsigned loc_method_l;
const char *loc_path;
char *buf, *q;

	get_method_path(base, &base_method, &base_method_l, &base_path);
	get_method_path(loc, &loc_method, &loc_method_l, &loc_path);

	if (loc_method_l)
	{
		buf=malloc(strlen(loc)+1);
		if (!buf)
			rfc2045_enomem();
		else
			strcpy(buf, loc);
		return (buf);
	}

	loc_method=base_method;
	loc_method_l=base_method_l;

	if (!base_path)	base_path="";
	if (!loc_path)	loc_path="";

	buf=malloc(loc_method_l + strlen(base_path)+strlen(loc_path) + 3);

	if (!buf)
	{
		rfc2045_enomem();
		return (0);
	}

	if (loc_method_l)
		memcpy(buf, loc_method, loc_method_l);
	buf[loc_method_l]=0;

	q=buf + loc_method_l;

	strcat(strcpy(q, base_path), "/");

	if ( loc_path[0] == '/')
	{
	char *r;

		if (loc_path[1] == '/')
			/* Location is absolute */
		{
			*q=0;
		}

		/* Relative to top of base */

		else if ( q[0] == '/' && q[1] == '/' &&
			(r=strchr(q+2, '/')) != 0)
		{
			*r=0;
		}
		else
			*q=0;	/* No sys in base, just start with / */
	}

	strcat(q, loc_path);

	return (buf);
}

char *rfc2045_content_base(struct rfc2045 *p)
{
	return (rfc2045_append_url(p->content_base, p->content_location));
}
