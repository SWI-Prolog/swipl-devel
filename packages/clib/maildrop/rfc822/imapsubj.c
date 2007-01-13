/*
** Copyright 2000 Double Precision, Inc.
** See COPYING for distribution information.
*/

/*
** $Id$
*/
#include	<stdio.h>
#include	<ctype.h>
#include	<stdlib.h>
#include	<string.h>
#include	"rfc822.h"
#ifndef __WINDOWS__
#include	"config.h"
#endif

#if	HAVE_STRCASECMP

#else
#define	strcasecmp	stricmp
#endif

#if	HAVE_STRNCASECMP

#else
#define	strncasecmp	strnicmp
#endif


/* Remove artifacts from the subject header */

static void stripsubj(char *s, int *hasrefwd)
{
	char	*p;
	char	*q;

	for (p=q=s; *p; p++)
	{
		if (!isspace((int)(unsigned char)*p))
		{
			*q++=*p;
			continue;
		}
		while (p[1] && isspace((int)(unsigned char)p[1]))
		{
			++p;
		}
		*q++=' ';
	}
	*q=0;

	for (;;)
	{
		/*
		**
		** (2) Remove all trailing text of the subject that matches
		** the subj-trailer ABNF, repeat until no more matches are
		** possible.
		**
		**  subj-trailer    = "(fwd)" / WSP
		*/

		for (p=s; *p; p++)
			;
		while (p > s)
		{
			if ( isspace((int)(unsigned char)p[-1]))
			{
				--p;
				continue;
			}
			if (p-s >= 5 && strncasecmp(p-5, "(FWD)", 5) == 0)
			{
				p -= 5;
				*hasrefwd=1;
				continue;
			}
			break;
		}
		*p=0;

		for (p=s; *p; )
		{
			int isrefwd=0;

			for (;;)
			{
				int needcolon=1;

				/*
				**
				** (3) Remove all prefix text of the subject
				** that matches the subj-leader ABNF.
				**
				**   subj-leader     = (*subj-blob subj-refwd) / WSP
				**
				**   subj-blob       = "[" *BLOBCHAR "]" *WSP
				**
				**   subj-refwd      = ("re" / ("fw" ["d"])) *WSP [subj-blob] ":"
				**
				**   BLOBCHAR        = %x01-5a / %x5c / %x5e-7f
				**                   ; any CHAR except '[' and ']'
				**
				**   THIS IS A BUG: whitespace should also be excluded from BLOBCHAR
				*/

				if (isspace((int)(unsigned char)*p))
				{
					++p;
					continue;
				}
				if (strncasecmp(p, "RE", 2) == 0)
				{
					q=p+2;
					isrefwd=1;
				}
				else if (strncasecmp(p, "FWD", 3) == 0)
				{
					q=p+3;
					isrefwd=1;
				}
				else if (strncasecmp(p, "FW", 2) == 0)
				{
					q=p+2;
					isrefwd=1;
				}
				else if (*p == '[')
				{
					q=p;
					needcolon=0;
				}
				else
					break;

				if (*q == '[')
				{
					++q;
					
					while (*q && *q != '[' && *q != ']'
					       && *q != ' ')
						++q;

					if (*q != ']')	break;
					++q;
				}
				if (needcolon && *q++ != ':')	break;
				p=q;
			}
			/*
			** (4) If there is prefix text of the subject that
			** matches the subj-blob ABNF, and removing that
			** prefix leaves a non-empty subj-base, then remove
			** the prefix text.
			**
			**   subj-base       = NONWSP *([*WSP] NONWSP)
			**                   ; can be a subj-blob
			*/


			if (*p == '[')
			{
				for (q=p; *q; q++)
					if (*q == '[' || *q == ' '
					    || *q == ']')
						break;
				if (*q == ']')
				{
					++q;
					while (*q && isspace((int)(unsigned
								   char)*q))
						++q;
					
					if (*q)
					{
						p=q;
						if (isrefwd)
							*hasrefwd=1;
						continue;
					}
				}
			}
			if (isrefwd)
				*hasrefwd=1;

			break;
		}
		/*
		**
		** (6) If the resulting text begins with the subj-fwd-hdr ABNF
		** and ends with the subj-fwd-trl ABNF, remove the
		** subj-fwd-hdr and subj-fwd-trl and repeat from step (2).
		**
		**   subj-fwd-hdr    = "[fwd:"
		**
		**   subj-fwd-trl    = "]"
		*/

		if (strncasecmp(p, "[FWD:", 5) == 0)
		{
			q=strrchr(p, ']');
			if (q && q[1] == 0)
			{
				*q=0;
				p += 5;

				q=s;
				while ( (*q++ = *p++) != 0)
					;

				*hasrefwd=1;
				continue;
			}
		}
		break;
	}

	q=s;
	while ( (*q++ = *p++) != 0)
		;
}

char *rfc822_coresubj(const char *s, int *hasrefwd)
{
	char *q=strdup(s), *r;
	int dummy;

	if (!hasrefwd)
		hasrefwd= &dummy;

	*hasrefwd=0;
	if (!q)	return (0);

	for (r=q; *r; r++)
		if ((*r & 0x80) == 0)	/* Just US-ASCII casing, thanks */
			*r=toupper((int)(unsigned char)*r);
	stripsubj(q, hasrefwd);
	return (q);
}

char *rfc822_coresubj_nouc(const char *s, int *hasrefwd)
{
	char *q=strdup(s);
	int dummy;

	if (!hasrefwd)
		hasrefwd= &dummy;

	*hasrefwd=0;
	if (!q)	return (0);

	stripsubj(q, hasrefwd);
	return (q);
}
