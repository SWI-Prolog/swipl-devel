/*
** Copyright 1998 - 1999 Double Precision, Inc.  See COPYING for
** distribution information.
*/

/*
** $Id$
*/
#include	<stdio.h>
#include	"rfc2045.h"

#define	DUMP(s,n) if ( (s) ) { printf("%*s%s: %s\n", level*4, "", n, (s)); }

/* Extended RFC2045 dump */

static void dodump(struct rfc2045 *p, int level)
{
	if (!p->isdummy)
	{
		printf("%*sMessage start %ld, end %ld, startbody %ld, endbody %ld.\n", level*4,
			"", (long)p->startpos, (long)p->endpos,
			(long)p->startbody, (long)p->endbody);
		DUMP(p->mime_version, "Mime-Version")
		DUMP(p->content_type, "Content-Type")
		DUMP(rfc2045_getattr(p->content_type_attr, "charset"),
								"Charset")
		DUMP(p->content_transfer_encoding, "Transfer Encoding")
		DUMP(rfc2045_getattr(p->content_type_attr, "boundary"),
								"Boundary")
		DUMP(p->content_disposition, "Content Disposition")
		DUMP(rfc2045_getattr(p->content_disposition_attr, "name"),
							"Disposition Name")
		DUMP(rfc2045_getattr(p->content_disposition_attr, "filename"),
							"Disposition Filename")
	}

	for (p=p->firstpart; p; p=p->next)
	{
		printf("%*s{\n", level*4, "");
		dodump(p, level+1);
		printf("%*s}\n", level*4, "");
	}
}

void rfc2045_xdump(struct rfc2045 *p)
{
	dodump(p, 0);
}
