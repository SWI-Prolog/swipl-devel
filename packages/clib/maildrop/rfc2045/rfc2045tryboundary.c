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
#include       <string.h>
#if    HAVE_STRINGS_H
#include       <strings.h>
#endif
#ifdef __WINDOWS__
#define strncasecmp strnicmp
#include <io.h>
#define read _read
#define lseek _lseek
#endif


/* $Id$ */

extern void rfc2045_add_buf( char **, size_t *, size_t *,
	const char *, size_t);

static const char *boundary_chk_val;
static size_t boundary_chk_val_len;
static char *boundary_chk_buf;
static size_t boundary_chk_bufsize, boundary_chk_buflen;
static int boundary_chk_flag;

static void boundary_chk_add(const char *p, size_t l)
{
	if (boundary_chk_buflen < boundary_chk_val_len+20)
		rfc2045_add_buf( &boundary_chk_buf,
			&boundary_chk_bufsize,
			&boundary_chk_buflen, p, l);
}

static int boundary_chk(const char *p, size_t l, void *ptr)
{
static	size_t	i, j;

	for (j=i=0; i<l; i++)
	{
		if (p[i] == '\n')
		{
			boundary_chk_add(p+j, i-j);

			if (boundary_chk_buflen >= boundary_chk_val_len+2 &&
				boundary_chk_buf[0] == '-' &&
				boundary_chk_buf[1] == '-' &&
				strncasecmp(boundary_chk_val,
					boundary_chk_buf+2,
					boundary_chk_val_len) == 0)
					boundary_chk_flag=1;
					
			boundary_chk_buflen=0;
			j=i+1;
		}
	}
	boundary_chk_add(p+j, l-j);
	return (0);
}

static int try_boundary(struct rfc2045 *p, int fd)
{
int	rc;
char	buf[512];
int	n, cnt;
off_t	ps;

	if (p->firstpart)
	{
		for (p=p->firstpart; p; p=p->next)
			if ((rc=try_boundary(p, fd)) != 0)
				return (rc);
		return (0);
	}

	if (p->content_transfer_encoding &&
		strcmp(p->content_transfer_encoding, "base64") == 0)
		return (0);

	boundary_chk_flag=0;
	boundary_chk_buflen=0;

	if (lseek(fd, p->startbody, SEEK_SET) == -1)	return (-1);
	rfc2045_cdecode_start(p, boundary_chk, 0);

	ps=p->startbody;
	while (ps < p->endbody)
	{
		if (p->endbody - ps < sizeof(buf))
			cnt=p->endbody-ps;
		else	cnt=sizeof(buf);
		n=read(fd, buf, cnt);
		if (n <= 0)	return (-1);
		rfc2045_cdecode(p, buf, n);
		ps += n;
		if (boundary_chk_flag)	break;
	}
	rfc2045_cdecode_end(p);
	if (boundary_chk_buflen)
		boundary_chk("\n", 1, 0);	/* Flush out partial line */
	return (boundary_chk_flag);
}

int rfc2045_try_boundary(struct rfc2045 *p, int fd, const char *boundary)
{
int	n;

	boundary_chk_val_len=strlen(boundary_chk_val=boundary);
	boundary_chk_buf=0;
	boundary_chk_bufsize=0;
	n=try_boundary(p, fd);
	if (boundary_chk_buf)	free(boundary_chk_buf);
	return (n);
}
