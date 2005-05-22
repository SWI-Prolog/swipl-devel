/*
** Copyright 1998 - 1999 Double Precision, Inc.  See COPYING for
** distribution information.
*/

/*
** $Id$
*/
#include	"rfc2045.h"

static unsigned char base64buf[57];
		/* When encoded this becomes 76 characters */
static int base64cnt;

static void (*output_func)(const char *, size_t);

void rfc2045_base64encode_start( void (*func)(const char *, size_t))
{
	output_func=func;
	base64cnt=0;
}

void rfc2045_base64encode(const char *buf, size_t n)
{
	while (n)
	{
	size_t	i;

		if (base64cnt == sizeof(base64buf))
		{
			rfc2045_base64encode_end();
			base64cnt=0;
		}
		i=n;
		if (i > sizeof(base64buf) - base64cnt)
			i=sizeof(base64buf) - base64cnt;
		memcpy(base64buf + base64cnt, buf, i);
		base64cnt += i;
		buf += i;
		n -= i;
	}
}

static const char base64tab[]=
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

void rfc2045_base64encode_end()
{
int	a=0,b=0,c=0;
int	i, j;
int	d, e, f, g;
char	output_buf[ sizeof(base64buf) / 3 * 4+1];

	if (base64cnt == 0)	return;

	for (j=i=0; i<base64cnt; i += 3)
	{
		a=base64buf[i];
		b= i+1 < base64cnt ? base64buf[i+1]:0;
		c= i+2 < base64cnt ? base64buf[i+2]:0;

		d=base64tab[ a >> 2 ];
		e=base64tab[ ((a & 3 ) << 4) | (b >> 4)];
		f=base64tab[ ((b & 15) << 2) | (c >> 6)];
		g=base64tab[ c & 63 ];
		if (i + 1 >= base64cnt)	f='=';
		if (i + 2 >= base64cnt) g='=';
		output_buf[j++]=d;
		output_buf[j++]=e;
		output_buf[j++]=f;
		output_buf[j++]=g;
	}

	output_buf[j++]='\n';
	(*output_func)(output_buf, j);
}
