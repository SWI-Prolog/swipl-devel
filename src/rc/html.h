/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1998-2011, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef HTML_H_INCLUDED
#define HTML_H_INCLUDED

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define HTML_TAG_STRING	0
#define HTML_TAG_SHORT	1
#define HTML_TAG_INT	2
#define HTML_TAG_LONG	3
#define HTML_TAG_FLOAT	4
#define HTML_TAG_DOUBLE	5
#define HTML_TAG_BOOL	6
#define HTML_TAG_ENUM   7

#define MAXTAGLEN 128
#define MAXTAGPROPLEN 1024

typedef int (*HtmlTagConverter)(const char *data, size_t len, void *dst,
				void *closure);

typedef struct
{ char		       *tag;		/* tag-name */
  int	 		offset;		/* byte-offset */
  HtmlTagConverter	convert;	/* conversion function */
  void		       *closure;	/* conversion closure */
} htmltagdef, *HtmlTagDef;

extern int html_fd_next_tag(FILE *fd, char *tag, char *props);
extern int html_fd_find_close_tag(FILE *fd, const char *etag);

extern char *html_decode_tag(const char *data, HtmlTagDef spec, void *dest);
extern char *html_find_tag(const char *data, const char *end, const char *tag);
extern char *html_find_close_tag(const char *data, const char *tag);

extern int html_cvt_malloc_string(const char *d, size_t len, void *dst, void *cl);
extern int html_cvt_long(const char *d, size_t len, void *dst, void *cl);
extern int html_cvt_date(const char *d, size_t len, void *dst, void *cl);

#endif /*HTML_TAG_ENUM*/
