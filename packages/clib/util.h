/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

#ifndef UTIL_H_INCLUDED
#define UTIL_H_INCLUDED

#define HAVE_STRERROR 1
#define HAVE_TZNAME

#if defined(sun) && !defined(__svr4__)	/* Old SunOS 4.x */
#undef HAVE_STRERROR
#undef HAVE_TZNAME
#endif

#include <stdarg.h>
#include <time.h>

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

typedef struct
{ char *name;
  char *value;
} var, *Var;

extern int	use_nph;		/* 1: nph-cgi-script */

/* util.c */
#ifndef HAVE_STRERROR
const char *	strerror(int err);
#endif
char *		save_string(const char *s);
int             cat(const char *file);
int             catfd(const char *file, FILE *out);
void            error(const char *msg, ...);
void		printenv(void);
int		copyPage(FILE *in, FILE *out, Var vars);
int		echoPage(const char *page, ...);
int		echoPagev(const char *page, va_list args);
int		mailFileFromVars(const char *email,
				 const char *from,
				 const char *file,
				 Var vars);
int		mailFile(const char *email,
			 const char *from,
			 const char *file,
			 ...);
long		fileSize(const char *file);
char *		rfc_date(time_t t);
char *		rfc_modified(const char *file);

#endif /*UTIL_H_INCLUDED*/
