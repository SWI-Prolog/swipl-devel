/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#define MAXNAME 256
#define MAXVALUE 1024
#define MAXLINE 10240

#define ERROR_NAME_TOO_LONG  -1
#define ERROR_VALUE_TOO_LONG -2
#define ERROR_SYNTAX_ERROR   -3

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

typedef struct
{ char *ptr;
  char *name;
} form_arg;

/* form.c */
int		break_form_argument(const char *formdata,
				    int (*func)(const char *name,
						const char *value,
						void *closure), void *closure);
int		break_multipart(char *formdata, int len,
				const char *boundary,
				int (*func)(const char *name,
					    const char *value,
					    int valuelen,
					    const char *filename,
					    void *closure),
				void *closure);
char *		get_raw_form_data(int *lenp);
int		decode_form_arguments(const char *data, form_arg *args);
