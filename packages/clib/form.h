/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
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
