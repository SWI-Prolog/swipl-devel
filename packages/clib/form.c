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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <ctype.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <string.h>
#include "form.h"
#ifdef WIN32
#include <io.h>
#endif

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Breaks a string holding data from a WWW form into its values.  Outputs a
sequence of NAME=VALUE commands for a shell.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
dehex(int chr)
{ chr &= 0xff;

  if ( chr >= '0' && chr <= '9' )
    return chr - '0';
  if ( chr >= 'A' && chr <= 'F' )
    return chr - 'A' + 10;
  if ( chr >= 'a' && chr <= 'f' )
    return chr - 'f' + 10;

  return -1;
}


static int
form_argument_decode(const char *in, int inlen, char *out, int outlen)
{ const char *ein  = in+inlen;
  int written = 0;

  for(; in < ein; in++)
  { switch(*in)
    { case '+':
	if ( ++written < outlen )
	  *out++ = ' ';
        break;
      case '%':
	if ( in+2 < ein )
	{ int h1 = dehex(*(++in));
	  int h2 = dehex(*(++in));

	  if ( h1 < 0 || h2 < 0 )
	    return -1;
	  
	  if ( ++written < outlen )
	    *out++ = h1<<4|h2;
	} else
	  return -1;			/* syntax error */
	break;
      default:
	if ( ++written < outlen )
	  *out++ = *in;
        break;
    }
  }

  if ( written < outlen )
    *out++ = '\0';

  return written;
}


int
break_form_argument(const char *formdata,
		    int (*func)(const char* name,
				const char *value,
				void *closure),
		    void *closure)
{ while ( *formdata )
  { char name[MAXNAME];
    char value[MAXVALUE];
    char *eq = strchr(formdata, '=');

    if ( eq )
    { int len = eq-formdata;
      char *end;
      int vlen;

      if ( len > MAXNAME-1 )
	return ERROR_NAME_TOO_LONG;
      strncpy(name, formdata, len);
      name[len] = '\0';

      eq++;
      end = strchr(eq, '&');
      if ( !end )
	end = eq+strlen(eq);		/* end of the string */

      if ( (vlen=form_argument_decode(eq, end-eq, value, MAXVALUE)) >= MAXVALUE )
	return ERROR_VALUE_TOO_LONG;
      if ( vlen < 0 )
	return ERROR_SYNTAX_ERROR;

      (func)(name, value, closure);

      if ( *end )
	formdata = end+1;
      else
	formdata = end;
    }
  }

  return TRUE;
}


static char *
find_boundary(const char *data, const char *end, const char *boundary)
{ int blen = strlen(boundary);

  while ( data < end &&
	  !(strncmp(data, boundary, blen) == 0) )
    data++;

  if ( data < end )
  { while(data[-1] == '-')
      data--;
    return (char *)data;
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find a named attribute in a mime header of a multipart form
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static char *
attribute_of_multipart_header(const char *name, char *header, char *endheader)
{ char *value;
  int nlen = strlen(name);

  while( header < endheader &&
	 !(header[nlen] == '=' && strncmp(header, name, nlen) == 0) )
    header++;

  if ( header < endheader )
  { header += nlen+1;

    if ( header[0] == '"' )
    { char *end;

      value = ++header;
      if ( (end = strchr(value, '"')) )
      { *end = '\0';
        return value;
      }
    } else
    { char *end;

      value = header;

      for(end=header; *end && isalnum(*end&0xff); end++)
	;
      *end = '\0';
      return value;
    }
  }

  return NULL;
}


static char *
looking_at_blank_lines(const char *line, int n)
{ while(n-- > 0)
  { if ( (line[0] == '\r' && line[1] == '\n') )
      line += 2;
    else if ( line[0] == '\n' )
      line += 1;
    else
      return NULL;
  }

  return (char *)line;
}


char *
next_line(const char *in)
{ if ( (in = strchr(in, '\n')) )
    return (char *)(in+1);

  return NULL;
}


int
break_multipart(char *formdata, int len,
		const char *boundary,
		int (*func)(const char *name,
			    const char *value,
			    int valuelen,
			    const char *filename,
			    void *closure),
		void *closure)
{ char *enddata = formdata+len;

  while(formdata < enddata)
  { char *header;
    char *name, *filename;
    char *data = NULL;
    char *end;

    if ( !(formdata=find_boundary(formdata, enddata, boundary)) ||
	 !(formdata=next_line(formdata)) )
      break;

    header = formdata;
					/* find the end of the header */
    for( ; formdata < enddata; formdata++ )
    { char *end;

      if ( (end = looking_at_blank_lines(formdata, 2)) )
      { formdata[0] = '\0';
	formdata = data = end;
	break;
      }
    }

    if ( !data )
      break;

    if ( !(name = attribute_of_multipart_header("name", header, data)) )
    {
#ifdef UTIL_H_INCLUDED
      error("Cannot find field \"name\" in multipart message");
#endif
      return FALSE;
    }
    filename = attribute_of_multipart_header("filename", header, data);

    if ( !(formdata=find_boundary(data, enddata, boundary)) )
      break;
    end = formdata-1;
    if ( end[-1] == '\r' )
      end--;
    end[0] = '\0';

    if ( !(func)(name, data, end-data, filename, closure) )
      return FALSE;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Get the raw data from the standard   input or QUERY_STRING. If `lenp' is
provided, it is filled with the length  of the contents. The input value
for lenp is the maximum acceptable content-length.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

char *
get_raw_form_data(int *lenp)
{ char *method;
  char *s;

  if ( (method = getenv("REQUEST_METHOD")) &&
       strcmp(method, "POST") == 0 )
  { char *lenvar = getenv("CONTENT_LENGTH");
    char *q;
    int len;

    if ( !lenvar )
      return NULL;
    len = atoi(lenvar);
    if ( lenp )
    { if ( *lenp && len > *lenp )
      {
#ifdef UTIL_H_INCLUDED
	error("Contents too long (accept max. %d Kbytes)", *lenp/1024);
#endif
	return NULL;
      }
      *lenp = len;
    }

    q = s = malloc(len+1);
    if ( !q )
    {
#ifdef UTIL_H_INCLUDED
      error("Not enough memory");
#endif
      return NULL;
    }
    while(len > 0)
    { int done;

      while( (done=read(fileno(stdin), q, len)) > 0 )
      { q+=done;
	len-=done;
      }
    }
    if ( len == 0 )
    { *q = '\0';
      return s;
    }
  } else if ( (s = getenv("QUERY_STRING")) )
  { if ( lenp )
      *lenp = strlen(s);
    return s;
  }
    
  return NULL;
}


static int
fill_arg(const char *name, const char *value, void *closure)
{ form_arg *args = closure;

  for(; args->name; args++)
  { if ( strcmp(name, args->name) == 0 )
    { args->ptr = malloc(strlen(value)+1);
      if ( args->ptr )
      { strcpy(args->ptr, value);

	return TRUE;
      }
    }
  }

  return FALSE;
}


int
decode_form_arguments(const char *data, form_arg *args)
{ return break_form_argument(data, fill_arg, args);
}


