/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "catalog.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
#ifndef MAXLINE
#define MAXLINE 1024
#endif
#ifndef EOS
#define EOS '\0'
#endif

static char *
skip_layout(const char *in)
{ while(*in && isspace(*in))
    in++;

  return (char *)in;
}


static char *
see_identifier(const char *in, const char *id)
{ in = skip_layout(in);

  while (*id && tolower(*id) == tolower(*in) )
    id++, in++;

  if ( *id == 0 && !isalnum(*in) )
    return skip_layout(in);

  return NULL;
}


static char *
see_string(const char *in, const char *s)
{ in = skip_layout(in);
  
  if ( *in != '"' )
  { while(*s && tolower(*s) == tolower(*in))
      s++, in++;
    if ( *s == 0 && isspace(*in) )
      return skip_layout(in);
    return NULL;
  }
  in++;
  while(*s && tolower(*s) == tolower(*in))
    s++, in++;

  if ( *s == 0 && *in == '"' )
    return skip_layout(++in);

  return NULL;
}


static char *
DirName(const char *f, char *dir)
{ const char *base, *p;

  for(base = p = f; *p; p++)
  { if (*p == '/' && p[1] != EOS )
      base = p;
  }
  if ( base == f )
  { if ( *f == '/' )
      strcpy(dir, "/");
    else
      strcpy(dir, ".");
  } else
  { strncpy(dir, f, base-f);
    dir[base-f] = EOS;
  }
  
  return dir;
}


char *
localpath(const char *ref, const char *name)
{ if ( name[0] == '/' )
    return strdup(name);
  else
  { char buf[MAXPATHLEN];

    DirName(ref, buf);
    strcat(buf, "/");
    strcat(buf, name);

    return strdup(buf);
  }
}


static char *
find_public_entity_in_catalog(const char *catfile,
			      const char *key,
			      const char *name)
{ FILE *fd;

  if ( (fd = fopen(catfile, "r")) != NULL )
  { char line[MAXLINE];
    char *s;
  
    while( (s=fgets(line, sizeof(line), fd)) )
    { char *e;

      if ( !(s=see_identifier(line, key)) )
	continue;
      if ( !(s=see_string(s, name)) )
	continue;

      fclose(fd);			/* found it */
					/* strip trailing blanks */
      for(e=s+strlen(s); e>s && isspace(e[-1]); e--)
	;
      *e = '\0';

      return localpath(catfile, s);
    }

    fclose(fd);
  }

  return NULL;
}


char *
find_in_catalog(const char *key, const char *name)
{ char *path = getenv("SGML_CATALOG_FILES");
  char *s;
  char buf[MAXPATHLEN];

  if ( !path )
    return NULL;

  while(*path)
  { if ( (s=strchr(path, ':')) )
    { strncpy(buf, path, s-path);
      buf[s-path] = '\0';
      path = s+1;
    } else
    { return find_public_entity_in_catalog(path, key, name);
    }

    if ( (s=find_public_entity_in_catalog(buf, key, name)) )
      return s;
  }

  return NULL;
}



