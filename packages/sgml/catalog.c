/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "util.h"
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
#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

#define streq(s1, s2) strcmp(s1, s2) ==	0
#define uc(p) (*(unsigned char const *)(p))

typedef struct _catalog_file
{ char *file;
  struct _catalog_file *next;
} catalog_file;

static catalog_file *catalog;

static char *
skip_layout(const char *in)
{ while(*in && isspace(uc(in)))
    in++;

  return (char *)in;
}


static char *
see_identifier(const char *in, const char *id)
{ in = skip_layout(in);

  while (*id && tolower(uc(id)) == tolower(uc(in)) )
    id++, in++;

  if ( *id == 0 && !isalnum(uc(in)) )
    return skip_layout(in);

  return NULL;
}


static char *
see_string(const char *in, const char *s)
{ in = skip_layout(in);
  
  if ( *in != '"' )
  { while(*s && tolower(uc(s)) == tolower(uc(in)))
      s++, in++;
    if ( *s == 0 && isspace(uc(in)) )
      return skip_layout(in);
    return NULL;
  }
  in++;
  while(*s && tolower(uc(s)) == tolower(uc(in)))
    s++, in++;

  if ( *s == 0 && *in == '"' )
    return skip_layout(++in);

  return NULL;
}

#ifdef WIN32
#define isDirSep(c) ((c) == '/' || (c) == '\\')
#define DIRSEPSTR "\\"
#else
#define isDirSep(c) ((c) == '/')
#define DIRSEPSTR "/"
#endif

static char *
DirName(const char *f, char *dir)
{ const char *base, *p;

  for(base = p = f; *p; p++)
  { if ( isDirSep(*p) && p[1] != EOS )
      base = p;
  }
  if ( base == f )
  { if ( isDirSep(*f) )
      strcpy(dir, DIRSEPSTR);
    else
      strcpy(dir, ".");
  } else
  { strncpy(dir, f, base-f);
    dir[base-f] = EOS;
  }
  
  return dir;
}


int
is_absolute_path(const char *name)
{ if ( isDirSep(name[0])
#ifdef WIN32
       || (isalpha(uc(name)) && name[1] == ':')
#endif
     )
    return TRUE;

  return FALSE;
} 

char *
localpath(const char *ref, const char *name)
{ char *local;

  if ( !ref || is_absolute_path(name) )
    local = strdup(name);
  else
  { char buf[MAXPATHLEN];

    DirName(ref, buf);
    strcat(buf, DIRSEPSTR);
    strcat(buf, name);

    local = strdup(buf);
  }

  if ( !local )
    sgml_nomem();

  return local;
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
      if ( *s == '"' )
	s++;
      for(e=s+strlen(s); e>s && isspace(uc(e-1)); e--)
	;
      if ( e>s && e[-1] == '"' )
	e--;
      *e = '\0';

      return localpath(catfile, s);
    }

    fclose(fd);
  }

  return NULL;
}


int
register_catalog_file(const char *file, catalog_location where)
{ catalog_file **f = &catalog;
  catalog_file *cf;

  for(; *f; f = &(*f)->next )
  { cf = *f;

    if ( streq(cf->file, file) )
      return TRUE;			/* existing, move? */
  }
  
  cf = sgml_malloc(sizeof(*cf));
  cf->file = strdup(file);
  if ( !cf->file )
    sgml_nomem();
  
  if ( where == CTL_END )
  { cf->next = NULL;
    *f = cf;
  } else
  { cf->next = catalog;
    catalog = cf;
  }

  return TRUE;
}


static void
init_catalog()
{ static int done = FALSE;

  if ( !done )
  { if ( !catalog )
    { char *path = getenv("SGML_CATALOG_FILES");

      if ( !path )
	return;
      
      while(*path)
      { char buf[MAXPATHLEN];
	char *s;

	if ( (s=strchr(path, ':')) )
	{ strncpy(buf, path, s-path);
	  buf[s-path] = '\0';
	  path = s+1;
	} else
	{ register_catalog_file(path, CTL_START);
	  return;
	}

	register_catalog_file(buf, CTL_START);
      }
    }
  }
}


char *
find_in_catalog(const char *key, const char *name)
{ catalog_file *cf;

  init_catalog();

  for(cf = catalog; cf; cf = cf->next)
  { char *s;

    if ( (s=find_public_entity_in_catalog(cf->file, key, name)) )
      return s;
  }

  return NULL;
}



