/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Richard O'Keefe
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

#include "util.h"
#include "catalog.h"
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#define DTD_MINOR_ERRORS 1
#include <dtd.h>			/* error codes */

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

typedef struct catalogue_item *catalogue_item_ptr;
struct catalogue_item
{ catalogue_item_ptr next;
  int kind;
  char const *target;
  char const *replacement;
};

static catalogue_item_ptr first_item = 0, last_item = 0;

typedef struct _catalog_file
{ char *file;
  struct _catalog_file *next;
  int loaded;			/* did we parse this file? */
  catalogue_item_ptr first_item;	/* List of items in the file */
  catalogue_item_ptr last_item;
} catalog_file;

static catalog_file *catalog;

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

  for (base = p = f; *p; p++)
  { if (isDirSep(*p) && p[1] != EOS)
      base = p;
  }
  if (base == f)
  { if (isDirSep(*f))
      strcpy(dir, DIRSEPSTR);
    else
      strcpy(dir, ".");
  } else
  { strncpy(dir, f, base - f);
    dir[base - f] = EOS;
  }

  return dir;
}


int
is_absolute_path(const char *name)
{ if (isDirSep(name[0])
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

  if (!ref || is_absolute_path(name))
    local = strdup(name);
  else
  { char buf[MAXPATHLEN];

    DirName(ref, buf);
    strcat(buf, DIRSEPSTR);
    strcat(buf, name);

    local = strdup(buf);
  }

  if (!local)
    sgml_nomem();

  return local;
}


int
register_catalog_file(const char *file, catalog_location where)
{ catalog_file **f = &catalog;
  catalog_file *cf;

  for (; *f; f = &(*f)->next)
  { cf = *f;

    if (streq(cf->file, file))
      return TRUE;		/* existing, move? */
  }

  cf = sgml_malloc(sizeof(*cf));
  memset(cf, 0, sizeof(*cf));
  cf->file = strdup(file);
  if (!cf->file)
    sgml_nomem();

  if (where == CTL_END)
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

  if (!done)
  { if (!catalog)
    { char *path = getenv("SGML_CATALOG_FILES");

      if (!path)
	return;

      while (*path)
      { char buf[MAXPATHLEN];
	char *s;

	if ((s = strchr(path, ':')))
	{ strncpy(buf, path, s - path);
	  buf[s - path] = '\0';
	  path = s + 1;
	} else
	{ register_catalog_file(path, CTL_START);
	  return;
	}

	register_catalog_file(buf, CTL_START);
      }
    }
  }
}


		 /*******************************
		 *     CATALOG FILE PARSING	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The code from here to the  end  of   this  file  was  written by Richard
O'Keefe and modified by Jan Wielemaker to fit   in  with the rest of the
parser.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*  OVERRIDE YES/NO
    sets a boolean flag initialised to NO.
    The value of this flag is stored as part of each entry.
    (PUBLIC|DOCTYPE|ENTITY)&YES will match whether a system identifier
    was provided in the source document or not;
    (PUBLIC|DOCTYPE|ENTITY)&NO will only match if a system identifier
    was not provided.
*/

/*  catalogue =
    (   PUBLIC  pubid filename
    |   SYSTEM  sysid filename
    |   DOCTYPE name  filename
    |   ENTITY  name  filename
    |   OVERRIDE YES
    |   OVERRIDE NO
    |   BASE          filename
    |   junk
    )*
*/


/*  Keywords are matched ignoring case.  */

static int
ci_streql(char const *a, char const *b)
{ unsigned char const *x = (unsigned char const *) a;
  unsigned char const *y = (unsigned char const *) b;

  for (;;)
  { if (tolower(*x) != tolower(*y))
      return 0;
    if (*x == '\0')
      return 1;
    x++, y++;
  }
}

/*  Names may be matched heeding case in XML.  */

static int
cs_streql(char const *a, char const *b)
{ return 0 == strcmp(a, b);
}

/*  Any other word or any quoted string is reported as CAT_OTHER.
    When we are not looking for the beginning of an entry, the only
    positive outcome is CAT_OTHER.
*/

static int
scan_overflow(size_t buflen)
{ gripe(ERC_REPRESENTATION, "token length");

  return EOF;
}

static int
scan(FILE * src, char *buffer, size_t buflen, int kw_expected)
{ int c, q;
  char *p = buffer, *e = p + buflen - 1;

  for (;;)
  { c = getc(src);
    if (c <= ' ')
    { if (c < 0)
	return EOF;
      continue;
    }
    if (c == '-')
    { c = getc(src);
      if (c != '-')
      { *p++ = '-';
	break;
      }
      for (;;)
      { c = getc(src);
	if (c < 0)
	  return EOF;
	if (c == '-')
	{ c = getc(src);
	  if (c < 0)
	    return EOF;
	  if (c == '-')
	    break;
	}
      }
      continue;
    }
    if (c == '"' || c == '\'')
    { q = c;
      for (;;)
      { c = getc(src);
	if (c < 0)
	  return EOF;
	if (c == q)
	{ *p = '\0';
	  return CAT_OTHER;
	}
	if (p == e)
	  return scan_overflow(buflen);
	*p++ = c;
      }
    }
    break;
  }
  /*  We reach here if there is an unquoted token.   */
  /*  Don't try "PUBLIC--well/sortof--'foo' 'bar'"   */
  /*  because hyphens are allowed in unquoted words  */
  /*  and so are slashes and a bunch of other stuff. */
  /*  To keep this code simple, an unquoted token    */
  /*  ends at EOF, ', ", or layout.                  */
  while (c > ' ' && c != '"' && c != '\'')
  { if (p == e)
      return scan_overflow(buflen);
    *p++ = c;
    c = getc(src);
  }
  *p = '\0';
  if (kw_expected)
  { if (ci_streql(buffer, "public"))
      return CAT_PUBLIC;
    if (ci_streql(buffer, "system"))
      return CAT_SYSTEM;
    if (ci_streql(buffer, "entity"))
      return CAT_ENTITY;
    if (ci_streql(buffer, "doctype"))
      return CAT_DOCTYPE;
    if (ci_streql(buffer, "override"))
      return CAT_OVERRIDE;
    if (ci_streql(buffer, "base"))
      return CAT_BASE;
  }
  return CAT_OTHER;
}

/*  The strings can represent names (taken verbatim),
    system identifiers (ditto), or public identifiers (squished).
    We need to squish, and we need to copy.  When it comes to
    squishing, we don't need to worry about Unicode spaces,
    because public identifiers aren't allow to have any characters
    that aren't in ASCII.
*/

static void
squish(char *pubid)
{ unsigned char const *s = (unsigned char const *) pubid;
  unsigned char *d = (unsigned char *) pubid;
  unsigned char c;
  int w;

  w = 1;
  while ((c = *s++) != '\0')
  { if (c <= ' ')
    { if (!w)
	*d++ = ' ', w = 1;
    } else
    { *d++ = c, w = 0;
    }
  }
  if (w && d != (unsigned char *) pubid)
    d--;
  *d = '\0';
}

/*  We represent a catalogue internally by a list of
    (CAT_xxx, string, string)
    triples.
*/

static void
load_one_catalogue(catalog_file * file)
{ FILE *src = fopen(file->file, "r");
  char buffer[2 * FILENAME_MAX];
  char base[2 * FILENAME_MAX];
  char *p;
  int t;
  catalogue_item_ptr this_item;
  int override = 0;

  if ( src == 0 )
  { gripe(ERC_NO_CATALOGUE, file->file);
    return;
  }

  (void) strcpy(base, file->file);
  p = base + strlen(base);
  while (p != base && !isDirSep(p[-1]))
    p--;

  for (;;)
  { t = scan(src, buffer, sizeof buffer, 1);
    switch (t)
    { case CAT_BASE:
	if (scan(src, buffer, sizeof buffer, 0) == EOF)
	  break;
	(void) strcpy(base, buffer);
	p = base + strlen(base);
	if (p != base && !isDirSep(p[-1]))
	  *p++ = '/';
	continue;
      case CAT_OVERRIDE:
	if (scan(src, buffer, sizeof buffer, 0) == EOF)
	  break;
	override = tolower(buffer[0]) == 'y' ? CAT_OVERRIDE : 0;
	continue;
      case CAT_PUBLIC:
      case CAT_SYSTEM:
      case CAT_ENTITY:
      case CAT_DOCTYPE:
	this_item = sgml_malloc(sizeof *this_item);
	if (scan(src, buffer, sizeof buffer, 0) == EOF)
	  break;
	if (t == CAT_PUBLIC)
	  squish(buffer);
	this_item->next = 0;
	this_item->kind = t == CAT_SYSTEM ? t : t + override;
	this_item->target = istrdup(buffer);

	if (scan(src, buffer, sizeof buffer, 0) == EOF)
	  break;

	if (is_absolute_path(buffer) || p == base)
	{ this_item->replacement = istrdup(buffer);
	} else
        { (void) strcpy(p, buffer);
          this_item->replacement = istrdup(base);
        }

	if (file->first_item == 0)
	{ file->first_item = this_item;
	} else
	{ file->last_item->next = this_item;
	}

	file->last_item = this_item;
	continue;
      case EOF:
	break;
      default:
	continue;
    }
    break;
  }
}


/*  To look up a DTD:
    f = find_in_catalogue(CAT_DOCTYPE, name, pubid, sysid, ci);
    If it cannot otherwise be found and name is not null,
    ${name}.dtd will be returned.

    To look up a parameter entity:
    f = find_in_catalogue(CAT_PENTITY, name, pubid, sysid, ci);
    The name may begin with a % but need not; if it doesn't    
    a % will be prefixed for the search.
    If it cannot otherwise be found ${name}.pen will be returned.

    To look up an ordinary entity:
    f = find_in_catalogue(CAT_ENTITY, name, pubid, sysid, ci);
    If the name begins with a % this is just like a CAT_PENTITY search.
    If it cannot otherwise be found %{name}.ent will be returned.

    The full catalogue format allows for NOTATION (which we still need
    for XML), SGMLDECL, DTDDECL, and LINKTYPE.  At the moment, only
    notation is plausible.  To handle such things,
    f = find_in_catalogue(CAT_OTHER, name, pubid, sysid, ci);
    If it cannot be found, NULL is returned.

    The name, pubid, and sysid may each be NULL.   It doesn't really
    make sense for them all to be NULL.

    For SGML, name matching (DOCTYPE, ENTITY) should normally ignore
    alphabetic case.  Pass ci=1 to make this happen.  For XML, name
    matching must heed alphabetic case.  Pass ci=0 to make that happen.

    A CAT_DOCTYPE, CAT_ENTITY, or CAT_PENTITY search doesn't really make
    sense withint a name, so if the name should happen to be 0, the search
    kind is converted to CAT_OTHER.
*/

char const *
find_in_catalogue(int kind,
		  char const *name,
		  char const *pubid, char const *sysid, int ci)
{ char penname[FILENAME_MAX];
  catalogue_item_ptr item;
  char const *result;
  catalog_file *catfile;

  init_catalog();

  if (name == 0)
  { kind = CAT_OTHER;
  } else
  { switch (kind)
    { case CAT_OTHER:
      case CAT_DOCTYPE:
	break;
      case CAT_PENTITY:
	if (name[0] != '%')
	{ penname[0] = '%';
	  (void) strcpy(penname + 1, name);
	  name = penname;
	}
	break;
      case CAT_ENTITY:
	if (name[0] == '%')
	{ kind = CAT_PENTITY;
	}
	break;
      default:
	return 0;
    }
  }

  result = 0;
  for (catfile = catalog;; catfile = catfile->next)
  { if (catfile)
    { if (!catfile->loaded)
      { load_one_catalogue(catfile);
	catfile->loaded = TRUE;
      }
      item = catfile->first_item;
    } else
      item = first_item;

    for (; item != 0; item = item->next)
    { switch (item->kind)
      { case CAT_PUBLIC:
	  if (sysid != 0)
	    break;
	/*FALLTHROUGH*/
	case OVR_PUBLIC:
	  if (pubid != 0 && result == 0 && cs_streql(pubid, item->target))
	    result = item->replacement;
	  break;
	case CAT_SYSTEM:
	  if (sysid != 0 && cs_streql(sysid, item->target))
	    return item->replacement;
	  break;
	case CAT_DOCTYPE:
	  if (sysid != 0)
	    break;
	/*FALLTHROUGH*/
	case OVR_DOCTYPE:
	  if (name != 0 && kind == CAT_DOCTYPE && result == 0
	      && (ci ? ci_streql : cs_streql) (name, item->target))
	    result = item->replacement;
	  break;
	case CAT_ENTITY:
	  if (sysid != 0)
	    break;
	 /*FALLTHROUGH*/ case OVR_ENTITY:
	  if (name != 0 && kind >= CAT_ENTITY && result == 0
	      && (ci ? ci_streql : cs_streql) (name, item->target))
	    result = item->replacement;
	  break;
	default:
	  break;
      }
    }

    if (!catfile)
      break;
  }
  if ( result != 0 )
    return result;
  if ( sysid != 0 )
    return sysid;
  if ( kind == CAT_OTHER || kind == CAT_DOCTYPE )
    return 0;

  if ( strlen(name)+4+1 > sizeof(penname) )
  { gripe(ERC_REPRESENTATION, "entity name");
    return NULL;
  }

  item = sgml_malloc(sizeof *item);
  item->next = 0;
  item->kind = kind;
  item->target = istrdup(name);

  switch (kind)
  { case CAT_DOCTYPE:
      (void) sprintf(penname, "%s.dtd", name);
      break;
    case CAT_PENTITY:
      item->kind = CAT_ENTITY;
      (void) sprintf(penname, "%s.pen", name + 1);
      break;
    case CAT_ENTITY:
      (void) sprintf(penname, "%s.ent", name);
      break;
    default:
      abort();
  }

  item->replacement = istrdup(penname);
  if (first_item == 0)
  { first_item = item;
  } else
  { last_item->next = item;
  }
  last_item = item;

  return item->replacement;
}

