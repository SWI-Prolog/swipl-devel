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

#include <stdio.h>
#include "dtd.h"
#include "util.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#define streq(s1, s2) (strcmp(s1, s2) == 0)

char *program;
int nerrors = 0;
int nwarnings = 0;
int style_messages = FALSE;

static void
usage(void)
{ fprintf(stderr,
	  "Usage: %s [-xml] [-s] [-nodefs] [file.dtd] [file]\n\n", program);
  fprintf(stderr,
	  "\t-xml\tForce XML mode\n"
	  "\t-s\tSilent: only report errors and warnings\n"
	  "\t-style\tWarn about correct but dubious input\n"
	  "\t-nodefs\tDo not include defaulted attributes\n");
  exit(EXIT_FAILURE);
}


static void
print_word(dtd_parser * p, char c,	/* preceding character */
	   ichar const *s,		/* where to start */
	   ichar const *e		/* where to end (at NUL if e is NULL) */
  )
{ register FILE *f = stdout;
  ichar x;

  putc(c, f);
  if (p->dtd->case_sensitive)
  { if (e != 0)
      while (s != e)
	putc(*s++, f);
    else
      while ((x = *s++) != (ichar) 0)
	putc(x, f);
  } else
  { if (e != 0)
      while (s != e)
	putc(toupper(*s++), f);
    else
      while ((x = *s++) != (ichar) 0)
	putc(toupper(x), f);
  }
}


static void
print_escaped(char c, ochar const *s)
{ register FILE *f = stdout;
  ochar x;

  putc(c, f);
  while ((x = *s++) != (ochar) 0)
  { if (x >= ' ')
    { if (x == '\\')
	putc(x, f);
      putc(x, f);
    } else if (x == '\t')
    { putc(x, f);
    } else if (x == '\n')
    { printf("\\n");
    } else
    { printf("\\%3o", x);
    }
  }
  putc('\n', f);
}


static int
print_close(dtd_parser * p, dtd_element * e)
{ print_word(p, ')', e->name->name, 0);
  putchar('\n');

  return TRUE;
}


typedef struct atdef
{ attrtype type;		/* AT_* */
  char const *name;		/* name */
  int islist;			/* list-type */
} atdef;

static atdef attrs[] = {
  {AT_CDATA,	"cdata",    FALSE},
  {AT_ENTITY,	"entity",   FALSE},
  {AT_ENTITIES,	"entity",   TRUE},
  {AT_ID,	"id",	    FALSE},
  {AT_IDREF,	"idref",    FALSE},
  {AT_IDREFS,	"idref",    TRUE},
  {AT_NAME,	"name",	    FALSE},
  {AT_NAMES,	"name",	    TRUE},
  {AT_NMTOKEN,	"nmtoken",  FALSE},
  {AT_NMTOKENS,	"nmtoken",  TRUE},
  {AT_NUMBER,	"number",   FALSE},
  {AT_NUMBERS,	"number",   TRUE},
  {AT_NUTOKEN,	"nutoken",  FALSE},
  {AT_NUTOKENS,	"nutoken",  TRUE},
  {AT_NOTATION,	"notation", FALSE},

  {AT_CDATA,	(char *) 0, FALSE}
};


static atdef *
find_attrdef(attrtype type)
{ atdef *ad;

  for (ad = attrs; ad->name != (char *) 0; ad++)
  { if (ad->type == type)
      return ad;
  }
  assert(0);
  return (atdef *) 0;
}


static ichar *
istrblank(ichar const *s)
{ for (; *s; s++)
  { if (isspace(*s))
      return (ichar *) s;
  }
  return (ichar *) 0;
}


static int
print_open(dtd_parser * p, dtd_element * e, int argc, sgml_attribute * argv)
{ int i;

  for (i = 0; i < argc; i++)
  { print_word(p, 'A', argv[i].definition->name->name, 0);
    switch (argv[i].definition->type)
    { case AT_CDATA:
	printf(" CDATA");
	print_escaped(' ', argv[i].value.cdata);
	continue;		/* so we don't get two line breaks */
      case AT_NUMBER:
	printf(" NUMBER ");
	if (argv[i].value.text)
	  printf("%s", argv[i].value.text);
	else
	  printf("%ld", argv[i].value.number);
	break;
      case AT_NAMEOF:
	printf(" NAME");
	print_word(p, ' ', argv[i].value.text, 0);
	break;
      default:
      { atdef *ad = find_attrdef(argv[i].definition->type);
	ichar const *val = argv[i].value.text;

	print_word(p, ' ', (ichar const *) ad->name, 0);
	if (ad->islist)
	{ ichar const *n;

	  while ((n = istrblank(val)) != 0)
	  { if (n != val)
	      print_word(p, ' ', val, n);
	    val = n + 1;
	  }
	}
	print_word(p, ' ', val, 0);
      }
	break;
    }
    putchar('\n');
  }
  print_word(p, '(', e->name->name, 0);
  putchar('\n');
  return TRUE;
}


static int
print_data(dtd_parser * p, data_type type, int len, ochar const *data)
{ char c;

  switch (type)
  { case EC_CDATA:
      c = '-';
      break;
    case EC_NDATA:
      c = 'N';
      break;
    case EC_SDATA:
      c = 'S';
      break;
    default:
      assert(0);
  }
  print_escaped(c, data);
  return TRUE;
}


static int
on_entity(dtd_parser * p, dtd_entity * e, int chr)
{ if (e == 0)
    printf("&#%d;", chr);
  else
    printf("&%s;", e->name->name);
  return TRUE;
}


static int
on_pi(dtd_parser * p, ichar const *pi)
{ print_escaped('?', (ochar const *) pi);
  return TRUE;
}


static dtd_srcloc *
file_location(dtd_srcloc * l)
{ while (l->parent && l->type != IN_FILE)
    l = l->parent;
  return l;
}

static int
on_error(dtd_parser * p, dtd_error * error)
{ char const *severity;
  char const *dialect;
  dtd_srcloc *l = file_location(error->location);

  switch (p->dtd->dialect)
  { case DL_SGML:
      dialect = "sgml";
      break;
    case DL_XML:
      dialect = "xml";
      break;
    case DL_XMLNS:
    default:			/* make compiler happy */
      dialect = "xmlns";
      break;
  }

  switch (error->severity)
  { case ERS_STYLE:
      severity = "Style";
      if ( !style_messages )
	return TRUE;
      break;
    case ERS_WARNING:
      severity = "Warning";
      nwarnings++;
      break;
    case ERS_ERROR:
    default:			/* make compiler happy */
      severity = "Error";
      nerrors++;
      break;
  }

  fprintf(stderr, "%s: (%s mode) %s: %s:%d:%d %s\n",
	  program, dialect, severity,
	  l->name ? l->name : "[]", l->line, l->linepos,
	  error->plain_message);

  return TRUE;
}


static void
set_functions(dtd_parser * p, int output)
{ if (output)
  { p->on_end_element = print_close;
    p->on_begin_element = print_open;
    p->on_data = print_data;
    p->on_entity = on_entity;
    p->on_pi = on_pi;
  }
  p->on_error = on_error;
}

#define shift (argc--, argv++)

#define strcaseeq(x, y) istrcaseeq((ichar const *)(x), (ichar const *)(y))

static ichar const *no_dtd = (ichar const *) NULL;

int
main(int argc, char **argv)
{ dtd_parser *p = NULL;
  char *s;
  int xml = FALSE;
  int output = TRUE;
  int nodefs = FALSE;		/* include defaulted attributes */

  s = strchr(argv[0], '/');
  program = s == NULL ? argv[0] : s + 1;
  if (streq(program, "xml"))
    xml = TRUE;

  shift;

  while (argc > 0 && argv[0][0] == '-')
  { if (streq(argv[0], "-xml"))
    { xml = TRUE;
    } else if (streq(argv[0], "-s"))
    { output = FALSE;
    } else if (streq(argv[0], "-nodefs"))
    { nodefs = TRUE;
    } else if (streq(argv[0], "-style"))
    { style_messages = TRUE;
    } else
    { usage();
    }
    shift;
  }

  if (argc > 0)
  { char *slash = strchr(argv[0], '/');
    char *dot = strchr(argv[0], '.');
    char *ext = dot == 0 || (slash != 0 && slash > dot) ? "." : dot;

    if (strcaseeq(ext, ".dtd"))
    { char doctype[256];

      strncpy(doctype, argv[0], ext - argv[0]);
      doctype[ext - argv[0]] = '\0';

      p = new_dtd_parser(new_dtd((ichar const *) doctype));
      load_dtd_from_file(p, argv[0]);
      shift;
    } else if (strcaseeq(ext, ".html") || strcaseeq(ext, ".htm"))
    { p = new_dtd_parser(new_dtd((ichar const *) "html"));
      load_dtd_from_file(p, "html.dtd");
    } else if (xml || strcaseeq(ext, ".xml"))
    { dtd *dtd = new_dtd(no_dtd);

      set_dialect_dtd(dtd, DL_XML);
      p = new_dtd_parser(dtd);
    } else
    { p = new_dtd_parser(new_dtd(no_dtd));
    }
  } else
  { p = new_dtd_parser(new_dtd(no_dtd));
  }

  if (nodefs)
    p->flags |= SGML_PARSER_NODEFS;

  switch (argc)
  { case 1:
    { set_functions(p, output);
      sgml_process_file(p, argv[0], 0);
      free_dtd_parser(p);
      if (output && nerrors == 0)
	printf("C\n");
      return 0;
    }
    case 0:
    { set_functions(p, output);
      set_src_dtd_parser(p, IN_FILE, "stdin");
      set_mode_dtd_parser(p, DM_DATA);
      sgml_process_stream(p, stdin, 0);
      free_dtd_parser(p);
      if (output && nerrors == 0 && nwarnings == 0)
	printf("C\n");
      return 0;
    }
    default:
    { usage();
      return EXIT_FAILURE;
    }
  }
}
