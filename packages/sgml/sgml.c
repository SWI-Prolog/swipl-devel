/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "dtd.h"
#include "util.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

#define streq(s1, s2) (strcmp(s1, s2) == 0)

char *program;
int nerrors;

static void
usage()
{ fprintf(stderr,
	  "Usage: %s [-xml] [-s] [-nodefs] [file.dtd] [file]\n\n",
	  program);

  fprintf(stderr,
	  "\t-xml\tForce XML mode\n"
	  "\t-s\tSilent: only report errors and warnings\n"
	  "\t-nodefs\tDo not include defaulted attributes\n");

  exit(1);
}


static int
print_close(dtd_parser *p, dtd_element *e)
{ ichar name[MAXNMLEN];

  istrcpy(name, e->name->name);
  printf(")%s\n", istrupper(name));

  return TRUE;
}

typedef struct _atdef
{ attrtype	type;			/* AT_* */
  const char *	name;			/* name */
  int	       islist;			/* list-type */
} atdef;


static atdef attrs[] = 
{ { AT_CDATA,	 "cdata",    FALSE },
  { AT_ENTITY,	 "entity",   FALSE },
  { AT_ENTITIES, "entity",   TRUE },
  { AT_ID,	 "id",	     FALSE },
  { AT_IDREF,	 "idref",    FALSE },
  { AT_IDREFS,	 "idref",    TRUE },
  { AT_NAME,	 "name",     FALSE },
  { AT_NAMES,	 "name",     TRUE },
  { AT_NMTOKEN,	 "nmtoken",  FALSE },
  { AT_NMTOKENS, "nmtoken",  TRUE },
  { AT_NUMBER,	 "number",   FALSE },
  { AT_NUMBERS,	 "number",   TRUE },
  { AT_NUTOKEN,	 "nutoken",  FALSE },
  { AT_NUTOKENS, "nutoken",  TRUE },
  { AT_NOTATION, "notation", FALSE },

  { 0, NULL, FALSE }
};


static atdef *
find_attrdef(attrtype type)
{ atdef *ad = attrs;

  for(; ad->name; ad++)
  { if ( ad->type == type )
      return ad;
  }

  assert(0);
  return NULL;
}


static char *
fixcase(dtd_parser *p, const ichar *s)
{ if ( !p->dtd->case_sensitive )
  { int len = strlen(s)+1;
    ichar *buf = alloca(len);

    istrcpy(buf, s);
    return str2ring((char *)istrupper(buf));
  } else
    return (char *)s;
}


static ichar *
istrncpy(ichar *d, const ichar *s, unsigned len)
{ ichar *r = d;

  while(len-- > 0)
    *d++ = *s++;

  return r;
}


static ichar *
istrblank(const ichar *s)
{ for( ; *s; s++ )
  { if ( isspace(*s) )
      return (ichar *)s;
  }

  return NULL;
}


static char *
nstring(const ichar *s, unsigned len)
{ ichar *buf = alloca(len+1);
  istrncpy(buf, s, len);
  buf[len] = 0;
  return str2ring((char *)buf);
}


static int
print_open(dtd_parser *p, dtd_element *e, int argc, sgml_attribute *argv)
{ int i;

  for(i=0; i<argc; i++)
  { switch(argv[i].definition->type)
    { case AT_CDATA:
	printf("A%s CDATA %s\n",
	       fixcase(p, argv[i].definition->name->name),
	       argv[i].value.cdata);
	break;
      case AT_NUMBER:
	printf("A%s NUMBER ",
	       fixcase(p, argv[i].definition->name->name));

	if ( argv[i].value.text )
	  printf("%s\n", argv[i].value.text);
	else
	  printf("%ld\n", argv[i].value.number);

	break;
      case AT_NAMEOF:
	printf("A%s NAME %s\n",
	       fixcase(p, argv[i].definition->name->name),
	       fixcase(p, argv[i].value.text));
	break;
      default:
      { atdef *ad = find_attrdef(argv[i].definition->type);
	const ichar *val = argv[i].value.text;

	printf("A%s %s",
	       fixcase(p, argv[i].definition->name->name),
	       fixcase(p, ad->name));

        if ( ad->islist )
	{ const ichar *e;

	  for(e=istrblank(val); e; val = e+1, e=istrblank(val))
	  { if ( e == val )
	      continue;			/* skip spaces */
	    printf(" %s", fixcase(p, nstring(val, e-val)));
	  }
          printf(" %s\n", fixcase(p, val));
	} else
	{ printf(" %s\n", fixcase(p, val));
	}
	break;
      }
    }
  }

  printf("(%s\n", fixcase(p, e->name->name));

  return TRUE;
}


static int
print_data(dtd_parser *p, data_type type, int len, const ochar *data)
{ switch(type)
  { case EC_CDATA:
      putchar('-');
      break;
    case EC_NDATA:
      putchar('N');
      break;
    case EC_SDATA:
      putchar('S');
      break;
    default:
      assert(0);
  }

  for( ; *data; data++ )
  { if ( *data == '\n' )
    { putchar('\\');
      putchar('n');
    } else
      putchar(*data);
  }
  putchar('\n');

  return TRUE;
}


static int
on_entity(dtd_parser *p, dtd_entity *e, int chr)
{ if ( e )
  { printf("&%s;", e->name->name);
  } else
    printf("&#%d;", chr);

  return TRUE;
}


static int
on_pi(dtd_parser *p, const ichar *pi)
{ printf("?%s?\n", pi);

  return TRUE;
}


static dtd_srcloc *
file_location(dtd_srcloc *l)
{ while(l->parent && l->type != IN_FILE)
    l = l->parent;

  return l;
}



static int
on_error(dtd_parser *p, dtd_error *error)
{ const char *severity;
  const char *dialect;
  dtd_srcloc *l = file_location(error->location);

  switch(p->dtd->dialect)
  { case DL_SGML:
      dialect = "sgml";
      break;
    case DL_XML:
      dialect = "xml";
      break;
    case DL_XMLNS:
    default:				/* make compiler happy */
      dialect = "xmlns";
      break;
  }

  switch(error->severity)
  { case ERS_WARNING:
      severity = "Warning";
      break;
    case ERS_ERROR:
    default:				/* make compiler happy */
      severity = "Error";
      nerrors++;
      break;
  }

  fprintf(stderr, "%s: (%s mode) %s: %s:%d:%d %s\n",
	  program,
	  dialect,
	  severity,
	  l->name ? l->name : "[]",
	  l->line, l->linepos,
	  error->plain_message);

  return TRUE;
}


static void
set_functions(dtd_parser *p, int output)
{ if ( output )
  { p->on_end_element = print_close;
    p->on_begin_element = print_open;
    p->on_data = print_data;
    p->on_entity = on_entity;
    p->on_pi = on_pi;
  }

  p->on_error = on_error;
}

#define shift (argc--, argv++)

int
main(int argc, char **argv)
{ dtd_parser *p = NULL;
  char *s;
  int xml = FALSE;
  int output = TRUE;
  int nodefs = FALSE;			/* include defaulted attributes */

  if ( (s=strrchr(argv[0], '/')) )
    program = s+1;
  else
    program = argv[0];

  if ( streq(program, "xml") )
    xml = TRUE;

  shift;

  while(argc>0 && argv[0][0] == '-')
  { if ( streq(argv[0], "-xml") )
    { xml = TRUE;
      shift;
    } else if ( streq(argv[0], "-s") )
    { output = FALSE;
      shift;
    } else if ( streq(argv[0], "-nodefs") )
    { nodefs = TRUE;
      shift;
    } else
      usage();
  }

  if ( argc > 0 )
  { char *slash = strchr(argv[0], '/');
    char *dot   = strchr(argv[0], '.');
    char *ext   = (dot == NULL || (slash != 0 && slash > dot)) ? "." : dot;

    if ( istrcaseeq(ext, ".dtd") )
    { char doctype[256];
      
      strncpy(doctype, argv[0], ext-argv[0]);
      doctype[ext-argv[0]] = '\0';
	
      p = new_dtd_parser(new_dtd(doctype));
      load_dtd_from_file(p, argv[0]);
      argc--; argv++;
    } else if ( istrcaseeq(ext, ".html") ||
		istrcaseeq(ext, ".htm" ) )
    { p = new_dtd_parser(new_dtd("html"));
  
      load_dtd_from_file(p, "html.dtd");
    } else if ( xml || istrcaseeq(ext, ".xml") )
    { dtd *dtd = new_dtd(NULL);
  
      set_dialect_dtd(dtd, DL_XML);
      p = new_dtd_parser(dtd);
    } else
    { p = new_dtd_parser(new_dtd(NULL));
    }
  } else
    p = new_dtd_parser(new_dtd(NULL));

  if ( nodefs )
    p->flags |= SGML_PARSER_NODEFS;

  switch(argc)
  { case 1:
    { set_functions(p, output);
      sgml_process_file(p, argv[0]);
      free_dtd_parser(p);
      if ( output && nerrors == 0 )
	printf("C\n");
      return 0;
    }
    case 0:
    { int chr;

      set_functions(p, output);
      set_src_dtd_parser(p, IN_FILE, "stdin");
      set_mode_dtd_parser(p, DM_DATA);
      while( (chr = getchar()) != EOF )
	putchar_dtd_parser(p, chr);

      end_document_dtd_parser(p);
      free_dtd_parser(p);
      if ( output )
	printf("C\n");
      return 0;
    }
    default:
    { usage();
      return 1;
    }
  }
}




