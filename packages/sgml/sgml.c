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

#define streq(s1, s2) (strcmp(s1, s2) == 0)

char *program;

static void
usage()
{ fprintf(stderr, "Usage: %s [-xml] [-s] [file.dtd] file\n", program);
  exit(1);
}


static int
print_close(dtd_parser *p, dtd_element *e)
{ ichar name[MAXNMLEN];

  istrcpy(name, e->name->name);
  printf(")%s\n", istrupper(name));

  return TRUE;
}

static int
print_open(dtd_parser *p, dtd_element *e, int argc, sgml_attribute *argv)
{ ichar name[MAXNMLEN];
  int i;

  istrcpy(name, e->name->name);
  printf("(%s\n", istrupper(name));

  for(i=0; i<argc; i++)
  { switch(argv[i].definition->type)
    { case AT_CDATA:
	printf("\t %s=%s\n",
	       argv[i].definition->name->name,
	       argv[i].value.cdata);
	break;
      case AT_NUMBER:
	printf("\t %s=%ld\n",
	       argv[i].definition->name->name,
	       argv[i].value.number);
	break;
      default:
	printf("\t %s=%s\n",
	       argv[i].definition->name->name,
	       argv[i].value.text);
	break;
    }
  }

  return TRUE;
}


static int
print_cdata(dtd_parser *p, int len, const ochar *data)
{ if ( *data != '\n' )
    putchar('-');

  for( ; *data; data++ )
  { if ( *data == '\n' )
    { putchar('\n');
      putchar('-');
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


static void
set_functions(dtd_parser *p)
{ p->on_end_element = print_close;
  p->on_begin_element = print_open;
  p->on_cdata = print_cdata;
  p->on_entity = on_entity;
}

#define shift (argc--, argv++)

int
main(int argc, char **argv)
{ dtd_parser *p = NULL;
  char *s, *ext;
  int xml = FALSE;
  int output = TRUE;

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
    } else
      usage();
  }

  if ( argc == 0 )
    usage();

  ext = strchr(argv[0], '.');
  if ( streq(ext, ".dtd") )
  { char doctype[256];
    
    strncpy(doctype, argv[0], ext-argv[0]);
    doctype[ext-argv[0]] = '\0';
      
    p = new_dtd_parser(new_dtd(doctype));
    load_dtd_from_file(p, argv[0]);
    argc--; argv++;
  } else if ( streq(ext, ".html") )
  { p = new_dtd_parser(new_dtd("html"));

    load_dtd_from_file(p, "html.dtd");
  } else if ( streq(ext, ".xml") )
  { dtd *dtd = new_dtd(NULL);

    set_dialect_dtd(dtd, DL_XML);
    p = new_dtd_parser(dtd);
  } else
  { p = new_dtd_parser(new_dtd(NULL));
  }

  if ( argc == 1 )
  { if ( output )
      set_functions(p);
    sgml_process_file(p, argv[0]);
    free_dtd_parser(p);
    return 0;
  } else
  { usage();
    return 1;
  }
}

