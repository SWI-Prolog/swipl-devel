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


static void
usage()
{ fprintf(stderr, "Usage: html [file.dtd] file\n");
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


static void
set_functions(dtd_parser *p)
{ p->on_end_element = print_close;
  p->on_begin_element = print_open;
  p->on_cdata = print_cdata;
}


int
main(int argc, char **argv)
{ dtd_parser *p = NULL;
  char *ext;

  if ( argc <= 1 )
    usage();

  ext = strchr(argv[1], '.');
  if ( streq(ext, ".dtd") )
  { char doctype[256];
    
    strncpy(doctype, argv[1], ext-argv[1]);
    doctype[ext-argv[1]] = '\0';
      
    p = new_dtd_parser(new_dtd(doctype));
    load_dtd_from_file(p, argv[1]);
    argc--; argv++;
  } else if ( streq(ext, ".html") )
  { p = new_dtd_parser(new_dtd("html"));

    load_dtd_from_file(p, "html.dtd");
  } else
  { p = new_dtd_parser(new_dtd(NULL));
  }

  if ( argc == 2 )
  { set_functions(p);
    sgml_process_file(p, argv[1]);
    free_dtd(p->dtd);
    free_dtd_parser(p);
    return 0;
  }
    
  usage();
  return 1;
}
