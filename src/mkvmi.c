/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#ifdef __WINDOWS__
#include <direct.h>
#else
#include <unistd.h>
#endif

#define NO_SWIPL 1
#include "pl-hash.c"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This program creates pl-codetable.c, pl-jumptable.ic   and pl-vmi.h from
pl-vmi.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

const char *program;
int verbose = 0;
const char *vmi_file    = "pl-vmi.c";
const char *ctable_file = "pl-codetable.c";
const char *jump_table  = "pl-jumptable.ic";
const char *vmi_hdr	= "pl-vmi.h";

#define MAX_VMI 1000

typedef struct				/* VMI( */
{ char *name;				/* Name */
  char *flags;				/* Flags (VIF_*) */
  char *argc;				/* Argument length (or VM_DYNARGC) */
  char *args;				/* Argument types (max 3) */
} vmi;					/* ) */

vmi vmi_list[MAX_VMI];
int vmi_count = 0;

char *synopsis;
size_t syn_size = 0;
size_t syn_allocated = 0;

static void
add_synopsis(const char *s, size_t len)
{ if ( syn_size+len+1 > syn_allocated )
  { if ( syn_allocated == 0 )
    { syn_allocated = 1024;
      synopsis = malloc(syn_allocated);
      syn_size = 0;
    } else
    { syn_allocated *= 2;
      synopsis = realloc(synopsis, syn_allocated);
    }
  }

  strncpy(&synopsis[syn_size], s, len);
  syn_size+=len;
  synopsis[syn_size++]='&';
}


static char *
skip_ws(const char *s)
{ if ( s )
  { for (; *s; s++)
    { if ( *s != ' ' && *s != '\t' )
	return (char*)s;
    }
  }

  return NULL;
}


static char *
skip_id(const char *s)
{ if ( s )
  { for (; *s; s++)
    { if ( *s == '_' ||
	   (*s >= 'A' && *s <= 'Z') ||
	   (*s >= '0' && *s <= '9') )
	continue;

      return (char*)s;
    }
  }

  return NULL;
}


static char *
skip_flags(const char *s)
{ if ( s )
  { for (; *s; s++)
    { if ( *s == '_' || *s == '|' ||
	   (*s >= 'A' && *s <= 'Z') ||
	   (*s >= '0' && *s <= '9') )
	continue;

      return (char*)s;
    }
  }

  return NULL;
}


static char *
skip_over(const char *s, int c)
{ if ( s )
  { for (; *s; s++)
    { if ( *s == c )
	return (char*)(s+1);
    }
  }

  return NULL;
}


static char *				/* not always around */
my_strndup(const char *in, size_t len)
{ char *s = malloc(len+1);

  strncpy(s, in, len);
  s[len] = '\0';

  return s;
}


static int
load_vmis(const char *file)
{ FILE *fd = fopen(file, "r");

  if ( fd )
  { char buf[1024];
    int line = 0;

    while(fgets(buf, sizeof(buf), fd))
    { line++;

      if ( strncmp(buf, "VMI(", 4) == 0 )
      { const char *s1 = skip_ws(buf+4);
	const char *e1 = skip_id(s1);
	const char *s2 = skip_ws(skip_over(e1, ','));
	const char *e2 = skip_flags(s2);
	const char *s3 = skip_ws(skip_over(e2, ','));
	const char *e3 = skip_flags(s3);
	const char *s4 = skip_over(skip_ws(skip_over(e3, ',')), '(');
	const char *e4 = skip_over(s4, ')');

	if ( !e4 )
	{ fprintf(stderr, "Syntax error at %s:%d\n", file, line);
	  exit(1);
	} else
	  e4--;				/* backspace over ) */

	vmi_list[vmi_count].name  = my_strndup(s1, e1-s1);
	vmi_list[vmi_count].flags = my_strndup(s2, e2-s2);
	vmi_list[vmi_count].argc  = my_strndup(s3, e3-s3);
	vmi_list[vmi_count].args  = my_strndup(s4, e4-s4);

	add_synopsis(s1, e1-s1);	/* flags (s2) isn't needed for VM signature */
	add_synopsis(s3, e3-s3);
	add_synopsis(s4, e4-s4);

	vmi_count++;
      }
    }

    fclose(fd);
    return 0;
  }

  return -1;
}


static int
cmp_file(const char *from, const char *to)
{ FILE *f1 = fopen(from, "r");
  FILE *f2 = fopen(to, "r");
  int rc = -1;

  if ( f1 && f2 )
  { char l1[1024];
    char l2[1024];

    while ( fgets(l1, sizeof(l1), f1) )
    { if ( fgets(l2, sizeof(l2), f2) )
      { if ( strcmp(l1, l2) == 0 )
	  continue;
      }
      goto out;
    }
    if ( !fgets(l2, sizeof(l2), f2) )
      rc = 0;
  }

out:
  if ( f1 ) fclose(f1);
  if ( f2 ) fclose(f2);
  return rc;
}


static int
update_file(const char *from, const char *to)
{ if ( cmp_file(from, to) == 0 )
  { if ( verbose )
      fprintf(stderr, "\t%s: no change\n", to);
    return remove(from);
  } else
  { remove(to);
    if ( verbose )
      fprintf(stderr, "\t%s: updated\n", to);
    return rename(from, to);
  }
}


static char *
mystrlwr(char *to, const char *from)	/* my*: Windows name conflict */
{ char *s;

  for(s=to; *from; from++)
  { *s++ = tolower(*from);
  }
  *s = '\0';

  return to;
}


static int
emit_code_table(const char *to)
{ const char *tmp = "vmi.tmp";
  FILE *out = fopen(tmp, "w");
  int i;

  fprintf(out, "/*  File: %s\n\n", to);
  fprintf(out, "    This file provides a description of the virtual machine instructions\n");
  fprintf(out, "    and their arguments. It  is  used by  pl-comp.c  to facilitate the\n");
  fprintf(out, "    compiler and decompiler, as well as pl-wic.c to save/reload sequences\n");
  fprintf(out, "    of virtual machine qinstructions.\n");
  fprintf(out, "\n");
  fprintf(out, "    Note: this file is generated by %s from %s.  DO NOT EDIT", program, vmi_file);
  fprintf(out, "    \n");
  fprintf(out, "*/\n\n");
  fprintf(out, "#include \"pl-incl.h\"\n\n");
  fprintf(out, "const code_info codeTable[] = {\n");
  fprintf(out, "  /* {name, ID, flags, #args, argtype} */\n");

  for(i=0; i<vmi_count; i++)
  { char name[100];

    fprintf(out, "  {\"%s\", %s, %s, %s, {%s}},\n",
	    mystrlwr(name, vmi_list[i].name),
	    vmi_list[i].name,
	    vmi_list[i].flags,
	    vmi_list[i].argc,
	    vmi_list[i].args[0] ? vmi_list[i].args : "0");
  }

  fprintf(out, "  { NULL, 0, 0, 0, {0} }\n");
  fprintf(out, "};\n");
  fclose(out);

  return update_file(tmp, to);
}


static int
emit_jump_table(const char *to)
{  const char *tmp = "vmi.tmp";
  FILE *out = fopen(tmp, "w");
  int i;

  fprintf(out, "/*  File: %s\n\n", to);
  fprintf(out, "    This file provides the GCC-2 jump-labels to exploit GCC's\n");
  fprintf(out, "    support for threaded code.\n");
  fprintf(out, "\n");
  fprintf(out, "    Note: this file is generated by %s from %s.  DO NOT EDIT", program, vmi_file);
  fprintf(out, "    \n");
  fprintf(out, "*/\n\n");

  fprintf(out, "static void *jmp_table[] =\n");
  fprintf(out, "{\n");


  for(i=0; i<vmi_count; i++)
  { fprintf(out, "  &&%s_LBL,\n", vmi_list[i].name);
  }

  fprintf(out, "  NULL\n");
  fprintf(out, "};\n");

  fclose(out);

  return update_file(tmp, to);
}


static int
emit_code_defs(const char *to)
{  const char *tmp = "vmi.tmp";
  FILE *out = fopen(tmp, "w");
  int i;

  fprintf(out, "/*  File: %s\n\n", to);
  fprintf(out, "    This file provides the definition of type code.\n");
  fprintf(out, "\n");
  fprintf(out, "    Note: this file is generated by %s from %s.  DO NOT EDIT", program, vmi_file);
  fprintf(out, "    \n");
  fprintf(out, "*/\n\n");

  fprintf(out, "typedef enum\n");
  fprintf(out, "{\n");


  for(i=0; i<vmi_count; i++)
  { fprintf(out, "  %s,\n", vmi_list[i].name);
  }

  fprintf(out, "  VMI_END_LIST\n");
  fprintf(out, "} vmi;\n\n");
  fprintf(out, "#define I_HIGHEST ((int)VMI_END_LIST)\n");
  fprintf(out, "#define VM_SIGNATURE 0x%x\n", MurmurHashAligned2(synopsis, syn_size, 0x12345678));

  fclose(out);

  return update_file(tmp, to);
}

int
main(int argc, char **argv)
{ program = argv[0];

  argc--;
  argv++;

  if ( argc >= 1 && strcmp(argv[0], "-v") == 0 )
  { argc--;
    argv++;
    verbose = 1;
  }

  if ( argc == 1 )
  { if ( chdir(argv[0]) )
    { fprintf(stderr, "%s: Could not chdir to %s\n", argv[0], argv[0]);
      exit(1);
    }
  }

  load_vmis(vmi_file);
  if ( verbose )
    fprintf(stderr, "Found %d VMs\n", vmi_count);

  if ( emit_code_table(ctable_file) == 0 &&
       emit_jump_table(jump_table) == 0 &&
       emit_code_defs(vmi_hdr) == 0 )
    return 0;
  else
    return 1;
}
