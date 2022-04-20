/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2018, University of Amsterdam
			      CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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

#ifdef _MSC_VER
#pragma warning(disable : 4996)	/* deprecate open() etc */
#endif

#ifndef PATH_MAX
#define PATH_MAX 1024
#endif

#define NO_SWIPL 1
#include "pl-hash.c"

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This program creates vmi-metadata.h from pl-vmi.c.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

const char *program;
int verbose = 0;
const char *vmi_file    = "pl-vmi.c";
const char *vmi_hdr	= "vmi-metadata.h";

#define MAX_VMI 1000

typedef struct				/* VMI( */
{ char *name;				/* Name */
  char *flags;				/* Flags (VIF_*) */
  char *argc;				/* Argument length (or VM_DYNARGC) */
  char *args;				/* Argument types (max 3) */
  char *argn;				/* VMH ONLY: Argument names */
  int is_vmh;				/* 0 for VMI, 1 for VMH */
} vmi;					/* ) */

vmi vmi_list[MAX_VMI];
int vmi_count = 0;
int vmh_count = 0;

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
skip_id(const char *s, int allow_lower)
{ if ( s )
  { for (; *s; s++)
    { if ( *s == '_' ||
	   (*s >= 'A' && *s <= 'Z') ||
	   (*s >= 'a' && *s <= 'z' && allow_lower) ||
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
    { if ( *s == '_' || *s == '|' || *s == '+' ||
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

  if( len )				/* avoid -Wstringop-truncation */
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

      if ( strncmp(buf, "VMI(", 4) == 0 || strncmp(buf, "VMH(", 4) == 0 )
      { const int is_vmh = buf[2] == 'H';
        const char *s1 = skip_ws(buf+4);
	const char *e1 = skip_id(s1, is_vmh);
	const char *s2 = is_vmh ? NULL : skip_ws(skip_over(e1, ','));
	const char *e2 = is_vmh ? NULL : skip_flags(s2);
	const char *s3 = skip_ws(skip_over(is_vmh ? e1 : e2, ','));
	const char *e3 = skip_flags(s3);
	const char *s4 = skip_over(skip_ws(skip_over(e3, ',')), '(');
	const char *e4 = skip_over(s4, ')');
	const char *s5 = is_vmh ? skip_over(skip_ws(skip_over(e4, ',')), '(') : NULL;
	const char *e5 = is_vmh ? skip_over(s5, ')') : NULL;

	if ( !e4 || (is_vmh && !e5) )
	{ fprintf(stderr, "Syntax error at %s:%d\n", file, line);
	  exit(1);
	} else
	{ e4--;				/* backspace over ) */
	  e5 -= is_vmh;
	}

	vmi_list[vmi_count].name  = my_strndup(s1, e1-s1);
	vmi_list[vmi_count].flags = is_vmh ? NULL : my_strndup(s2, e2-s2);
	vmi_list[vmi_count].argc  = my_strndup(s3, e3-s3);
	vmi_list[vmi_count].args  = my_strndup(s4, e4-s4);
	vmi_list[vmi_count].argn  = is_vmh ? my_strndup(s5, e5-s5) : NULL;
	vmi_list[vmi_count].is_vmh = is_vmh;

	if (!is_vmh)			/* VMH pseudo-instructions aren't part of signature */
	{ add_synopsis(s1, e1-s1);	/* flags (s2) isn't needed for VM signature */
	  add_synopsis(s3, e3-s3);
	  add_synopsis(s4, e4-s4);
	}

	vmi_count++;
	vmh_count += is_vmh;
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
emit_vmi_hdr(const char *to)
{ const char *tmp = "vmi.tmp";
  FILE *out = fopen(tmp, "w");
  int i;

  fprintf(out, "/*  File: %s\n\n", to);
  fprintf(out, "    This file provides a set of defines describing the virtual machine\n");
  fprintf(out, "    instructions and their arguments. These are used by other source files\n");
  fprintf(out, "    to define various attributes and structures. See pl-vmi.h for more details.\n");
  fprintf(out, "\n");
  fprintf(out, "    Note: this file is generated by %s from %s.  DO NOT EDIT", program, vmi_file);
  fprintf(out, "    \n");
  fprintf(out, "*/\n\n");

  fprintf(out, "#ifndef _IN_PL_VMI_H\n");
  fprintf(out, "#error \"Do not include <%s> directly! Include \\\"pl-vmi.h\\\"\"\n", to);
  fprintf(out, "#endif\n\n");

  fprintf(out, "#define VM_SIGNATURE 0x%x\n", MurmurHashAligned2(synopsis, syn_size, 0x12345678));
#define PRINT_LIST(macro, condition) do \
  { int written = 0; \
    fprintf(out, "#define " #macro "(sep,f,...)"); \
    for(i=0; i<vmi_count; i++) \
    { if (condition) \
        fprintf(out, "%s f(%s, ## __VA_ARGS__)", written++ > 0 ? " sep()" : "", vmi_list[i].name); \
    } \
    fprintf(out, "\n"); \
  } while(0)
  PRINT_LIST(FOREACH_VMI_CALL, !vmi_list[i].is_vmh);
  PRINT_LIST(FOREACH_VMH_CALL, vmi_list[i].is_vmh);

  fprintf(out, "/* Instruction data */\n\n");

  for(i=0; i<vmi_count; i++)
  { char name[100];
    vmi *Vmi = vmi_list + i;
    fprintf(out, "#define VMLCASE_%s     %s\n", Vmi->name, mystrlwr(name, Vmi->name));

    if (Vmi->is_vmh)
    { fprintf(out, "#define VMHDECL_%s     %s, %s, (%s), (%s)\n", Vmi->name, Vmi->name, Vmi->argc, Vmi->args, Vmi->argn);
      fprintf(out, "#define VMHARGNAMES_%s %s\n", Vmi->name, Vmi->argn);
    }
    else
    { fprintf(out, "#define VMIDECL_%s     %s, %s, %s, (%s)\n", Vmi->name, Vmi->name, Vmi->flags, Vmi->argc, Vmi->args);
      fprintf(out, "#define VMIFLAGS_%s    %s\n", Vmi->name, Vmi->flags);
    }
    
    fprintf(out, "#define VM%cARGCOUNT_%s %s\n", Vmi->is_vmh ? 'H' : 'I', Vmi->name, Vmi->argc);
    fprintf(out, "#define VM%cARGTYPES_%s %s\n\n", Vmi->is_vmh ? 'H' : 'I', Vmi->name, Vmi->args);
  }

  fclose(out);

  return update_file(tmp, to);
}

int
main(int argc, char **argv)
{ program = argv[0];
  char buf[PATH_MAX];

  argc--;
  argv++;

  if ( argc >= 1 && strcmp(argv[0], "-v") == 0 )
  { argc--;
    argv++;
    verbose = 1;
  }

  if ( argc >= 1 )
    snprintf(buf, sizeof(buf), "%s/%s", argv[0], vmi_file);
  else
    snprintf(buf, sizeof(buf), "%s", vmi_file);


  load_vmis(buf);
  if ( verbose )
    fprintf(stderr, "Found %d VMs and %d helpers\n", vmi_count - vmh_count, vmh_count);

  if ( emit_vmi_hdr(vmi_hdr) == 0 )
    return 0;
  else
    return 1;
}
