/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

#include "pl-incl.h"
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <sys/stat.h>

#define closeFiles chplCloseFiles
#define warning chplWarning

#ifndef EOS
#define EOS '\0'
#endif
#define MAXLINE 1024
#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

#define ENDHEADER "# End Header\n"

#define VERBOSE(g) if ( verbose ) { g; }

static char *program;
static char *state      = NULL;		/* program to work on */
static char *emulator   = NULL;		/* -e */
static char *outfile    = NULL;		/* -o */
static char *headerfile = NULL;		/* -h */
static int  verbose     = 0;		/* -v */
static int  extract     = 0;		/* -x */

void
warning(char *fm, ...)
{ va_list args;

  va_start(args, fm);
  fprintf(stderr, "%s: ", program);
  vfprintf(stderr, fm, args);
  fprintf(stderr, "\n");
  va_end(args);
}


void
usage()
{ fprintf(stderr, "usage: %s -e emulator [-o outfile] state\n", program);
  fprintf(stderr, "   or: %s -x [-o outfile] state\n", program);
  fprintf(stderr, "   or: %s -h header [-o outfile] state\n", program);

  exit(1);
}


char *
OsError(void)
{
#ifdef HAVE_STRERROR
#ifdef __WIN32__
  return strerror(_xos_errno());
#else
  return strerror(errno);
#endif
#else /*HAVE_STRERROR*/
  static char errmsg[64];

#if unix
  extern int sys_nerr;
#if !EMX
  extern char *sys_errlist[];
#endif
  extern int errno;

  if ( errno < sys_nerr )
    return sys_errlist[errno];
#endif

  sprintf(errmsg, "Unknown Error (%d)", errno);
  return errmsg;
#endif /*HAVE_STRERROR*/
}


int
replacevar(char *line, char *var, char *value)
{ char *s;
  int varl = strlen(var);

  for(s=line; *s; s++)
  { if ( *s == '$' && s[1] == '{' )
    { s += 2;
      if ( strncmp(s, var, varl) == 0 && s[varl] == '-' )
      { char *e;

	s += varl+1;
	if ( (e = strchr(s, '}')) )
	{ char buf[MAXLINE];

	  strcpy(buf, e);
	  strcpy(s, value);
	  strcat(s, buf);
	  return 1;
	}
      }
    }
  }

  return 0;
}


static int
cpmode(char *from, char *to)
{ struct stat buf;

  if ( stat(from, &buf) == 0 )
  { return chmod(to, buf.st_mode);
  }

  return -1;
}


static int
openFiles(char *qlf, char *outfile, char *tmpqlf, FILE **in, FILE **out)
{ strcpy(tmpqlf, ".");
  strcat(tmpqlf, outfile);

  if ( (*in = fopen(qlf, "rb")) == NULL )
  { warning("cannot open %s: %s", qlf, OsError());
    return 1;
  }
  if ( (*out = fopen(tmpqlf, "wb")) == NULL )
  { warning("cannot open %s: %s", tmpqlf, OsError());
    return 1;
  }

  return 0;
}

static int
closeFiles(char *qlf, char *outfile, char *tmpqlf, FILE *in, FILE *out)
{ if ( ferror(in) )
  { warning("Error reading %s: %s\n", qlf, OsError());
    return 1;
  }
  if ( ferror(out) )
  { warning("Error writing %s: %s\n", tmpqlf, OsError());
    return 1;
  }

  fclose(in);
  fclose(out);

  if ( cpmode(qlf, tmpqlf) != 0 )
    warning("failed to copy mode from %s to %s: %s", qlf, tmpqlf, OsError());

  if ( rename(tmpqlf, outfile) )
  { warning("failed to rename %s into %s: %s", tmpqlf, qlf, OsError());
    return 1;
  }

  return 0;
}


static int
copyFd(FILE *in, FILE *out, char *infile, char *outfile)
{ char line[MAXLINE];

  for(;;)
  { char *s;
    int n;

    for(s=line, n=0; n<sizeof(line); n++)
    { int c = fgetc(in);

      if ( c == EOF )
      { char *q = line;

	for( ; q<s; q++)
	{ if ( fputc(*q, out) < 0 )
	  { warning("write failed on %s: %s", outfile, OsError());
	    return 1;
	  }
	}
	return 0;
      } else
	*s++ = c;
    }
    for(s=line, n=0; n<sizeof(line); n++)
    { if ( fputc(*s++, out) < 0 )
      { warning("write failed on %s: %s", outfile, OsError());
	return 1;
      }
    }
  }
}


int
changeEmulator(char *qlf, char *emulator, char *outfile)
{ char tmpqlf[MAXPATHLEN];
  char line[MAXLINE];
  FILE *in, *out;
  int n;

  if ( openFiles(qlf, outfile, tmpqlf, &in, &out) )
    return 1;

  for(n=0; n<100; n++)
  { if ( !fgets(line, sizeof(line), in) )
    { warning("%s: not a SWI-Prolog saved state", qlf);
      return 1;
    }
    if ( replacevar(line, "SWIPL", emulator) )
    { fputs(line, out);
      goto copy;
    }
    fputs(line, out);
  }
  warning("%s: not a SWI-Prolog saved state", qlf);
  return 1;

copy:
  if ( copyFd(in, out, qlf, tmpqlf) )
    return 1;

  return closeFiles(qlf, outfile, tmpqlf, in, out);
}


int
extractHeader(char *qlf, char *outfile)
{ FILE *in, *out;
  char line[MAXLINE];

  if ( (in = fopen(qlf, "rb")) == NULL )
  { warning("cannot open %s: %s", qlf, OsError());
    return 1;
  }
  if ( !outfile )
    out = stdout;
  else
  { if ( (out = fopen(outfile, "wb")) == NULL )
    { warning("cannot open %s: %s", outfile, OsError());
      return 1;
    }
  }

  for(;;)
  { if ( !fgets(line, sizeof(line), in) )
    { warning("%s: Could not find %s", qlf, ENDHEADER);
      return 1;
    }

    if ( strcmp(line, ENDHEADER) == 0 )
      break;

    if ( fputs(line, out) < 0 )
    { warning("write failed: %s\n", OsError());
      return 1;
    }
  }

  fflush(out);
  if ( outfile )
    fclose(out);
  fclose(in);

  return 0;
}


static int
skipHeader(FILE *in, char *file)
{ char line[MAXLINE];

  for(;;)
  { if ( !fgets(line, sizeof(line), in) )
    { warning("%s: Could not find %s", file, ENDHEADER);
      return 1;
    }
    if ( strcmp(line, ENDHEADER) == 0 )
      break;
  }

  return 0;
}


int
replaceHeader(char *qlf, char *headerfile, char *outfile)
{ char tmpqlf[MAXPATHLEN];
  FILE *in, *out, *hf;

  if ( openFiles(qlf, outfile, tmpqlf, &in, &out) )
    return 1;

  if ( strcmp(headerfile, "-") == 0 )
    hf = stdin;
  else if ( (hf = fopen(headerfile, "r")) == NULL )
  { warning("Failed to open %s: %s", headerfile, OsError());
    return 1;
  }
  
  if ( copyFd(hf, out, headerfile, tmpqlf) )
    return 1;
  fprintf(out, "%s", ENDHEADER);
  if ( skipHeader(in, qlf) ||
       copyFd(in, out, qlf, tmpqlf) )
    return 1;

  return closeFiles(qlf, outfile, tmpqlf, in, out);
}


char *
basename(char *path)
{ char *b;

  for(b=path; *path; path++)
  { if ( *path == '/' )
      b = &path[1];
  }

  return b;
}


int
main(int argc, char **argv)
{ program = basename(argv[0]);

  argc--; argv++;
  while(argc > 0 && argv[0][0] == '-')
  { char *opts = &argv[0][1];

    argc--; argv++;
    for(; *opts; opts++)
    { switch(*opts)
      { case 'e':
	  if ( argc <= 0 )
	    usage();
	  emulator = argv[0];
	  argc--; argv++;
	  continue;
	case 'o':
	  if ( argc <= 0 )
	    usage();
	  outfile = argv[0];
	  argc--; argv++;
	  continue;
	case 'x':
	  extract++;
	  continue;
	case 'h':
	  if ( argc <= 0 )
	    usage();
	  headerfile = argv[0];
	  argc--; argv++;
	case 'v':
	  verbose++;
	  continue;
      }
    }
  }

  if ( argc != 1 )
    usage();
  state = argv[0];

  if ( extract )
  { exit(extractHeader(state, outfile));
  }

  if ( !outfile )
    outfile = state;

  if ( headerfile )
  { exit(replaceHeader(state, headerfile, outfile));
  }

  if ( emulator )
  { if ( emulator[0] != '/' )
    { warning("Path to emulator must be absolute");
      exit(1);
    }
    if ( access(emulator, X_OK) != 0 )
      warning("%s: emulator is not executable: %s\n", emulator, OsError());


    exit(changeEmulator(state, emulator, outfile));
  }

  usage();
  return 1;
}


